################# IMPORT DATASET ##############

#Load data from a file
liver_file = read.csv("C:/Users/w1701833/Desktop/Business Intelligence/liver.csv")

#Upload data to database
dbWriteTable(con, name='Liver', value=liver_file)

#Display table names
dbListTables(con)

#Display Fileds in Liver table
dbListFields(con, 'Liver')

################# LOAD DATA FROM DATABASE ########
liver_table <- tbl(con, "Liver") %>% data.frame

head(liver_table)

################# DATA PREPARATION ################

#Drop all Observations with NULL value
liver_filtered <- liver_table %>% drop_na()


# Replace gender with numerical values
liver_filtered[liver_filtered == "Male"] <-1
liver_filtered[liver_filtered == "Female"] <-2

anyNA(liver_filtered)
# [1] FALSE

#Save working data set into database#
dbWriteTable(con, name='Liver_worksample', value=liver_filtered[-1], overwrite=TRUE)

################# DATA ANALYTICS ###################
liverDB <- tbl(con, 'Liver_worksample')

#Mean(avg) age of subjects
liverDB %>% group_by(gender) %>% summarise(avg(age))  

#Median  and quartiles of subject ages
female_age <-liverDB %>% select(age, gender) %>% filter(gender == 2) %>% data.frame
male_age <-liverDB %>% select(age, gender) %>% filter(gender == 1) %>% data.frame

female_Q <- quantile(female_age$age, probs=c(.25, .5, .75), na.rm = FALSE)
male_Q <- quantile(male_age$age, probs=c(.25, .5, .75), na.rm = FALSE)

#Display Values
female_Q
male_Q

################# DATA VISUALISATION #############
liverDB <- tbl(con, 'Liver_worksample')

###Perform a histogram of the frequency of patients per age
  #check number of groups  
  liverDB %>% group_by(age) %>% summarise(count(age))%>% data.frame
  #extract age data
  agehist<-liverDB %>% select(age) %>% data.frame

  #display histogram (step 1)
  hist(agehist$age, breaks = seq(1,100, by=1), xlim = c(0,100), 
       col="light blue", xlab="Age (Step by 1)", main = "Histogram of patients age (step by 1)")
  
  #display histogram (step 3)
  hist(agehist$age, breaks = seq(1,100, by=3), xlim = c(0,100),  ylim = c(0,50),
       col="light blue", xlab="Age (Step by 3)", main = "Histogram of patients age (step by 3)")
  
  #display ggplot histogram
  ggplot(agehist, aes(age)) + geom_histogram(binwidth=3, fill="light blue", color="black") 


###Perform a histogram of the frequency of patients per sgpt
  #check number of groups
  liverDB %>% group_by(sgpt) %>% summarise(count(sgpt))%>% data.frame
  
  #extract data
  sgpthist<-liverDB %>% select(sgpt) %>% data.frame 
  
  #display histogram
    hist(sgpthist$sgpt, breaks = seq(1,10, by=.1), xlim = c(2,10), ylim = c(0,35),
       col="light blue", xlab="Alamine Aminotransferase (Step by 0.1)", 
       main = "Histogram of Alamine Aminotransferase (step by 0.1)")
  
  hist(sgpthist$sgpt, breaks = seq(1,10, by=.2), xlim = c(2,10), ylim = c(0,60),
       col="light blue", xlab="Alamine Aminotransferase (Step by 0.2)",
       main = "Histogram of Alamine Aminotransferase (step by 0.2)")
  
  #ggplot histogram
    ggplot(sgpthist, aes(sgpt)) + geom_histogram(binwidth=.1, fill="light blue", color="black")


###Perform a boxplot of Gender (Male & Female) vs age
  boxplot(female_age$age, male_age$age, names=c("Female","Male"), 
          col = "light blue", horizontal = TRUE)


################# GENERATE TRAINING AND TESTING DATASET ########################
  #table from database
  liverDB <- tbl(con, 'Liver_worksample')
  
  #save to dataframe
  liver_ <- liverDB %>% data.frame
  
  #Slice data for testing and training sample
  set.seed(3003)
  intrain <- createDataPartition(y = liver_$is_patient, p= 0.8, list = FALSE)
  training <- liver_[intrain,]
  testing <- liver_[-intrain,]

  #save training and testing samples to the database
  dbWriteTable(con, name='Liver_training', value=training[-1], overwrite=TRUE)
  dbWriteTable(con, name='Liver_testing', value=testing[-1], overwrite=TRUE)

  dim(training) #464
  dim(testing)  #115
  
################# C5.0 MODEL DECISION TREE #####################################
  
  #Load data
  liver_train <- tbl(con, 'Liver_training')
  liver_test <- tbl(con, 'Liver_testing')
  
  
  training_set <- liver_train %>% data.frame
  training_set <- training_set[-1] #%>% data.frame
  
  testing_set <- liver_test %>% 
    select(age,gender,tot_bilirubin,direct_bilirubin,tot_proteins,albumin,ag_ratio,sgpt,sgot,alkphos,is_patient) %>% 
    data.frame
  
  
  #Convert data to factor
  training_set$is_patient <- as.factor(training_set$is_patient)
  
  #set.seed(2345)
  model <- C5.0(is_patient ~., data=training_set )
  
  plot(model)
  
  
  #test treee
  results <- predict(object=model, newdata=testing_set, type="class")
  
  #evaluation
  #table(results, testing_set$is_patient)
  tt <- as.factor(testing_set$is_patient)
  confusionMatrix(results, tt)
  
  #F1 Score
  F1_Score(tt, results)
  
  #ROC
  C5ROC <- roc(results, testing_set$is_patient, plot = TRUE)
  
  auc(C5ROC)
  
  ################# CART MODEL DECISION TREE ####################################
  
  ###Load data
  liver_train <- tbl(con, 'Liver_training')
  liver_test <- tbl(con, 'Liver_testing')
  
  
  training_set <- liver_train %>% data.frame
  training_set <- training_set[-1] #%>% data.frame
  
  testing_set <- liver_test %>% 
    select(age,gender,tot_bilirubin,direct_bilirubin,tot_proteins,albumin,ag_ratio,sgpt,sgot,alkphos,is_patient) %>% 
    data.frame 
  
  # Train tree
  set.seed(3333)
  model <- rpart(is_patient~.,data=training_set,method="class")
  model
  
  plot(model)
  text(model, digits = 3)
 
  # test tree
  predicted.classes <- model %>% predict(testing_set, type = "class") 
  head(predicted.classes)
  
  mean(predicted.classes == testing_set$is_patient)


  #evaluate tree
  tt <- as.factor(testing_set$is_patient)
  confusionMatrix(predicted.classes, tt)

  #F1 Score
  F1_Score(predicted.classes, tt)
  
  #ROC
  cartROC <- roc(predicted.classes, testing_set$is_patient, plot = TRUE)
  
  auc(cartROC)
  
################# PRUNING AND IMPROVEMENT  ################################  
  
  ###Load data
  liver_train <- tbl(con, 'Liver_training')
  liver_test <- tbl(con, 'Liver_testing')
  
  
  training_set <- liver_train %>% data.frame
  training_set <- training_set[-1] #%>% data.frame
  
  testing_set <- liver_test %>% 
    select(age,gender,tot_bilirubin,direct_bilirubin,tot_proteins,albumin,ag_ratio,sgpt,sgot,alkphos,is_patient) %>% 
    data.frame 
  
  # Train tree
  set.seed(3333)
  
  rctr <- rpart.control(maxdepth = 10 , minsplit = 10)
  model <- rpart(is_patient~.,data=training_set,method="class", control = rctr)
  
  
  plot(model)
  text(model, digits = 3)
  
  # test tree
  predicted.classes <- model %>% predict(testing_set, type = "class") 
  head(predicted.classes)
  
  mean(predicted.classes == testing_set$is_patient)
  
  
  #evaluate tree
  tt <- as.factor(testing_set$is_patient)
  confusionMatrix(predicted.classes, tt)
  
  #F1 Score
  F1_Score(predicted.classes, tt)
  
     #ROC
  cartROC <- roc(predicted.classes, testing_set$is_patient, plot = TRUE)
  
  auc(cartROC)
  
 