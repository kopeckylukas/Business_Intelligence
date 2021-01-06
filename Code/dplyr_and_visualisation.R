
############################### QUESTION 1 #####################################

##### TASK 1 : ISPECT WHAT YOU HAVE LOADED ######

#Create table Cars
dbSendQuery(con, "
  CREATE TABLE Cars (
  ID INT PRIMARY KEY,
  mpg FLOAT,
  cylinders FLOAT,
  displacement FLOAT,
  horsepower FLOAT, 
  weight FLOAT,
  acceleration FLOAT,
  model FLOAT,
  origin FLOAT,
  car_name VARCHAR(250),
  price FLOAT);")

#Load Data from CSV File
cars_info <- read.csv("C:/Windows/Temp/cars_info.csv")  

#Write data into database
dbWriteTable(con, name='Cars', value=cars_info, overwrite=TRUE)

#Displays names of the columns in the table Car
dbListFields(con, 'Cars')

#Display all Data
result <- dbSendQuery(con, 'SELECT * From Cars')
data <- fetch(result, n=-1)
data

########### TASK 2 : RMySQL QUERIES #####################
#1 Get the first 10 rows in the imported table
result <- dbSendQuery(con, 'SELECT * From Cars WHERE ID < 11')
data <- fetch(result, n=-1)
data

#2 Get all eight-cylinder cars with miles per gallon greater than 18
result <- dbSendQuery(con, 'SELECT * From Cars WHERE cylinders = 8 AND
                       mpg > 18')
data <- fetch(result, n=-1)
data

#3 Get the average horsepower and mpg by number of cylinder groups
result <- dbSendQuery(con, 'SELECT cylinders, AVG(horsepower), AVG(mpg) From Cars Group by cylinders')
data <- fetch(result, n=-1)
data

#4 Get all cars with less than eight-cylinder and with acceleration from 11 to 13 (inclusive both limits)
result <- dbSendQuery(con, 'SELECT * From Cars WHERE cylinders < 8 AND (acceleration >= 11 and acceleration <= 13)')
data <- fetch(result, n=-1)
data

#5 Get the car names and horsepower of the cars with 3 cylinders
result <- dbSendQuery(con, 'SELECT car_name, horsepower From Cars WHERE cylinders = 3 ')
data <- fetch(result, n=-1)
data


########### TASK 2 dplyr QUERIES #####################
#saves table car to data structure cars_db
cars_db <- tbl(con, "Cars")

#set minimum of printed tibble rows to 100 
options( tibble.print_min = 100)

#1 Get the first 10 rows in the imported table
cars_db %>% filter(ID < 11) 

#2 Get all eight-cylinder cars with miles per gallon greater than 18
cars_db %>% filter(cylinders == 8, mpg > 18) 

#3 Get the average horsepower and mpg by number of cylinder groups
cars_db %>% group_by(cylinders) %>% summarise(avg(mpg), avg(horsepower))

#4 Get all cars with less than eight-cylinder and with acceleration from 11 to 13 (inclusive both limits)
cars_db %>% filter(cylinders < 8, acceleration <= 13, acceleration >=11 ) 

#5 Get the car names and horsepower of the cars with 3 cylinders
cars_db %>% filter(cylinders == 3) %>% select(car_name, horsepower) 


##################### TASK 3 : VISUALISATION ##########################
#saves table car to data structure cars_db
cars_db <- tbl(con, "Cars")



####### 1.1 distribution of values for cylinders
#retrieve data frame of cylinders
cylinders<-cars_db %>% select(cylinders) %>% data.frame
hist(cylinders$cylinders, main = "Histogram of Cylinders", 
     xlab = "Number of cylinders", col='light blue')

#Display table showing number of models by cylinder
cars_db %>% group_by(cylinders) %>% summarise(count(cylinders))

#average per year
aCYL <- cars_db %>% group_by(model) %>% summarise(avg(cylinders)) %>% data.frame

####### 1.2 distribution of values for Mpg 

mpg<-cars_db %>% select(mpg) %>% data.frame
hist(mpg$mpg, main = "Histogram of Miles per gallon", 
     xlab = "Number of Miles per gallon", col='light blue')

#cars with higher fuel consumption than 10mpg
cars_db %>% filter(mpg <= 10)

#average per year
aMPG <- cars_db %>% group_by(model) %>% summarise(avg(mpg)) %>% data.frame

#Relationship between 
CYLMPG <- merge( aCYL,aMPG,by="model")
scatter.smooth( CYLMPG$avg.cylinders., CYLMPG$avg.mpg., xlab= "Average cylinders",
                ylab = " Average Mpg") 
abline(lm(CYLMPG$avg.mpg. ~ CYLMPG$avg.cylinders.), col="red")



####### 2 boxplot to show the mean and distribution of Mpg measurements for each year
cars_db <- tbl(con, "Cars")

#select value for each year
y70<-cars_db %>% filter(model == 70) %>% select(mpg) %>% data.frame
y71<-cars_db %>% filter(model == 71) %>% select(mpg) %>% data.frame
y72<-cars_db %>% filter(model == 72) %>% select(mpg) %>% data.frame
y73<-cars_db %>% filter(model == 73) %>% select(mpg) %>% data.frame
y74<-cars_db %>% filter(model == 74) %>% select(mpg) %>% data.frame
y75<-cars_db %>% filter(model == 75) %>% select(mpg) %>% data.frame
y76<-cars_db %>% filter(model == 76) %>% select(mpg) %>% data.frame
y77<-cars_db %>% filter(model == 77) %>% select(mpg) %>% data.frame
y78<-cars_db %>% filter(model == 78) %>% select(mpg) %>% data.frame
y79<-cars_db %>% filter(model == 79) %>% select(mpg) %>% data.frame
y80<-cars_db %>% filter(model == 80) %>% select(mpg) %>% data.frame
y81<-cars_db %>% filter(model == 81) %>% select(mpg) %>% data.frame
y82<-cars_db %>% filter(model == 82) %>% select(mpg) %>% data.frame

#display boxplot
boxplot(y70$mpg,y71$mpg,y72$mpg,y73$mpg,y74$mpg,y75$mpg,y76$mpg,y77$mpg,y78$mpg,y79$mpg,
        y80$mpg,y81$mpg,y82$mpg,
        xlab = "Mean and distribution of mpg for each yaer",
        ylab = "Miles per gallon",
        names=c(1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982),
        col="light blue")

####### 3 scatter plot showing the relationship between weight and Mpg
cars_db <- tbl(con, "Cars")


cars_table <- dbReadTable(con, "Cars")
scatter.smooth( cars_table$weight, cars_table$mpg, xlab= "Weight (Lbs.)",
                ylab = "Consuption Mpg", col="blue") 
abline(lm(cars_table$mpg ~cars_table$weight ), col="red")


