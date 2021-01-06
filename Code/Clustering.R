######## LOAD DATA #########

#Load Data
library(readxl)
vehicles <- read_excel("~/Desktop/BI/vehicles.xlsx")

#Create Working set
vehicles.raw <- vehicles[2:20]


#view Structure
str(vehicles.raw)
head(vehicles.raw)
summary(vehicles.raw)

classes.name <- vehicles$Class
set.seed(20)

######### Data Preparation ########

#Look for outliers
boxplot(vehicles.raw[1:18], plot=TRUE)

#Outlier detected at columns index: 4, 5, 6, 11, 12, 14, 15, 16

#Retrieve list of Outliers
col4 <- which(vehicles.raw$Rad.Ra %in% boxplot(vehicles.raw[4], plot=FALSE)$out)
col5 <- which(vehicles.raw$Pr.Axis.Ra %in% boxplot(vehicles.raw[5], plot=FALSE)$out)
col6 <- which(vehicles.raw$Max.L.Ra %in% boxplot(vehicles.raw[6], plot=FALSE)$out)
col11 <- which(vehicles.raw$Sc.Var.Maxis %in% boxplot(vehicles.raw[11], plot=FALSE)$out)
col12 <- which(vehicles.raw$Sc.Var.maxis %in% boxplot(vehicles.raw[12], plot=FALSE)$out)
col14 <- which(vehicles.raw$Skew.Maxis %in% boxplot(vehicles.raw[14], plot=FALSE)$out)
col15 <- which(vehicles.raw$Skew.maxis %in% boxplot(vehicles.raw[15], plot=FALSE)$out)
col16 <- which(vehicles.raw$Kurt.maxis %in% boxplot(vehicles.raw[16], plot=FALSE)$out)

#append all columns together
allColumns <- append(col4, c(col5,col6,col11,col12,col14,col15,col16))

#check for unique otliers 
outliers <- unique(allColumns) # = 33 Unique Outliers

#delete outlires
vehicles.clear <- vehicles.raw[-outliers,]

#plot clear dataset
boxplot(vehicles.clear[1:18], plot=TRUE)

#copy Class names for evaluation
classes.name <- vehicles.clear$Class

#delete class column!!!!!!!!!!!!!!!! it is important to do this step after removing
#outliers, so the last colum will be the same length as the dataset
vehicles.clear <- vehicles.clear[1:18]


set.seed(20)
######## SCALE DATA #########
vehicles.scaled <- scale(vehicles.clear)   
summary(vehicles.scaled)

normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


vehicles.norm <- as.data.frame(lapply(vehicles.clear, normalise))

######### NB CLUST ##########
library("NbClust")

set.seed(26)
clusterNo=NbClust(vehicles.scaled,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")
clusterNo=NbClust(vehicles.scaled,distance="manhattan", min.nc=2,max.nc=10,method="kmeans",index="all")
clusterNo=NbClust(vehicles.scaled,distance="maximum", min.nc=2,max.nc=10,method="kmeans",index="all")

table(clusterNo$Best.n[1,])



barplot(table(clusterNo$Best.n[1,]),  
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 30 Criteria")

set.seed(26)
wss <- 0
for (i in 1:15){
  wss[i] <- sum(kmeans(vehicles.scaled, centers=i)$withinss)
}

plot(1:15,
     wss,
     type="b",    
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")




################### APPLY K-MEANS ###################

set.seed(20)

set.seed(20)
fit.km <- kmeans(vehicles.scaled, 2, nstart=20)

library(amap)
set.seed(20)
#fit.km <- Kmeans(vehicles.scaled, 3, iter.max = 30, nstart = 10,
#       method = "maximum")

#fit.km <- Kmeans(vehicles.scaled, 2, iter.max = 30, nstart = 10,method = "manhattan")

fit.km

fit.km$centers
fit.km$size


library(fpc)
plotcluster(vehicles.scaled, fit.km$cluster)


#Evaluation
confuseTable.km <- table(classes.name, fit.km$cluster)
confuseTable.km

table(classes.name,fit.km$cluster)  
table(fit.km$cluster,classes.name)  

