library(dplyr)
library(RMySQL)
library(ggplot2)
library(tidyr)
library(caret)
library(C50)
library(rpart.plot)
library(MLmetrics)
library(pROC)
library(adabag)
library(maptree)

########## Database Connection #############
con <- DBI::dbConnect(RMySQL::MySQL(), 
                      host = "0.0.0.0",
                      user = "username",
                      dbname="dynamo",
                      port=2222,
                      password = "dbpswd")
dbListTables(con)s
