########################## GENERATE DATA #######################################
#Create State Table
state_table <-
  data.frame(key=c("FR", "LA", "SY", "SE", "CT"),
             name=c("Frankfurt", "Los Angeles", "Sydney", "Seoul", "Cape Town"),
             country=c("Germany", "USA", "Australia", "S. Korea", "South Africa"))

# Create Month Table
month_table <-
  data.frame(key=1:12,
             desc=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
             quarter=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))

# Create Product Table
prod_table <-
  data.frame(key=c("Washing Machine", "Fridge", "Vacuum Cleaner", "Microwave Oven"),
             price=c(500, 150, 400, 200), #Values are shifted (see reprot)
             stringsAsFactors = TRUE)


#########Generate randomly 500 samples within 6 years 
# Define funcition
gen_sales <- function(no_of_recs) {
  # Generate transaction data randomly
  loc <- sample(state_table$key, no_of_recs, replace=T, prob=c(2,2,1,1,1))
  time_month <- sample(month_table$key, no_of_recs, replace=T)
  time_year <- sample(c(2015, 2016, 2017, 2018, 2019, 2020), no_of_recs, replace=T)
  prod <- sample(prod_table$key, no_of_recs, replace=T)
  unit <- sample(c(1,2), no_of_recs, replace=T, prob=c(10, 4))
  amount <- unit*prod_table[prod,]$price
  
  sales <- data.frame(month=time_month,
                      year=time_year,
                      loc=loc,
                      prod=prod,
                      unit=unit,
                      amount=amount)
  # Sort the records by time order
  sales <- sales[order(sales$year, sales$month),]
  row.names(sales) <- NULL
  return(sales)
}

#Generate Samples
sales_fact <- gen_sales(500)

head(sales_fact)


##### Save sales_fact to Database 
dbWriteTable(con, name='Sales_CWK2', value=sales_fact, overwrite=TRUE)

####################### LOAD FROM DATABASE ####################################

salesDB <- tbl(con, 'Sales_CWK2')


sales_table <- salesDB %>% select(month, year, loc, prod, unit, amount) %>% data.frame


##### Create Revenue Cube ####
revenue_cube <-
  tapply(sales_table$amount,
         sales_table[,c("prod", "month", "year", "loc")],
         FUN=function(x){return(sum(x))})


# Display Revenue Cube
revenue_cube

# Display dimension names
dimnames(revenue_cube)


######################## OLAP OPERATIONS #######################################

#######Slice
  revenue_cube[, "1", "2016",]

  revenue_cube["Fridge", "1", "2017",]

#######Dice

  revenue_cube[c("Washing Machine", "Fridge"),
             c("1","2",'5'), ,
             c("SY","LA")]

    revenue_cube[c("Washing Machine", "Vacuum Cleaner"),
               c("1","2",'12'), ,
               c("SE","LA", "CT")]

#######Roll-up
apply(revenue_cube, c("year", "prod"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})


apply(revenue_cube, c("year", "prod"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})



#######Drill-down
apply(revenue_cube, c("year", "month", "prod"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})





apply(revenue_cube, c("year",  "month", "loc"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

#########Pivot
apply(revenue_cube, c("year", "month"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})






