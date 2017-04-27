############################## Install Packages 
install.packages("Quandl")
install.packages('parallel')
install.packages('foreach')
install.packages('doParallel')

library(foreach)
library(doParallel)
library(parallel)
library(Quandl)

############################## Load Data 
gold <- Quandl("LBMA/GOLD")

############################### Visualize the data
head(gold)
nrow(gold)
class(gold)

############################### Shaping the Data
gold$"EURO (PM)" <- NULL
gold$"EURO (AM)" <- NULL
gold$"GBP (PM)" <- NULL
gold$"GBP (AM)" <- NULL
gold$"USD (AM)" <- NULL

############################## Creating a Subset of the data
gold2 <- gold[1:248,]

############################# Calculating daily Drift 
line <- lm(log(gold2$`USD (PM)`) ~ gold2$Date, data=gold2)
drift_daily <- 0.0002069 

############################# Creating log returns column

log_returns <- c()

for (i in 2:length(gold2$`USD (PM)`)){
  x <- gold2$`USD (PM)`[i]/gold2$`USD (PM)`[i-1]
  y<- log(x)
  log_returns <- c(log_returns,y)
}

gold2$log_return <- c("NA",log_returns)

gold2$log_return <- as.numeric(gold2$log_return)

############################ Calculating Daily Volatility
vol_daily <- sd(gold2$log_return, na.rm=TRUE)

############################ Calculating Drift Mean 
drift_mean <- drift_daily-0.5*(vol_daily^2)

############################ Calculating Predicted log returns for the next 15 days
z <- rnorm(15,mean=0,sd=1)
log_ret <- drift_mean+z*vol_daily

############################ Non-Parallelized Algorithm 
non_parallel_start<-Sys.time()
table <- data.frame(row.names=1:15)

for (i in 1:66000){
  z <- rnorm(15,mean=0,sd=1)
  log_ret <- drift_mean+z*vol_daily
  price_pred <- c()
  price_i <- 1133.65
  for (j in 1:15){
    price_i = price_i*exp(log_ret[j])
    price_pred <- c(price_pred,price_i)
  }
  table <- cbind(table,data.frame(price_pred))
}

table$average <- apply(table,1,mean)

plot(table$average)
non_parallel_end<-Sys.time()
non_parallel_end-non_parallel_start
################################### Parallelized Algorithm

no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
registerDoParallel(cl)
parallel_start<-Sys.time()
table <- data.frame(row.names=1:15)

result <- foreach(i = 1:3) %dopar% {
  for (i in 1:100000){
    z <- rnorm(15,mean=0,sd=1)
    log_ret <- drift_mean+z*vol_daily
    price_pred <- c()
    price_i <- 1185.35
    for (j in 1:15){
      price_i = price_i*exp(log_ret[j])
      price_pred <- c(price_pred,price_i)
    }
    table <- cbind(table,data.frame(price_pred))
  }
  table$average <- apply(table,1,mean)
}


table2 <- data.frame(row.names=1:15)

table2 <- cbind(table2,data.frame(result[1]))
table2 <- cbind(table2,data.frame(result[2]))
table2 <- cbind(table2,data.frame(result[3]))

table2$final <- apply(table2,1,mean)

plot(table2$final)
parallel_end<-Sys.time()
parallel_end-parallel_start
stopCluster(cl)


