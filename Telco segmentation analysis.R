
setwd("C:\\Users\\PHANI KUMAR\\Desktop\\Final segmentation cases")

mydata <- read.csv("Telco Segmentation Data.csv")

#Loading required packages
library(caret)
library(GPArotation)
library(tables)
library(psych)
library(cluster)
library(dplyr)

str(mydata)
summary(mydata)

#################################Data Preparation################################

#Creating dummyvariable for gender as it is a categorical variable

dv=dummyVars(~GENDER,data = mydata)

dummy_gender <- data.frame(predict(dv,mydata))

mydata <- cbind(mydata,dummy_gender)

#Removing not necessary variables
mydata$CUST_ID <- NULL
mydata$GENDER <- NULL
mydata$GENDER.F <- NULL

#user defined function to get the descriptive statistics
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

#Applying the above function on the data
descriptive_stats <- sapply(mydata,FUN = mystats)
write.csv(descriptive_stats,"Descriptive stats.csv")

#counting missing values
colSums(is.na(mydata))

#There are no missing values so we can proceed with outlier treatment

#Outlier treatment(User defined function)
#UC = 0.99
#LC = 0.1

outlier_treat <- function(x){
  UC1 = quantile(x, p=0.99,na.rm=T)
  LC1 = quantile(x, p=0.01,na.rm=T)
  x=ifelse(x>UC1, UC1, x)
  x=ifelse(x<LC1, LC1, x)
  return(x)
  
}

mydata <- data.frame(sapply(mydata,FUN = outlier_treat))
mydata$GENDER <- gender
input_data <- mydata

#Correlation matrix among the vars
corr_matrix <- cor(input_data,method = "pearson")

#Eigen values
eigen_values <- mutate(data.frame(eigen(corr_matrix)$values)
                       ,cum_sum_eigen=cumsum(eigen.corr_matrix..values)
                       , pct_var=eigen.corr_matrix..values/sum(eigen.corr_matrix..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corr_matrix..values))                                                   

write.csv(eigen_values,"Eigen values.csv")

scree(corr_matrix,factors = TRUE,pc = TRUE,main = "Scree plot",hline = NULL,add = FALSE)

#Based on scree plot, 6 components should be used 
FA <- fa(r = corr_matrix,6,rotate = "varimax",fm = "ml")
FA_SORT <- fa.sort(FA)                         

Loadings1 <- data.frame(FA_SORT$loadings[1:ncol(mydata),])

write.csv(Loadings1,"loadings.csv")

#Vars selected in factor analysis

cluster_vars <- c("SMS_OUT_CALLS",
                  "OUT_COMMUNITY_SMS",
                  "OUT_CALLS_ROAMING",
                  "VOICE_OUT_CALLS",
                  "OUT_CALLS_PEAK",
                  "TOTAL_OUT_CALLS",
                  "OUT_MINS_INTERNATIONAL",
                  "IN_CALLS_PEAK",
                  "VOICE_IN_CALLS",
                  "IN_COMMUNITY_TOTAL")
inputdata_cluster <- mydata[cluster_vars]

#scaling the selected vars
inputdata_final <- scale(inputdata_cluster)

#building clusters using k-means clustering
cluster_three <- kmeans(inputdata_final,3)
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
cluster_six <- kmeans(inputdata_final,6)
cluster_seven <- kmeans(inputdata_final,7)

telco_clust <- data.frame(cbind(mydata,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,
                                km_clust_5=cluster_five$cluster,km_clust_6=cluster_six$cluster,
                                km_clust_7=cluster_seven$cluster))

#Converting the cluster variables to factor variables
telco_clust$km_clust_3 <- as.factor(telco_clust$km_clust_3)
telco_clust$km_clust_4 <- as.factor(telco_clust$km_clust_4)
telco_clust$km_clust_5 <- as.factor(telco_clust$km_clust_5)
telco_clust$km_clust_6 <- as.factor(telco_clust$km_clust_6)
telco_clust$km_clust_7 <- as.factor(telco_clust$km_clust_7)

profile <- tabular(1+OUT_COMMUNITY_TOTAL+OUT_COMMUNITY_VOICE+OUT_COMMUNITY_SMS+IN_COMMUNITY_VOICE+IN_COMMUNITY_SMS+
                 IN_COMMUNITY_TOTAL+VOICE_OUT_CALLS+VOICE_IN_CALLS+SMS_OUT_CALLS+MMS_OUT_CALLS+EVENTS_CALLS+
                 INTERNET_CALLS+TOTAL_OUT_CALLS+VOICE_OUT_MINS+VOICE_IN_MINS+GPRS_TRAFFIC+EVENTS_TRAFFIC+
                 OUT_CALLS_ROAMING+OUT_MINS_ROAMING+OUT_CALLS_INTERNATIONAL+OUT_MINS_INTERNATIONAL+OUT_CALLS_PEAK+
                 OUT_CALLS_OFFPEAK+OUT_CALLS_WORK+OUT_CALLS_NONWORK+IN_CALLS_PEAK+IN_CALLS_OFFPEAK+IN_CALLS_WORK+      
                 IN_CALLS_NONWORK+DAYS_OUT+DAYS_IN+AGE+GENDER.M ~ mean +(mean*km_clust_3)+(mean*km_clust_4)+
                 (mean*km_clust_5)+(mean*km_clust_6)+(mean*km_clust_7),data = telco_clust)

profile1 <- as.matrix(profile)

profile1 <- data.frame(profile1)

profile2 <- tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6)+
                      (length*km_clust_7),data=telco_clust)

profile2 <- as.matrix(profile2)
profile2 <- data.frame(profile2)

write.csv(profile1,"profile1.csv",row.names = F)
write.csv(profile2,"profile2.csv",row.names = F)
