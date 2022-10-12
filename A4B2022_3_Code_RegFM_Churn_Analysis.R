
##### ************** DATA & LIBRARY IMPORTATION & PREPARATION ************** #####


##### Required libraries ####
library("plyr")
library(dplyr)
library(lubridate)
library(rfm)
library("writexl")
library(markovchain)
library(diagram)
library(expm)
library(pracma)


##### Transaction dataset importation ####
setwd("C:/Users/aubin/OneDrive/Documents/Cours/AFB_MBA_Cleaned_Code/CRM_datasets/Datasets")
data <- read.csv("transaction_dataset.csv",sep=";")
head(data)
dim(data) #rows * columns
colnames(data)
summary(data)
glimpse(data)

sum(is.na(data)) # NA check total
sapply(data, function(x) sum(is.na(x))) # NA check by column

glimpse(data)


##### Transaction dataset mining/transformation ####

#Mining of data dataset:
data$ï..ticket_id<-as.factor(data$ï..ticket_id)
data$id_customer<-as.factor(data$id_customer)
data$date <- as.Date(data$date)#,"%y-%m-%d")
data$store_id<-as.factor(data$store_id)
data$store_type<-as.factor(data$store_type)
data$PL_gross_sales<-as.numeric(gsub(",", ".", data$PL_gross_sales))
data$PL_gross_sales[data$PL_gross_sales == "NULL"] = NA
data$PL_gross_sales<-as.numeric(data$PL_gross_sales)
data$Promo_gross_sales<-as.numeric(gsub(",", ".", data$Promo_gross_sales))
data$Promo_gross_sales[data$Promo_gross_sales == "NULL"] = NA
data$Promo_gross_sales<-as.numeric(data$Promo_gross_sales)
data$net_sales<-as.numeric(gsub(",", ".", data$net_sales))
data$net_sales[data$net_sales == "NULL"] = NA
data$net_sales<-as.numeric(data$net_sales)
data$gross_sales<-as.numeric(gsub(",", ".", data$gross_sales))
data$gross_sales[data$gross_sales == "NULL"] = NA
data$gross_sales<-as.numeric(data$gross_sales)
data$number_items_other[data$number_items_other == "NULL"] = NA
data$number_items_other<-as.numeric(data$number_items_other)
data$number_item_PL[data$number_item_PL == "NULL"] = NA
data$number_item_PL<-as.numeric(data$number_item_PL)

#Display head of data dataset:
glimpse(data)


##### Customer dataset importation ####

cust <- read.csv("customer_dataset.csv",sep=";") #Import
glimpse(cust) #Display head of customers dataset


##### Customers dataset mining/transformation ####

#Transform column types:
cust$ï..first_purchase_year<-as.factor(cust$ï..first_purchase_year)
cust$birth_year<-as.factor(cust$birth_year)
cust$gender<-as.factor(cust$gender)
cust$id_customer<-as.factor(cust$id_customer)
cust$loyalty_store<-as.factor(cust$loyalty_store)
#Display new head of customers dataset:
glimpse(cust)


##### Join tickets and customers datasets by customer ID

df<-inner_join(data,cust,by="id_customer")
glimpse(df)


##### Computation of customer frequencies #####

#Creation of a new dataset 'ids':
ids <- data.frame(table(df$id_customer))
#Frequencies computation in ids dataset:
ids$Freq
#Joining id_customer to ids:
colnames(ids)[1]<-"id_customer"
#Joining ids frequencies to df:
df<-inner_join(df,ids,by="id_customer")
glimpse(df)


##### Add weeks, months and week days to whole data #####

#Combine weeks and months to df:
week<-data.frame(lubridate::week(ymd(df$date)))
month<-data.frame(lubridate::month(ymd(df$date)))
df<-cbind(df,week,month)
colnames(df)[17]<-"week"
colnames(df)[18]<-"month"

#Add one more column with days of the week
Sys.setlocale("LC_TIME", "C")
df$dayweek <- weekdays(as.Date(df$date))

###### ANOMALY DETECTION######
customers=aggregate(data[ , c(9)], by = list(data$id_customer), FUN = sum)
summary(customers)
colnames(customers)[1]<- "id_customer"
colnames(customers)[2]<- "tot_gross_sales"
customers<-inner_join(customers,cust,by="id_customer")
colnames(customers)[3]<- "first_purchase_year"
x11()
boxplot(customers$tot_gross_sales)

customers$first_purchase_year<-as.numeric(as.character(customers$first_purchase_year))
customers$birth_year<-as.numeric(as.character(customers$birth_year))
customers_selection<-filter(customers,customers$tot_gross_sales>40000 | ((customers$first_purchase_year-customers$birth_year)>90))
customers<-inner_join(customers,ids,by="id_customer")






######## RegFM ######## 

#Gross sales by weeks
df_subset<-anti_join(df, customers_selection, by="id_customer")

#delete weeks
df_new<-df_subset[!(df_subset$week=="1"|df_subset$week=="32"|df_subset$week=="33"|
                      df_subset$week=="34"|df_subset$week=="50"|df_subset$week=="51"|df_subset$week=="52"|df_subset$week=="53"),]

#create datasets for the 9 different periods
#subset 1
df_new1<-df_new[(df_new$week=="2"|df_new$week=="3"|df_new$week=="4"|df_new$week=="5"|df_new$week=="6"),]
#subset 2
df_new2<-df_new[(df_new$week=="7"|df_new$week=="8"|df_new$week=="9"|df_new$week=="10"|df_new$week=="11"),]
#subset 3
df_new3<-df_new[(df_new$week=="12"|df_new$week=="13"|df_new$week=="14"|df_new$week=="15"|df_new$week=="16"),]
#subset 4
df_new4<-df_new[(df_new$week=="17"|df_new$week=="18"|df_new$week=="19"|df_new$week=="20"|df_new$week=="21"),]
#subset 5
df_new5<-df_new[(df_new$week=="22"|df_new$week=="23"|df_new$week=="24"|df_new$week=="25"|df_new$week=="26"),]
#subset 6
df_new6<-df_new[(df_new$week=="27"|df_new$week=="28"|df_new$week=="29"|df_new$week=="30"|df_new$week=="31"),]
#subset 7
df_new7<-df_new[(df_new$week=="35"|df_new$week=="36"|df_new$week=="37"|df_new$week=="38"|df_new$week=="39"),]
#subset 8
df_new8<-df_new[(df_new$week=="40"|df_new$week=="41"|df_new$week=="42"|df_new$week=="43"|df_new$week=="44"),]
#subset 9
df_new9<-df_new[(df_new$week=="45"|df_new$week=="46"|df_new$week=="47"|df_new$week=="48"|df_new$week=="49"),]


#assign recency date
#recency 1
rfm_date_1 <- lubridate::as_date("2021-02-11")
#recency 2
rfm_date_2 <- lubridate::as_date("2021-03-18")
#recency 3
rfm_date_3 <- lubridate::as_date("2021-04-22")
#recency 4
rfm_date_4 <- lubridate::as_date("2021-05-27")
#recency 5
rfm_date_5 <- lubridate::as_date("2021-07-01")
#recency 6
rfm_date_6 <- lubridate::as_date("2021-08-05")
#recency 7
rfm_date_7 <- lubridate::as_date("2021-09-30")
#recency 8
rfm_date_8 <- lubridate::as_date("2021-11-04")
#recency 9
rfm_date_9 <- lubridate::as_date("2021-12-09")

#create datasets to exploit for assigning later RegFM rules
#rfm_1
rfm_result_1 <- rfm_table_order(df_new1, id_customer, date, gross_sales, rfm_date_1)
segment_1 <- rfm_segment(rfm_result_1)
#rfm_2
rfm_result_2 <- rfm_table_order(df_new2, id_customer, date, gross_sales, rfm_date_2)
segment_2 <- rfm_segment(rfm_result_2)
#rfm_3
rfm_result_3 <- rfm_table_order(df_new3, id_customer, date, gross_sales, rfm_date_3)
segment_3 <- rfm_segment(rfm_result_3)
#rfm_4
rfm_result_4 <- rfm_table_order(df_new4, id_customer, date, gross_sales, rfm_date_4)
segment_4 <- rfm_segment(rfm_result_4)
#rfm_5
rfm_result_5 <- rfm_table_order(df_new5, id_customer, date, gross_sales, rfm_date_5)
segment_5 <- rfm_segment(rfm_result_5)
#rfm_6
rfm_result_6 <- rfm_table_order(df_new6, id_customer, date, gross_sales, rfm_date_6)
segment_6 <- rfm_segment(rfm_result_6)
#rfm_7
rfm_result_7 <- rfm_table_order(df_new7, id_customer, date, gross_sales, rfm_date_7)
segment_7 <- rfm_segment(rfm_result_7)
#rfm_8
rfm_result_8 <- rfm_table_order(df_new8, id_customer, date, gross_sales, rfm_date_8)
segment_8 <- rfm_segment(rfm_result_8)
#rfm_9
rfm_result_9 <- rfm_table_order(df_new9, id_customer, date, gross_sales, rfm_date_9)
segment_9 <- rfm_segment(rfm_result_9)

#add a column to identify the time
segment_1$time<-'1'
segment_2$time<-'2'
segment_3$time<-'3'
segment_4$time<-'4'
segment_5$time<-'5'
segment_6$time<-'6'
segment_7$time<-'7'
segment_8$time<-'8'
segment_9$time<-'9'

#group datasets
segment_new<-rbind(segment_1[,c(1,4,5,6,10)],segment_2[,c(1,4,5,6,10)],segment_3[,c(1,4,5,6,10)],segment_4[,c(1,4,5,6,10)]
                   ,segment_5[,c(1,4,5,6,10)],segment_6[,c(1,4,5,6,10)],segment_7[,c(1,4,5,6,10)],segment_8[,c(1,4,5,6,10)],segment_9[,c(1,4,5,6,10)])

#create REGULARITY metrics
segment_new$regularity<-35/segment_new$transaction_count-segment_new$recency_days
x11()
hist(segment_new$regularity)

segment_new$Regularity<-case_when(segment_new$regularity <= -5 ~ "1",
                                  segment_new$regularity > -5 & segment_new$regularity <= 5 ~ "2", 
                                  segment_new$regularity > 5 ~ "3")
#create MONETARY metrics
round(quantile(segment_new$amount,  prob=c(.35,.75,1)),0)

segment_new$Monetary<-case_when(segment_new$amount <= 165 ~ "1",
                                segment_new$amount > 165 & segment_new$amount <= 455 ~ "2", 
                                segment_new$amount > 455 ~ "3")

#create FREQUENCY metrics
quantile(segment_new$transaction_count, prob=c(.35,.75,1))

segment_new$Frequency<-case_when(segment_new$transaction_count <= 5 ~ "1",
                                 segment_new$transaction_count > 5 & segment_new$transaction_count <= 12 ~ "2", 
                                 segment_new$transaction_count > 12 ~ "3")



segment_new$Monetary<-as.numeric(segment_new$Monetary)
segment_new$Frequency<-as.numeric(segment_new$Frequency)
segment_new$Regularity<-as.numeric(segment_new$Regularity)



#create CUSTOMERS' CATEGORIES
segment_new$Category<-case_when(segment_new$Monetary == 3 & segment_new$Frequency >= 2 & segment_new$Regularity >= 2 ~ "01_Champions",
                                segment_new$Monetary == 2 & segment_new$Frequency >= 2 & segment_new$Regularity >= 2 ~ "02_Loyal_Customers", 
                                segment_new$Monetary == 1 & segment_new$Frequency >= 2 & segment_new$Regularity >= 1 ~ "06_Promising",
                                segment_new$Monetary >= 2 & segment_new$Frequency >= 2 & segment_new$Regularity == 1 ~ "04_Can't_Lose_Them",
                                segment_new$Monetary == 3 & segment_new$Frequency == 1 & segment_new$Regularity >= 2 ~ "03_Whoolesalers",
                                segment_new$Monetary == 2 & segment_new$Frequency == 1 & segment_new$Regularity >= 2 ~ "07_Recent_Users",
                                segment_new$Monetary >= 2 & segment_new$Frequency == 1 & segment_new$Regularity == 1 ~ "05_Need_Attention",
                                segment_new$Monetary == 1 & segment_new$Frequency >= 1 & segment_new$Regularity <= 2 ~ "09_Partial_Churner",
                                segment_new$Monetary == 1 & segment_new$Frequency <= 2 & segment_new$Regularity == 3 ~ "08_New_Customers")

#create again the periods
period_1<-filter(segment_new, segment_new$time=="1")
period_2<-filter(segment_new, segment_new$time=="2")
period_3<-filter(segment_new, segment_new$time=="3")
period_4<-filter(segment_new, segment_new$time=="4")
period_5<-filter(segment_new, segment_new$time=="5")
period_6<-filter(segment_new, segment_new$time=="6")
period_7<-filter(segment_new, segment_new$time=="7")
period_8<-filter(segment_new, segment_new$time=="8")
period_9<-filter(segment_new, segment_new$time=="9")


#change the column names
colnames(period_1)[10]="period_1"
colnames(period_2)[10]="period_2"
colnames(period_3)[10]="period_3"
colnames(period_4)[10]="period_4"
colnames(period_5)[10]="period_5"
colnames(period_6)[10]="period_6"
colnames(period_7)[10]="period_7"
colnames(period_8)[10]="period_8"
colnames(period_9)[10]="period_9"

#join the datasets and change na with Total_Churner
path<-join_all(list(period_1[,c(1,10)],period_2[,c(1,10)],period_3[,c(1,10)],period_4[,c(1,10)],period_5[,c(1,10)],period_6[,c(1,10)],period_7[,c(1,10)],period_8[,c(1,10)],period_9[,c(1,10)])
               ,by="customer_id", type='full')

path[is.na(path)] <- "10_Total_Churner"

#transform the variables segment as factor
for (i in 2:10) {
  path[,i]<-factor(path[,i])
}

#####CREATE CUSTOMERS' CATEGORIES######
#data manipulation for creating later the customers categories
df_ptp1<-aggregate(df_new1[ , c(8,9)], by=list(df_new1$id_customer), FUN=sum)
colnames(df_ptp1)[1]<-"id_customer"
df_ptp1$prone_to_promotion_1<-df_ptp1$net_sales/df_ptp1$gross_sales

df_ptp2<-aggregate(df_new2[ , c(8,9)], by=list(df_new2$id_customer), FUN=sum)
colnames(df_ptp2)[1]<-"id_customer"
df_ptp2$prone_to_promotion_2<-df_ptp2$net_sales/df_ptp2$gross_sales

df_ptp3<-aggregate(df_new3[ , c(8,9)], by=list(df_new3$id_customer), FUN=sum)
colnames(df_ptp3)[1]<-"id_customer"
df_ptp3$prone_to_promotion_3<-df_ptp3$net_sales/df_ptp3$gross_sales

df_ptp4<-aggregate(df_new4[ , c(8,9)], by=list(df_new4$id_customer), FUN=sum)
colnames(df_ptp4)[1]<-"id_customer"
df_ptp4$prone_to_promotion_4<-df_ptp4$net_sales/df_ptp4$gross_sales

df_ptp5<-aggregate(df_new5[ , c(8,9)], by=list(df_new5$id_customer), FUN=sum)
colnames(df_ptp5)[1]<-"id_customer"
df_ptp5$prone_to_promotion_5<-df_ptp5$net_sales/df_ptp5$gross_sales

df_ptp6<-aggregate(df_new6[ , c(8,9)], by=list(df_new6$id_customer), FUN=sum)
colnames(df_ptp6)[1]<-"id_customer"
df_ptp6$prone_to_promotion_6<-df_ptp6$net_sales/df_ptp6$gross_sales

df_ptp7<-aggregate(df_new7[ , c(8,9)], by=list(df_new7$id_customer), FUN=sum)
colnames(df_ptp7)[1]<-"id_customer"
df_ptp7$prone_to_promotion_7<-df_ptp7$net_sales/df_ptp7$gross_sales

df_ptp8<-aggregate(df_new8[ , c(8,9)], by=list(df_new8$id_customer), FUN=sum)
colnames(df_ptp8)[1]<-"id_customer"
df_ptp8$prone_to_promotion_8<-df_ptp8$net_sales/df_ptp8$gross_sales

df_ptp9<-aggregate(df_new9[ , c(8,9)], by=list(df_new9$id_customer), FUN=sum)
colnames(df_ptp9)[1]<-"id_customer"
df_ptp9$prone_to_promotion_9<-df_ptp9$net_sales/df_ptp9$gross_sales

Y<-path
colnames(Y)[1]<-"id_customer"
cmc<-join_all(list(Y[,],customers[,c(1,3,4,5)], df_ptp1[,c(1,4)],df_ptp2[,c(1,4)], df_ptp3[,c(1,4)], df_ptp4[,c(1,4)],
                   df_ptp5[,c(1,4)], df_ptp6[,c(1,4)], df_ptp7[,c(1,4)], df_ptp8[,c(1,4)], df_ptp9[,c(1,4)]), by="id_customer", type='left')

colnames(cmc)[11]<-"first_purchase_year"

cmc$time_1<-'1'
cmc$time_2<-'2'
cmc$time_3<-'3'
cmc$time_4<-'4'
cmc$time_5<-'5'
cmc$time_6<-'6'
cmc$time_7<-'7'
cmc$time_8<-'8'
cmc$time_9<-'9'

cmc$first_purchase_year<-as.numeric(as.character(cmc$first_purchase_year))
cmc$birth_year<-as.numeric(as.character(cmc$birth_year))
cmc$age<-2021-cmc$birth_year
cmc$duration_of_relationship<-2022-cmc$first_purchase_year
cmc$duration_of_relationship[cmc$duration_of_relationship=="-1"] <- 0

#Age
cmc$age_class<-case_when(cmc$age %in%  0:34 ~ "Young", 
                         cmc$age %in%  35:55 ~ "Adult",
                         cmc$age %in%  56:75 ~ "Senior",
                         cmc$age %in%  76:400 ~ "Old")

#Duration of the relationship
cmc$duration_of_relationship_class<-case_when(cmc$duration_of_relationship %in% 0:1 ~ "1",
                                              cmc$duration_of_relationship %in%  2:5 ~ "2", 
                                              cmc$duration_of_relationship %in%  6:10 ~ "3",
                                              cmc$duration_of_relationship %in%  11:51 ~ "4")

#exlude some not active customers + other data manipulation
for (i in 2:10) {
  cmc[,i]<-as.character(cmc[,i])
}
cmc$period_1[cmc$period_1=="10_Total_Churner" & cmc$first_purchase_year=="2021"] <- "11_Not_Active"
cmc$period_2[cmc$period_1=="11_Not_Active"&cmc$period_2=="10_Total_Churner" & cmc$first_purchase_year=="2021"] <- "11_Not_Active"
cmc$period_3[cmc$period_2=="11_Not_Active"&cmc$period_3=="10_Total_Churner" & cmc$first_purchase_year=="2021"] <- "11_Not_Active"
cmc$period_4[cmc$period_3=="11_Not_Active"&cmc$period_4=="10_Total_Churner" & cmc$first_purchase_year=="2021"] <- "11_Not_Active"
cmc$period_5[cmc$period_4=="11_Not_Active"&cmc$period_5=="10_Total_Churner" & cmc$first_purchase_year=="2021"] <- "11_Not_Active"
cmc$period_6[cmc$period_5=="11_Not_Active"&cmc$period_6=="10_Total_Churner" & cmc$first_purchase_year=="2021"] <- "11_Not_Active"
cmc$period_7[cmc$period_6=="11_Not_Active"&cmc$period_7=="10_Total_Churner" & cmc$first_purchase_year=="2021"] <- "11_Not_Active"
cmc$period_8[cmc$period_7=="11_Not_Active"&cmc$period_8=="10_Total_Churner" & cmc$first_purchase_year=="2021"] <- "11_Not_Active"
cmc$period_9[cmc$period_8=="11_Not_Active"&cmc$period_9=="10_Total_Churner" & cmc$first_purchase_year=="2021"] <- "11_Not_Active"
for (i in 2:10) {
  cmc[,i]<-factor(cmc[,i])
}


cmc1<-cmc[,c(1,2,3,23,14,13,34,35)]
cmc2<-cmc[,c(1,3,4,24,15,13,34,35)]
cmc3<-cmc[,c(1,4,5,25,16,13,34,35)]
cmc4<-cmc[,c(1,5,6,26,17,13,34,35)]
cmc5<-cmc[,c(1,6,7,27,18,13,34,35)]
cmc6<-cmc[,c(1,7,8,28,19,13,34,35)]
cmc7<-cmc[,c(1,8,9,29,20,13,34,35)]
cmc8<-cmc[,c(1,9,10,30,21,13,34,35)]

colnames(cmc2)=colnames(cmc3)=colnames(cmc4)=colnames(cmc5)=colnames(cmc6)=colnames(cmc7)=colnames(cmc8)=colnames(cmc1)

cmc_new<-rbind(cmc1,cmc2,cmc3,cmc4,cmc5,cmc6,cmc7,cmc8)
colnames(cmc_new)[2]<-"starting_category"
colnames(cmc_new)[3]<-"landing_category"
colnames(cmc_new)[4]<-"time"
colnames(cmc_new)[5]<-"prone_to_promotion"

cmc_new$time<-as.factor(cmc_new$time)
cmc_new$age_class<-as.factor(cmc_new$age_class)
cmc_new$duration_of_relationship_class<-as.factor(cmc_new$duration_of_relationship_class)

#Prone to Promotion
cmc_new$prone_to_promotion<-case_when(cmc_new$prone_to_promotion == 0 ~ "NoValue",
                                      cmc_new$prone_to_promotion > 0 & cmc_new$prone_to_promotion <= 0.8 ~ "VeryProne", 
                                      cmc_new$prone_to_promotion > 0.8 & cmc_new$prone_to_promotion <= 0.9 ~ "Prone",
                                      cmc_new$prone_to_promotion > 0.9 & cmc_new$prone_to_promotion <= 1 ~ "NoProne")

cmc_new$prone_to_promotion<-as.factor(cmc_new$prone_to_promotion)


########### MARKOV CHAIN ON THE WHOLE DATASET############
#total dataset
mcTotal<-cmc[,c(2:10)]

mcFitTotal <- markovchainFit(data=mcTotal)
mcFitTotal$estimate
transitionMatrixTotal<-mcFitTotal$estimate@transitionMatrix
transitionMatrixTotal


########### MARKOV CHAIN FILTERING BY CUSTOMERS' CHARACTERISTICS############
#filtering
mcM<-filter(cmc[,c(2:10)], cmc$gender=="M")
mcF<-filter(cmc[,c(2:10)], cmc$gender=="F")
mcYoung<-filter(cmc[,c(2:10)], cmc$age_class=="Young")
mcAdult<-filter(cmc[,c(2:10)], cmc$age_class=="Adult")
mcSenior<-filter(cmc[,c(2:10)], cmc$age_class=="Senior")
mcOld<-filter(cmc[,c(2:10)], cmc$age_class=="Old")
mc1<-filter(cmc[,c(2:10)], cmc$duration_of_relationship_class=="1")
mc2<-filter(cmc[,c(2:10)], cmc$duration_of_relationship_class=="2")
mc3<-filter(cmc[,c(2:10)], cmc$duration_of_relationship_class=="3")
mc4<-filter(cmc[,c(2:10)], cmc$duration_of_relationship_class=="4")
mct12<-cmc1[,c(2:3)]
mct23<-cmc2[,c(2:3)]
mct34<-cmc3[,c(2:3)]
mct45<-cmc4[,c(2:3)]
mct56<-cmc5[,c(2:3)]
mct67<-cmc6[,c(2:3)]
mct78<-cmc7[,c(2:3)]
mct89<-cmc8[,c(2:3)]
mcVeryProne<-filter(cmc_new[,2:3], cmc_new$prone_to_promotion=="VeryProne")
mcProne<-filter(cmc_new[,2:3], cmc_new$prone_to_promotion=="Prone")
mcNoProne<-filter(cmc_new[,2:3], cmc_new$prone_to_promotion=="NoProne")

mcMYoung<-filter(cmc[,c(2:10)], cmc$gender=="M" & cmc$age_class=="Young")
mcFYoung<-filter(cmc[,c(2:10)], cmc$gender=="F" & cmc$age_class=="Young")
mcMAdult<-filter(cmc[,c(2:10)], cmc$gender=="M" & cmc$age_class=="Adult")
mcFAdult<-filter(cmc[,c(2:10)], cmc$gender=="F" & cmc$age_class=="Adult")
mcMSenior<-filter(cmc[,c(2:10)], cmc$gender=="M" & cmc$age_class=="Senior")
mcFSenior<-filter(cmc[,c(2:10)], cmc$gender=="F" & cmc$age_class=="Senior")
mcMOld<-filter(cmc[,c(2:10)], cmc$gender=="M" & cmc$age_class=="Old")
mcFOld<-filter(cmc[,c(2:10)], cmc$gender=="F" & cmc$age_class=="Old")

mc1VeryProne<-filter(cmc_new[,2:3], cmc$duration_of_relationship_class=="1" & cmc_new$prone_to_promotion=="VeryProne")
mc1Prone<-filter(cmc_new[,2:3], cmc$duration_of_relationship_class=="1" & cmc_new$prone_to_promotion=="Prone")
mc1NoProne<-filter(cmc_new[,2:3], cmc$duration_of_relationship_class=="1" & cmc_new$prone_to_promotion=="NoProne")
mc2VeryProne<-filter(cmc_new[,2:3], cmc$duration_of_relationship_class=="2" & cmc_new$prone_to_promotion=="VeryProne")
mc2Prone<-filter(cmc_new[,2:3], cmc$duration_of_relationship_class=="2" & cmc_new$prone_to_promotion=="Prone")
mc2NoProne<-filter(cmc_new[,2:3], cmc$duration_of_relationship_class=="2" & cmc_new$prone_to_promotion=="NoProne")
mc3VeryProne<-filter(cmc_new[,2:3], cmc$duration_of_relationship_class=="3" & cmc_new$prone_to_promotion=="VeryProne")
mc3Prone<-filter(cmc_new[,2:3], cmc$duration_of_relationship_class=="3" & cmc_new$prone_to_promotion=="Prone")
mc3NoProne<-filter(cmc_new[,2:3], cmc$duration_of_relationship_class=="3" & cmc_new$prone_to_promotion=="NoProne")
mc4VeryProne<-filter(cmc_new[,2:3], cmc$duration_of_relationship_class=="4" & cmc_new$prone_to_promotion=="VeryProne")
mc4Prone<-filter(cmc_new[,2:3], cmc$duration_of_relationship_class=="4" & cmc_new$prone_to_promotion=="Prone")
mc4NoProne<-filter(cmc_new[,2:3], cmc$duration_of_relationship_class=="4" & cmc_new$prone_to_promotion=="NoProne")

mcM1<-filter(cmc[,c(2:10)], cmc$gender=="M" & cmc$duration_of_relationship_class=="1")
mcF1<-filter(cmc[,c(2:10)], cmc$gender=="F" & cmc$duration_of_relationship_class=="1")
mcM2<-filter(cmc[,c(2:10)], cmc$gender=="M" & cmc$duration_of_relationship_class=="2")
mcF2<-filter(cmc[,c(2:10)], cmc$gender=="F" & cmc$duration_of_relationship_class=="2")
mcM3<-filter(cmc[,c(2:10)], cmc$gender=="M" & cmc$duration_of_relationship_class=="3")
mcF3<-filter(cmc[,c(2:10)], cmc$gender=="F" & cmc$duration_of_relationship_class=="3")
mcM4<-filter(cmc[,c(2:10)], cmc$gender=="M" & cmc$duration_of_relationship_class=="4")
mcF4<-filter(cmc[,c(2:10)], cmc$gender=="F" & cmc$duration_of_relationship_class=="4")

#create transition matrix and plot results

#gender
mcFitM <- markovchainFit(data=mcM)
mcFitM$estimate
transitionMatrixM<-mcFitM$estimate@transitionMatrix
transitionMatrixM

mcFitF <- markovchainFit(data=mcF)
mcFitF$estimate
transitionMatrixF<-mcFitF$estimate@transitionMatrix
transitionMatrixF

#age_class
mcFitYoung <- markovchainFit(data=mcYoung)
mcFitYoung$estimate
transitionMatrixYoung<-mcFitYoung$estimate@transitionMatrix
transitionMatrixYoung

mcFitAdult <- markovchainFit(data=mcAdult)
mcFitAdult$estimate
transitionMatrixAdult<-mcFitAdult$estimate@transitionMatrix
transitionMatrixAdult

mcFitSenior <- markovchainFit(data=mcSenior)
mcFitSenior$estimate
transitionMatrixSenior<-mcFitSenior$estimate@transitionMatrix
transitionMatrixSenior

mcFitOld <- markovchainFit(data=mcOld)
mcFitOld$estimate
transitionMatrixOld<-mcFitOld$estimate@transitionMatrix
transitionMatrixOld

#duration_of_relationship_class
mcFit1 <- markovchainFit(data=mc1)
mcFit1$estimate
transitionMatrix1<-mcFit1$estimate@transitionMatrix
transitionMatrix1

mcFit2 <- markovchainFit(data=mc2)
mcFit2$estimate
transitionMatrix2<-mcFit2$estimate@transitionMatrix
transitionMatrix2

mcFit3 <- markovchainFit(data=mc3)
mcFit3$estimate
transitionMatrix3<-mcFit3$estimate@transitionMatrix
transitionMatrix3

mcFit4 <- markovchainFit(data=mc4)
mcFit4$estimate
transitionMatrix4<-mcFit4$estimate@transitionMatrix
transitionMatrix4

#time
mcFitt12 <- markovchainFit(data=mct12)
mcFitt12$estimate
transitionMatrixt12<-mcFitt12$estimate@transitionMatrix
transitionMatrixt12

mcFitt23<- markovchainFit(data=mct23)
mcFitt23$estimate
transitionMatrixt23<-mcFitt23$estimate@transitionMatrix
transitionMatrixt23

mcFitt34 <- markovchainFit(data=mct34)
mcFitt34$estimate
transitionMatrixt34<-mcFitt34$estimate@transitionMatrix
transitionMatrixt34

mcFitt45 <- markovchainFit(data=mct45)
mcFitt45$estimate
transitionMatrixt45<-mcFitt45$estimate@transitionMatrix
transitionMatrixt45

mcFitt56 <- markovchainFit(data=mct56)
mcFitt56$estimate
transitionMatrixt56<-mcFitt56$estimate@transitionMatrix
transitionMatrixt56

mcFitt67 <- markovchainFit(data=mct67)
mcFitt67$estimate
transitionMatrixt67<-mcFitt67$estimate@transitionMatrix
transitionMatrixt67

mcFitt78 <- markovchainFit(data=mct78)
mcFitt78$estimate
transitionMatrixt78<-mcFitt78$estimate@transitionMatrix
transitionMatrixt78

mcFitt89 <- markovchainFit(data=mct89)
mcFitt89$estimate
transitionMatrixt89<-mcFitt89$estimate@transitionMatrix
transitionMatrixt89

#prone_to_promotions
mcFitVeryProne <- markovchainFit(data=mcVeryProne)
mcFitVeryProne$estimate
transitionMatrixVeryProne<-mcFitVeryProne$estimate@transitionMatrix
transitionMatrixVeryProne

mcFitProne <- markovchainFit(data=mcProne)
mcFitProne$estimate
transitionMatrixProne<-mcFitProne$estimate@transitionMatrix
transitionMatrixProne

mcFitNoProne <- markovchainFit(data=mcNoProne)
mcFitNoProne$estimate
transitionMatrixNoProne<-mcFitNoProne$estimate@transitionMatrix
transitionMatrixNoProne

#gender+age
mcFitMYoung <- markovchainFit(data=mcMYoung)
mcFitMYoung$estimate
transitionMatrixMYoung<-mcFitMYoung$estimate@transitionMatrix
transitionMatrixMYoung

mcFitFYoung <- markovchainFit(data=mcFYoung)
mcFitFYoung$estimate
transitionMatrixFYoung<-mcFitFYoung$estimate@transitionMatrix
transitionMatrixFYoung

mcFitMAdult <- markovchainFit(data=mcMAdult)
mcFitMAdult$estimate
transitionMatrixMAdult<-mcFitMAdult$estimate@transitionMatrix
transitionMatrixMAdult

mcFitFAdult <- markovchainFit(data=mcFAdult)
mcFitFAdult$estimate
transitionMatrixFAdult<-mcFitFAdult$estimate@transitionMatrix
transitionMatrixFAdult

mcFitMSenior <- markovchainFit(data=mcMSenior)
mcFitMSenior$estimate
transitionMatrixMSenior<-mcFitMSenior$estimate@transitionMatrix
transitionMatrixMSenior

mcFitFSenior <- markovchainFit(data=mcFSenior)
mcFitFSenior$estimate
transitionMatrixFSenior<-mcFitFSenior$estimate@transitionMatrix
transitionMatrixFSenior

mcFitMOld <- markovchainFit(data=mcMOld)
mcFitMOld$estimate
transitionMatrixMOld<-mcFitMOld$estimate@transitionMatrix
transitionMatrixMOld

mcFitFOld <- markovchainFit(data=mcFOld)
mcFitFOld$estimate
transitionMatrixFOld<-mcFitFOld$estimate@transitionMatrix
transitionMatrixFOld

#duration of relationship+prone to promotions
mcFit1VeryProne <- markovchainFit(data=mc1VeryProne)
mcFit1VeryProne$estimate
transitionMatrix1VeryProne<-mcFit1VeryProne$estimate@transitionMatrix
transitionMatrix1VeryProne

mcFit1Prone <- markovchainFit(data=mc1Prone)
mcFit1Prone$estimate
transitionMatrix1Prone<-mcFit1Prone$estimate@transitionMatrix
transitionMatrix1Prone

mcFit1NoProne <- markovchainFit(data=mc1NoProne)
mcFit1NoProne$estimate
transitionMatrix1NoProne<-mcFit1NoProne$estimate@transitionMatrix
transitionMatrix1NoProne

mcFit2VeryProne <- markovchainFit(data=mc2VeryProne)
mcFit2VeryProne$estimate
transitionMatrix2VeryProne<-mcFit2VeryProne$estimate@transitionMatrix
transitionMatrix2VeryProne

mcFit2Prone <- markovchainFit(data=mc2Prone)
mcFit2Prone$estimate
transitionMatrix2Prone<-mcFit2Prone$estimate@transitionMatrix
transitionMatrix2Prone

mcFit2NoProne <- markovchainFit(data=mc2NoProne)
mcFit2NoProne$estimate
transitionMatrix2NoProne<-mcFit2NoProne$estimate@transitionMatrix
transitionMatrix2NoProne

mcFit3VeryProne <- markovchainFit(data=mc3VeryProne)
mcFit3VeryProne$estimate
transitionMatrix3VeryProne<-mcFit3VeryProne$estimate@transitionMatrix
transitionMatrix3VeryProne

mcFit3Prone <- markovchainFit(data=mc3Prone)
mcFit3Prone$estimate
transitionMatrix3Prone<-mcFit3Prone$estimate@transitionMatrix
transitionMatrix3Prone

mcFit3NoProne <- markovchainFit(data=mc3NoProne)
mcFit3NoProne$estimate
transitionMatrix3NoProne<-mcFit3NoProne$estimate@transitionMatrix
transitionMatrix3NoProne

mcFit4VeryProne <- markovchainFit(data=mc4VeryProne)
mcFit4VeryProne$estimate
transitionMatrix4VeryProne<-mcFit4VeryProne$estimate@transitionMatrix
transitionMatrix4VeryProne

mcFit4Prone <- markovchainFit(data=mc4Prone)
mcFit4Prone$estimate
transitionMatrix4Prone<-mcFit4Prone$estimate@transitionMatrix
transitionMatrix4Prone

mcFit4NoProne <- markovchainFit(data=mc4NoProne)
mcFit4NoProne$estimate
transitionMatrix4NoProne<-mcFit4NoProne$estimate@transitionMatrix
transitionMatrix4NoProne

#gender+duration of relationship
mcFitM1 <- markovchainFit(data=mcM1)
mcFitM1$estimate
transitionMatrixM1<-mcFitM1$estimate@transitionMatrix
transitionMatrixM1

mcFitF1 <- markovchainFit(data=mcF1)
mcFitF1$estimate
transitionMatrixF1<-mcFitF1$estimate@transitionMatrix
transitionMatrixF1

mcFitM2 <- markovchainFit(data=mcM2)
mcFitM2$estimate
transitionMatrixM2<-mcFitM2$estimate@transitionMatrix
transitionMatrixM2

mcFitF2 <- markovchainFit(data=mcF2)
mcFitF2$estimate
transitionMatrixF2<-mcFitF2$estimate@transitionMatrix
transitionMatrixF2

mcFitM3 <- markovchainFit(data=mcM3)
mcFitM3$estimate
transitionMatrixM3<-mcFitM3$estimate@transitionMatrix
transitionMatrixM3

mcFitF3 <- markovchainFit(data=mcF3)
mcFitF3$estimate
transitionMatrixF3<-mcFitF3$estimate@transitionMatrix
transitionMatrixF3

mcFitM4 <- markovchainFit(data=mcM4)
mcFitM4$estimate
transitionMatrixM4<-mcFitM4$estimate@transitionMatrix
transitionMatrixM4

mcFitF4 <- markovchainFit(data=mcF4)
mcFitF4$estimate
transitionMatrixF4<-mcFitF4$estimate@transitionMatrix
transitionMatrixF4



######## Churn analysis ######## 


##### Step 0: Creation of Weekly_purchases dataset: 

# We decide to erase the transactions that occurred during the weeks related to the italian winter and summer holidays from the df dataset:
weekly_purchases <- df[!(df$week=="1"|df$week=="32"|df$week=="33"|df$week=="34"|df$week=="51"|df$week=="52"|df$week=="53"),]

#Keep only id_customer, net_sales and week columns from df, and reorder them:
weekly_purchases <- weekly_purchases[ , which(names(weekly_purchases) %in% c("id_customer","net_sales","week"))]
weekly_purchases <- weekly_purchases[,c(1,3,2)]
glimpse(weekly_purchases)

##### Step 1: Customer spending  aggregated at a weekly level
weekly_purchases <-weekly_purchases %>%
  group_by(id_customer, week) %>%
  summarise(net_sum=sum(net_sales))

#All the weeks are added for each customer in order to take into consideration also the weeks without purchase from the customers
#To do so, we create a dataframe called 'churn.df':
churn.df <- data.frame(id_customer=rep(0,nlevels(weekly_purchases$id_customer)*nlevels(as.factor(weekly_purchases$week))))
churn.df$id_customer <- rep(levels(weekly_purchases$id_customer),each=nlevels(as.factor(weekly_purchases$week)))
churn.df["week"] <- rep(levels(as.factor(weekly_purchases$week)),nlevels(weekly_purchases$id_customer))
churn.df["id_customer_week"]<-paste(churn.df$id_customer,churn.df$week)

#Then, we add this work into a new weekly_purchase column called 'id_customer_week':
weekly_purchases["id_customer_week"]<-paste(weekly_purchases$id_customer, weekly_purchases$week)
glimpse(weekly_purchases)

#We Join churn.df and weekly_purchases by this new 'customer_week_id' column:
weekly_purchases <- left_join(churn.df,weekly_purchases,by="id_customer_week") 

#We clean and rearrange weekly_purchases dataset:
weekly_purchases <- weekly_purchases[,which(names(weekly_purchases) %in% c("id_customer.x","week.x","id_customer_week","net_sum"))]
colnames(weekly_purchases)[1] <- "id_customer"
colnames(weekly_purchases)[2] <- "week"

# We replace NA values by 0 --> net_sum are zeros in the weeks where there is no purchase, instead of NA:
sapply(weekly_purchases, function(x) sum(is.na(x))) # Check for NA
weekly_purchases[is.na(weekly_purchases)] <- 0


##### Step 2: Moving average of the customer weekly spend computation to smoothen out fluctuation

# We arrange weekly_purchases:
weekly_purchases <- weekly_purchases %>%
  dplyr::arrange(id_customer) %>% 
  dplyr::group_by(id_customer) %>% 
  dplyr::mutate(moving_avg = zoo::rollmean(net_sum, k = 6, fill = NA, align = c("right"))) %>%
  dplyr::ungroup()

# We round the results to 2 digits in order to get rid of very small numbers:
weekly_purchases$moving_avg[complete.cases(weekly_purchases$moving_avg)]<-format(round(weekly_purchases$moving_avg[complete.cases(weekly_purchases$moving_avg)], 2), nsmall = 2)
weekly_purchases$moving_avg<-as.numeric(weekly_purchases$moving_avg)

# Afterwards, we want to visualize the purchases of desired customer. For example, customer_id = 1000:
choose.id <- data.frame(weekly_purchases[weekly_purchases$id_customer==1000,])
x11()
plot(choose.id$week,choose.id$net_sum,col='red',type='l', xlab='week',ylab='net sales') #plot the net sales in red
points(choose.id$week,choose.id$moving_avg,col='blue',type='l') #plot the moving average in blue
legend('topright', c('net Sales', 'Moving Average'), col=c('red', 'blue'), lty=c(2,3), lwd=2) #Add a legend
title('Moving Average of Customer') #Add a title


##### Step 3: Standard deviation of the moving average computation:

weekly_purchases <- weekly_purchases %>%
  dplyr::arrange(desc(id_customer)) %>% 
  dplyr::group_by(id_customer) %>% 
  dplyr::mutate(moving_avg_std_dev = zoo::rollapply(moving_avg, width = 12, FUN = sd, fill = NA, align = c("right"))) %>% 
  dplyr::ungroup()

# We round the moving average standard deviation to 2 digits in order to get rid of very small numbers:
weekly_purchases$moving_avg_std_dev[complete.cases(weekly_purchases$moving_avg_std_dev)]<-format(round(weekly_purchases$moving_avg_std_dev[complete.cases(weekly_purchases$moving_avg_std_dev)], 2), nsmall = 2)
weekly_purchases$moving_avg_std_dev<-as.numeric(weekly_purchases$moving_avg_std_dev)

# Visualize the desired customers purchases with a threshold
choose.id <- data.frame(weekly_purchases[weekly_purchases$id_customer==1000,])
x11()
plot(choose.id$week,choose.id$moving_avg,col='red',type='l',ylim=c(0, max(choose.id$moving_avg,na.rm=1)), xlab='week',ylab='net sales')
points(choose.id$week,choose.id$moving_avg_std_dev,col='blue',type='l')
legend('topright', c('Moving Average', 'Moving Average Standard Deviation'), col=c('red', 'blue'), lty=c(2,3), lwd=2)
title('Moving Average and the Standard Deviation of Moving Average of Customer')


##### Step 4:Define a minimum threshold

#Threshold computation:
weekly_purchases["threshold"] <- pmax(0,weekly_purchases$moving_avg - weekly_purchases$moving_avg_std_dev*2)

# We display the purchases of desired customer with the Threshold:
choose.id <- data.frame(weekly_purchases[weekly_purchases$id_customer==1000,])
x11()
plot(choose.id$week,choose.id$moving_avg,col='red',type='l',ylim=c(0, max(choose.id$moving_avg,na.rm=1)), xlab='week',ylab='net sales')
points(choose.id$week,choose.id$threshold,col='blue',type='l')
legend('topright', c('Moving Average', 'Threshold'), col=c('red', 'blue'), lty=c(2,3), lwd=2)
title('Moving Average and the Threshold of Customer')


##### Step 5: Move the minimum threshold by 6 weeks to take pro-active action

weekly_purchases$week <- as.numeric(as.character(weekly_purchases$week))
lag <- weekly_purchases$week
lag <- as.factor(lag)
levels(lag)<-c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,54,55,56,57,58,59) #6weeks shift
weekly_purchases["lagged_week"] <- lag

# churn.df will be used again to solve the "arranging the weeks" issue
churn.df["lagged_week"] <- lag


##### Step 6: Churn definition - define the churn in the desired time range.

# Create the matrix with moving average and the corresponding week
weekly_purchases["id_customer_lagged_week"]<-paste(weekly_purchases$id_customer, weekly_purchases$lagged_week)
churn.df["id_customer_lagged_week"]<-paste(churn.df$id_customer,churn.df$lagged_week)
mov_avg_df <- left_join(churn.df,weekly_purchases,by="id_customer_week")                              
mov_avg_df <- mov_avg_df[,which(names(mov_avg_df) %in% c("id_customer.x","week.x","id_customer_week","moving_avg","churn"))]
colnames(mov_avg_df)[1] <- "id_customer"
colnames(mov_avg_df)[2] <- "week"
glimpse(mov_avg_df)

# Create the matrix with threshold and the corresponding week
threshold_df <- left_join(churn.df,weekly_purchases,by="id_customer_lagged_week") 
threshold_df <- threshold_df[,which(names(threshold_df) %in% c("id_customer_lagged_week","threshold"))]
colnames(threshold_df)[1] <- "id_customer_week"

# Merge these two matrices
churn_data <- left_join(mov_avg_df,threshold_df,by="id_customer_week")
churn_data <- churn_data[,-which(names(churn_data) %in% c("id_customer_week"))]

# Create churn column and define churn
churn_data["churn"] <- rep(0,dim(churn_data)[1])
churn_data$churn[which(churn_data$moving_avg < churn_data$threshold)] <- 1 # 1 means churn
churn_data["hard_churn"] <- rep(0,dim(churn_data)[1])
churn_data$hard_churn[which(churn_data$moving_avg == 0) ] <- 1

# Define all the churned customers: Customers whose moving average is under its threshold at least 3 days of the given data frame which is 6 days
churned_customers <- churn_data[which(churn_data$week %in% c(45:50)),] # The chosen timeframe for churn observation is the last 6 weeks.
churned_customers <-churned_customers %>%
  group_by(id_customer) %>%
  summarise(churn_sum=sum(churn))
churned_customers <- churned_customers[which(churned_customers$churn_sum > 2),] 

# Define hard churners:
zero_avg_customers <- churn_data[which(churn_data$week %in% c(45:50)),] 
zero_avg_customers <-zero_avg_customers %>%
  group_by(id_customer) %>%
  summarise(hard_churn_sum=sum(hard_churn))
zero_avg_customers <- zero_avg_customers[which(zero_avg_customers$hard_churn_sum > 2),] # If they have moving average equal to zero for at least 3 days of the given data frame which is 6 days

hard_churned_customers <- left_join(zero_avg_customers,churned_customers)
hard_churned_customers <- hard_churned_customers[complete.cases(hard_churned_customers),]
hard_churned_customers <- hard_churned_customers[ , -which(names(hard_churned_customers) %in% c("churn_sum"))]

# Define soft churners: The churners which are not hard churner. 
soft_churned_customers <- churned_customers[-which(churned_customers$id_customer %in% hard_churned_customers$id_customer),]

# Observe the number of churners in each churn category.
churn_percentage <- dim(churned_customers)[1]/25000*100
hard_churn_percentage <- dim(hard_churned_customers)[1]/25000*100
soft_churn_percentage <- dim(soft_churned_customers)[1]/25000*100
churn_percentage #Display churn percentage result
hard_churn_percentage #Display hard churn percentage result
soft_churn_percentage #Display soft churn percentage result

# Choose a Customer from churned customers dataset and Visualize the Purchases of Desired Customer with the Threshold 
choose.id <- data.frame(churn_data[churn_data$id_customer==hard_churned_customers$id_customer[6],]) # 1 and 10016 are seasonal customers
x11()
plot(choose.id$week,choose.id$moving_avg,col='red',type='l',ylim=c(0, max(choose.id$moving_avg,na.rm=1)), xlab='week',ylab='net sales') # na.rm omits NA values otherwise it gives error
points(choose.id$week,choose.id$threshold,col='blue',type='l')
abline(v=45, col='gray')
abline(v=50, col='gray')
legend('topright', c('Moving Average', 'Threshold'), col=c('red', 'blue'), lty=c(2,3), lwd=2)
title('Moving Average and the Threshold of Customer')


##### CUSTOMER VALUE ######
#Let`s evaluate each customer`s value through a "value score" made up of RegFM scores in order 
#to define importance of customers for COOP

#Calculation of Value scores as a weighted average of RegFM scores
period_1$value_score1<- period_1$Regularity*0.4+period_1$Frequency*0.4+period_1$Monetary*0.2
period_2$value_score2<- period_2$Regularity*0.4+period_2$Frequency*0.4+period_2$Monetary*0.2
period_3$value_score3<- period_3$Regularity*0.4+period_3$Frequency*0.4+period_3$Monetary*0.2
period_4$value_score4<- period_4$Regularity*0.4+period_4$Frequency*0.4+period_4$Monetary*0.2
period_5$value_score5<- period_5$Regularity*0.4+period_5$Frequency*0.4+period_5$Monetary*0.2
period_6$value_score6<- period_6$Regularity*0.4+period_6$Frequency*0.4+period_6$Monetary*0.2
period_7$value_score7<- period_7$Regularity*0.4+period_7$Frequency*0.4+period_7$Monetary*0.2
period_8$value_score8<- period_8$Regularity*0.4+period_8$Frequency*0.4+period_8$Monetary*0.2
period_9$value_score9<- period_9$Regularity*0.4+period_9$Frequency*0.4+period_9$Monetary*0.2

#Table compilation
cust_value= data.frame(cust$id_customer)
colnames(cust_value)[1] <- "customer_id"
cust_value= left_join(cust_value,period_1[,c(1,11)], by="customer_id" )
cust_value= left_join(cust_value,period_2[,c(1,11)], by="customer_id" )
cust_value= left_join(cust_value,period_3[,c(1,11)], by="customer_id" )
cust_value= left_join(cust_value,period_4[,c(1,11)], by="customer_id" )
cust_value= left_join(cust_value,period_5[,c(1,11)], by="customer_id" )
cust_value= left_join(cust_value,period_6[,c(1,11)], by="customer_id" )
cust_value= left_join(cust_value,period_7[,c(1,11)], by="customer_id" )
cust_value= left_join(cust_value,period_8[,c(1,11)], by="customer_id" )
cust_value= left_join(cust_value,period_9[,c(1,11)], by="customer_id" )

#Taking into consideration absent states
cust_value[is.na(cust_value)] <- 0
cust_value["avr_value_score"] = rowMeans(cust_value[,2:10])

#Division of churned customers into soft and hard churners
churned_customers["soft"]= ifelse(churned_customers$id_customer %in% soft_churned_customers$id_customer, 1, 0)
churned_customers["hard"]= ifelse(churned_customers$id_customer %in% hard_churned_customers$id_customer, 1, 0)

#Creation of Churned customers table
colnames(churned_customers)[1] <- "customer_id"
churned_customers= left_join(churned_customers,cust_value[,c(1,11)], by="customer_id" )

#Categorization of churners according to their value scores in order to define following actions and spendings
churned_customers$label[which(churned_customers$avr_value_score>= 2)]="High Value"
churned_customers$label[which (between(churned_customers$avr_value_score, 1.3, 1.99))]="Medium Value"
churned_customers$label[which(churned_customers$avr_value_score< 1.3)]="Low Value"

#Numerical composition of categories
churned_customers$label= as.factor(churned_customers$label)
table(churned_customers$label)