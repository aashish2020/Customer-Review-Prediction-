#####Loading Library####
library(dplyr)
library(forecast)
library(leaps)
library(tidyverse)
library(caret)
library(corrplot)
library(nnet)
library(MASS)
library(randomForest)
library(lubridate)
library(pROC)
library(gains)

####Loading datsets####
orders <- read.csv("olist_orders_dataset.csv",header = T,stringsAsFactors = F)
customers <- read.csv("olist_customers_dataset.csv",header = T)
order_reviews <- read.csv("olist_order_reviews_dataset.csv",header = T)
order_payments <- read.csv("olist_order_payments_dataset.csv",header = T)
order_items_details <- read.csv("olist_order_items_dataset.csv",header = T)
sellers <- read.csv("olist_sellers_dataset.csv",header = T)
geolocation <- read.csv("olist_geolocation_dataset.csv",header = T)
products <- read.csv("olist_products_dataset.csv",header = T)

####Data preparation####
###Merging datasets###
orders <- orders[orders$order_status=="delivered",]
orders_all <- merge(orders,customers,by="customer_id",all.x=T)
orders_all$order_purchase_timestamp <- 
  as.POSIXct(orders_all$order_purchase_timestamp,"%Y-%m-%d %H:%M:",tz="GMT")
orders_all$order_delivered_customer_date<- 
  as.POSIXct(orders_all$order_delivered_customer_date,"%Y-%m-%d %H:%M:",tz="GMT")
orders_all$order_approved_at<- 
  as.POSIXct(orders_all$order_approved_at,"%Y-%m-%d %H:%M:",tz="GMT")
orders_all$order_estimated_delivery_date<- 
  as.POSIXct(orders_all$order_estimated_delivery_date,"%Y-%m-%d %H:%M:",tz="GMT")
orders_all <- merge(orders_all,order_reviews,by="order_id")
orders_all <- orders_all[!duplicated(orders_all$order_id),]
orders_all <- merge(orders_all,order_payments,by="order_id",all.x = T)
orders_all <- orders_all[!duplicated(orders_all$order_id),]
orders_all <- merge(orders_all,order_items_details,by="order_id",all.X=T)
orders_all <- orders_all[!duplicated(orders_all$order_id),]
orders_all <- merge(orders_all,sellers,by="seller_id",all.X=T)
orders_all <- merge(orders_all,products,by="product_id",all.X=T)
rm(list=setdiff(ls(), "orders_all"))
write.csv(orders_all,file="Orders_merged.csv",row.names = F)

####Loading the combined dataset####
setwd("D:/Fall-2018/BAR-Sourav/Project-Customer Churn/brazilian-ecommerce/")
orders_all <- read.csv("Orders_merged.csv",header = T,stringsAsFactors = F)
orders_all$order_purchase_timestamp <- 
  as.POSIXct(orders_all$order_purchase_timestamp,"%Y-%m-%d %H:%M:",tz="GMT")
orders_all$order_delivered_customer_date<- 
  as.POSIXct(orders_all$order_delivered_customer_date,"%Y-%m-%d %H:%M:",tz="GMT")
orders_all$order_approved_at<- 
  as.POSIXct(orders_all$order_approved_at,"%Y-%m-%d %H:%M:",tz="GMT")
orders_all$order_estimated_delivery_date<- 
  as.POSIXct(orders_all$order_estimated_delivery_date,"%Y-%m-%d %H:%M:",tz="GMT")
orders_all$order_delivered_month <- 
  format(as.Date(orders_all$order_estimated_delivery_date),"%Y-%m")
orders_all$del_time <- difftime(orders_all$order_delivered_customer_date,
                                orders_all$order_purchase_timestamp,
                                units="days")

orders_all$del_time[is.na(orders_all$del_time)] <- 0
orders_all$del_time_bucket <- ifelse(orders_all$del_time < 5,"<5",
                                     ifelse(orders_all$del_time<10,"5-10",
                                            ifelse(orders_all$del_time < 20,"10-20",
                                                   ifelse(orders_all$del_time<40,"20-40",">40"))))   

####Summarisation and data exploration####
no_of_orders_month <- orders_all %>% group_by(order_delivered_month) %>% 
  summarise(no=n(),rev_in_K=sum(price)/1000,del_time=mean(del_time))
no_of_orders_location <- orders_all %>% group_by(customer_state) %>% 
  summarise(no=n(),rev_in_K=sum(price)/1000,del_time=mean(del_time)) 
no_of_orders_location_month <- orders_all %>% group_by(customer_state,order_delivered_month) %>% 
  summarise(no=n(),rev_in_K=sum(price)/1000,del_time=mean(del_time)) 
no_of_orders_rs <-orders_all %>% group_by(review_score,order_delivered_month) %>% 
  summarise(no=n()) 
no_of_orders_rs_p <-orders_all %>% group_by(review_score,product_category_name) %>% 
  summarise(no=n()) 
no_of_orders_pt <- orders_all %>% group_by(payment_type) %>% 
  summarise(no=n(),rev_in_K=sum(price)/1000,del_time=mean(del_time))
no_of_orders_seller <- orders_all %>% group_by(seller_id) %>% 
  summarise(no=n(),rev_in_K=sum(price)/1000,del_time=mean(del_time))
no_of_orders_seller <- no_of_orders_seller[order(-no_of_orders_seller$rev_in_K),]
no_of_orders_seller_1 <- no_of_orders_seller[order(-no_of_orders_seller$no),]
no_of_orders_photos <- orders_all %>% group_by(product_photos_qty) %>% 
  summarise(no=n(),rev_in_K=sum(price)/1000,del_time=mean(del_time))
no_of_orders_pcm <- orders_all %>% group_by(product_category_name) %>% 
  summarise(no=n(),rev_in_K=sum(price)/1000,del_time=mean(del_time))
no_of_orders_nl <- orders_all %>% group_by(product_name_lenght) %>% 
  summarise(no=n(),rev_in_K=sum(price)/1000,del_time=mean(del_time))


####Data preparation for prediction####
###Selecting varaibles and dropping columns###
drops <- c("order_status","order_delivered_carrier_date","order_purchase_timestamp",
           "order_approved_at","customer_zip_code_prefix","customer_unique_id",
           "customer_city","review_id","review_comment_title","review_comment_message",
           "review_creation_date","review_answer_timestamp","order_item_id","seller_zip_code_prefix",
           "seller_city","product_category_name","order_delivered_month","product_id","seller_id",
           "order_id","customer_id","order_delivered_customer_date","order_estimated_delivery_date",
           "customer_state","seller_state","payment_sequential","payment_value","del_time_bucket","payment_type","product_weight_g",
           "product_height_cm","product_length_cm","product_width_cm","payment_installments")
orders_all_1 <- orders_all[ , !(names(orders_all) %in% drops)]

#####Creating partition sets####
orders_all_1[is.na(orders_all_1)] <- 0
orders_all_1$del_time <- as.numeric(orders_all_1$del_time)

set.seed(13)
train.index <- createDataPartition(orders_all_1$review_score, p = 0.8, list = FALSE)
train.df <- orders_all_1[train.index, ]
valid.df <- orders_all_1[-train.index, ]
orders_all_1[is.na(orders_all_1)] <- 0
cor_mat <- cor(orders_all_1)
corrplot(cor_mat, method="color")###Red tells us neg correl and blue gives us pos correl ,intentisty of color gives us the strenght of correl

##We decide that none of the variables are correlated strongly##

####Linear regression####

lm_1 <- lm(review_score~.,data=train.df)
summary(lm_1)

##We didnt use correl matrix because there were categorical variables and correl matrix causes multi colinearity##
####Reg search####
search <- regsubsets(review_score ~ ., data = train.df, nbest = 1, nvmax = dim(orders_all_1)[2],
                     method = "exhaustive")
sum <- summary(search)##Giving us that 7 predictors are the best model##
a <- predict(lm_1,valid.df)###Predicting on the validation dataset##
accuracy(a, valid.df$review_score)####RMSE too high,not so good model

#####since direct variables were of less correlation,we will try some derivative variables and try running to see if there's an improvememnt ####
orders_all_2 <- orders_all
orders_all_2$est_del_time<- difftime(orders_all$order_estimated_delivery_date,
                                     orders_all$order_approved_at,
                                     units="days")
orders_all_2$delta_time<- orders_all_2$est_del_time-orders_all_2$del_time
orders_all_2$Late <- ifelse(orders_all_2$delta_time<0,1,0)
orders_all_2$total_price <- orders_all_2$price+orders_all_2$freight_value
orders_all_2$freight_ratio <- orders_all_2$freight_value/orders_all_2$price
orders_all_2$purchase_day_of_week <- wday(orders_all_2$order_approved_at)
orders_all_2 <- orders_all_2[ , !(names(orders_all_2) %in% drops)]
orders_all_2$del_time <- as.numeric(orders_all_2$del_time)
orders_all_2$est_del_time <- as.numeric(orders_all_2$est_del_time)
orders_all_2$delta_time <- as.numeric(orders_all_2$delta_time)
orders_all_2[is.na(orders_all_2)] <- 0
orders_all_2 <- orders_all_2[ , -which(names(orders_all_2) %in% c("price"))]
cor_mat_2 <- cor(orders_all_2)
corrplot(cor_mat_2, method="color")

#####Partition the dataset########Using the new derived metrics####

set.seed(13)
train.index_1 <- createDataPartition(orders_all_2$review_score, p = 0.8, list = FALSE)
train_1.df <- orders_all_2[train.index_1, ]
valid_1.df <- orders_all_2[-train.index_1, ]

lm_2 <- lm(review_score~.,data=train_1.df,na.action = na.omit)
summary(lm_2)

a_1 <- (predict(lm_2,valid_1.df))###Predicting on the validation dataset##

accuracy(a_1, valid_1.df$review_score)####RMSE too high,not so good model###





#####Low Med High####
orders_all_2$review_score_1 <- ifelse(orders_all_2$review_score<3,0,1)
orders_all_2$review_score <- orders_all_2$review_score_1  
orders_all_2 <- orders_all_2[,1:12]
####Next model####--Logistic model--####
set.seed(13)
train.index_1 <- createDataPartition(orders_all_2$review_score, p = 0.8, list = FALSE)
train_1.df <- orders_all_2[train.index_1, ]
valid_1.df <- orders_all_2[-train.index_1, ]

orders_all_2$review_score <- (as.factor(orders_all_2$review_score))###Leveling by giving reference###
logit.reg <- glm(review_score~., data = train_1.df) 
summary(logit.reg)

###fitting the model and checking accuracy on the training model using a confusion matrix##
logit.reg.pred <- as.data.frame(predict(logit.reg, valid_1.df[, -1], type = "response"))
colnames(logit.reg.pred)[1] <- "p"
logit.reg.pred$class <- ifelse(logit.reg.pred$p>0.5,1,0)
cm <- table(logit.reg.pred$class,valid_1.df$review_score)
caret::confusionMatrix(cm)


########Next model -linear discriminant analysis#####
#Running linear discriminant analysis
lda <- lda(review_score~.,data=train_1.df)

###Checking model on test dataset##
pred_lda <- predict(lda, valid_1.df)

###Checking accuracy using a confusion matrix##Accuracy is again found to be 80%##
cm_lda <- (table(pred_lda$class, valid_1.df$review_score))
caret::confusionMatrix(cm_lda)

####Next model-random forest ####
##First convert the dependant to factors##
valid_1.df$review_score <- as.factor(valid_1.df$review_score)
train_1.df$review_score <- as.factor(train_1.df$review_score)

##Running randomForest on training##
rfm <- randomForest(review_score~., train_1.df)

###Prediting on the validation dataset and checking for accuracy###---90%
pred_rfm <-(predict(rfm,valid_1.df))
pred_rfm_p <- as.data.frame(predict(rfm, valid_1.df, type = "prob"))
cm_rfm <- (table(pred_rfm, valid_1.df$review_score))
caret::confusionMatrix(cm_rfm)
####Out of all the models random forest gives us the best result so we choose this to explain the model####

###Plotting roc curve for random forest###
r <- roc(valid_1.df$review_score,pred_rfm_p$`1`)
plot.roc(r)
auc(r)

###The area under the curve is found to be 0.7103
