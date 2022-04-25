
# Load the library --------------------------------------------------------

library(tidyverse)
library(scales)
library(caret)

# Import the dataset ------------------------------------------------------

transactions <- read_csv('data/transactions.csv')


# Look at the data structure ----------------------------------------------

glimpse(transactions)


# Convert the date type ---------------------------------------------------

transactions$date <- as.Date(transactions$date,format="%d/%m/%Y")
transactions$industry <- as.factor(transactions$industry)
transactions$location <- as.factor(transactions$location)

# Check the data type again -----------------------------------------------

glimpse(transactions)


# Look at the first few rows ----------------------------------------------

head(transactions)


# Check the missing values for each variable ------------------------------

cbind(
  lapply(
    lapply(transactions, is.na),sum)
)


# Look at the statistical description -------------------------------------

summary(transactions)


# EDA with data visualisation ---------------------------------------------

# Distribution for date
ggplot(data=transactions)+
  geom_bar(mapping=aes(x=date),fill='steelblue',color='grey')+
  labs(title="Number of transactions for each month",
       x="",
       y="Number of transactions")

# Distribution for industry (used in report)
ggplot(data=transactions)+
  geom_bar(mapping=aes(x=reorder(industry,industry,function(x)-length(x))),fill='forestgreen',color='grey')+
  labs(
    caption ="Figure 1. Number of transactions for each industry",
       x="Industry",
       y="Number of transactions")+
  theme_bw()+
  theme(plot.caption = element_text(size=11,hjust=0.5,face="bold"))

# Distribution for location (used in report)
ggplot(data=transactions,mapping=aes(x=fct_reorder(location,n)))+
  geom_bar(mapping=aes(x=reorder(location,location,function(x)-length(x))),fill='skyblue',color='grey')+
  labs(caption="Figure 2. Number of transactions for each location",
       x="Location",
       y="Number of transactions")+
  theme_bw()+
  theme(plot.caption = element_text(size=11,hjust=0.5,face="bold"))
  

# Distribution for monthly_amount (right skewed)
ggplot(data=transactions)+
  geom_histogram(mapping=aes(x=monthly_amount),fill='skyblue',color='grey')+
  # xlim(c(0,1000000))+
  labs(title="Distribution for monthly amount",
       x="Monthly amount",
       y="Number of transactions")

# Distribution for monthly_amount (right skewed) - log10
ggplot(data=transactions)+
  geom_histogram(mapping=aes(x=monthly_amount),fill='skyblue',color='grey')+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  labs(
      title  = "Figure 1. Distribution for monthly amount",
       x="Monthly amount",
       y="Number of transactions")+
  theme_bw()


# Monthly amount over previous years
ggplot(data=transactions)+
  geom_point(aes(x=date,y=monthly_amount),color="navy")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  labs(caption ="Figure 5. Monthly transaction amount from Jan 2013 to Nov 2016",
       x="",
       y="Monthly transactions amount"
       )+
  theme_bw()+
  theme(plot.caption = element_text(size=11,hjust=0.5,face="bold"))

# Relationship between Industry and Monthly_amount (used in report)
ggplot(data=transactions,mapping=aes(x=reorder(industry,-monthly_amount,median),y=monthly_amount))+
  geom_boxplot(color='forestgreen',outlier.shape = 1)+
  stat_summary(fun.y=mean, geom="point", size=1.5, color="sienna1")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                 labels = trans_format("log10", math_format(10^.x)))+
  labs(caption = "Figure 3. Monthly transaction amount for each industry",
       y="Monthly transaction amount",
       x="Industry"
       )+
  theme_bw()+
  theme(plot.caption = element_text(size=11,hjust=0.5,face="bold"))

# Relationship between Location and Monthly_amount (used in report) 
ggplot(data=transactions,mapping=aes(x=reorder(location,-monthly_amount,median),y=monthly_amount))+
  geom_boxplot(color='skyblue',outlier.shape = 1)+
  stat_summary(fun.y=mean, geom="point", size=1.5, color="sienna1")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  labs(caption ="Figure 4. Monthly transaction amount for each location",
                y="Monthly transaction amount",
                x="Location")+
  theme_bw()+
  theme(plot.caption = element_text(size=11,hjust=0.5,face="bold"))

# Monthly amount by location and industry over time (love this!)-----------
ggplot(data=transactions)+
  geom_point(mapping=aes(x=date,y=monthly_amount,color=location))+
  facet_wrap(~ industry,nrow=3)


# Basic Model Fitting -----------------------------------------------------

# Created an aggregated data set and a line plot of variable monthly_amount

ave_month <- 
  transactions %>% 
  group_by(date,industry,location) %>% 
  summarise(monthly_amount=mean(monthly_amount))
ave_month


# Create a line plot of the variable monthly_amount for industry = 1 and location = 1
loc1_ind1 <- ave_month %>% 
  filter(location==1 & industry==1)

  ggplot(loc1_ind1)+
  geom_line(mapping=aes(x=date,y=monthly_amount),size=1)+
  labs(caption ="Figure 5. Monthly amount for customers in industry 1 and location 1",
         y="Monthly amount",
         x=" ")+
    theme_bw()+
    theme(plot.caption = element_text(size=11,hjust=0.5,face="bold"))  
  
  
loc1_ind1 <- 
  loc1_ind1 %>% 
  data.frame() %>% 
  mutate(month=as.factor(format(date,format="%m"))) %>% 
  mutate(id=row_number())
 
loc1_ind1


# Fit the model -----------------------------------------------------------


# Training and test data split

train_test_split <- c(1:36)
training_data <- loc1_ind1[train_test_split,] #this is used to build the training model
test_data <- loc1_ind1[-train_test_split,] #this is used to test the final model only

# Fit the model1 (without polynomials)
model1 <- lm (monthly_amount ~ month + id,
                      data=training_data)
summary(model1)

training_data %>%
  mutate(prediction=predict(model1,data=training_data)) %>%
  ggplot()+
  geom_line(mapping=aes(x=date,y=monthly_amount),color="grey")+
  geom_line(mapping=aes(x=date,y=prediction),color="blue")

prediction1 <- model1 %>% predict(test_data)


#calculate RMSE,MAE and MAPE (MAE is used)
RMSE(prediction1,test_data$monthly_amount)
MAE(prediction1,test_data$monthly_amount)
mean(abs((test_data$monthly_amount-prediction1)/test_data$monthly_amount)) * 100

# Fit the model2 (with polynomials)
model2 <- lm (monthly_amount ~ month + I(id^2),
              data=training_data)
summary(model2)

summary(model2)$adj.r.squared

training_data %>%
  mutate(prediction=predict(model2,data=training_data)) %>%
  ggplot()+
  geom_line(mapping=aes(x=date,y=monthly_amount),color="grey")+
  geom_line(mapping=aes(x=date,y=prediction),color="blue")


# calculate the predicted monthly_amount in Dec 2016 
mth_dec16 <- 
  model2$coefficients[1]+model2$coefficients[12]+model2$coefficients[13]*48*4

Dec_16 <- data.frame("2016-12-01","1","1","02",48,mth_dec16)
names(Dec_16) <- c("date","industry","location","month","id","prediction")
rownames(Dec_16) <- 48

# create a plot for Dec 16 predicted value
ggplot(loc1_ind1)+
  geom_line(mapping=aes(x=date,y=monthly_amount),size=1)+
  geom_point(mapping=aes(x=as.Date("2016-12-01"),y=mth_dec16),color="red",size=3)+
  labs(caption ="Figure 6. Predicted amount for December 2016 (industry=1 and location=1)",
       y="Monthly amount",
       x=" ")+
  theme_bw()+
  theme(plot.caption = element_text(size=11,hjust=0.5,face="bold"))  


#prediction for test data(model2,data=test_data) and difference with actual

prediction2 <- model2 %>% predict(test_data)
test_data %>%
  mutate(prediction=prediction2,difference=prediction2-monthly_amount)


#calculate RMSE,MAE and MAPE (MAE is used)
RMSE(prediction2,test_data$monthly_amount)
MAE(prediction2,test_data$monthly_amount)
mean(abs((test_data$monthly_amount-prediction2)/test_data$monthly_amount)) * 100


# Advanced Model Fitting --------------------------------------------------

# Write the function to run all combination

output=data.frame()

train_test_split <- c(1:36)

for (i in seq(10)){
  
  for (j in seq(10)){
    
    loc_ind <- ave_month %>% 
      filter(location==i & industry==j) %>% 
      data.frame() %>% 
      mutate(month=as.factor(format(date,format="%m"))) %>% 
      mutate(id=row_number())
    
    if (length(unique(loc_ind$date))>=36){
      training_data <- loc_ind[train_test_split,]
      test_data <- loc_ind[-train_test_split,]      
      model <- lm (monthly_amount ~ month + I(id^2),
                   data=training_data)
      a=summary(model)$adj.r.squared
      prediction2=predict(model,test_data)
      b=MAE(prediction2,test_data$monthly_amount)
      p=model$coefficients[1]+model$coefficients[12]+model$coefficients[13]*48*48
      
      combined=c(i,j,a,b,p)
      output=rbind(output,combined)
      colnames(output) <- c("location","industry","Adjusted R^2","MAE","predict_Dec16")
    }
  }
}

output

# Select the 2 worst locations and industries by using Adjusted R squared

output[order(output$`Adjusted R^2`),][1:2,]

# Select the 2 worst locations and industries by using MAE
output[order(-output$MAE),][1:2,]



# Create graph for location 5 and industry 3

loc5_ind3 <- ave_month %>% 
  filter(location==5 & industry==3)

ggplot(loc5_ind3)+
  geom_line(mapping=aes(x=date,y=monthly_amount),size=1)+
  labs(caption ="Figure 7. Monthly amount for customers in location 5 and industry 3",
       y="Monthly amount",
       x=" ")+
  theme_bw()+
  theme(plot.caption = element_text(size=11,hjust=0.5,face="bold"))  

# Create graph for location 8 and industry 3

loc8_ind3 <- ave_month %>% 
  filter(location==8 & industry==3)

ggplot(loc8_ind3)+
  geom_line(mapping=aes(x=date,y=monthly_amount),size=1)+
  labs(caption ="Figure 8. Monthly amount for customers in location 8 and industry 3",
       y="Monthly amount",
       x=" ")+
  theme_bw()+
  theme(plot.caption = element_text(size=11,hjust=0.5,face="bold"))  






