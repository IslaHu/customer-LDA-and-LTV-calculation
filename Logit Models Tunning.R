library(ggplot2)
library(mlogit)      
library(data.table)      
library(aod);

phone_data <- read.csv('Phone Data.csv')

phone_logit <- glm(churn~customer.service.calls + total.intl.charge + total.night.charge + total.eve.charge + total.day.charge, data = phone_data, family = 'binomial')

wald.test(b = coef(phone_logit), Sigma = vcov(phone_logit), Terms = 2:6)


metro_data <- read.csv('Metro Data.csv')

metro_data[metro_data$CHOICE == 0, 'CHOICE'] <- 'unknown'
metro_data[metro_data$CHOICE == 1, 'CHOICE'] <- 'Train'
metro_data[metro_data$CHOICE == 2, 'CHOICE'] <- 'SM'
metro_data[metro_data$CHOICE == 3, 'CHOICE'] <- 'Car'

colnames(metro_data) <- c('Group',"ID",'First','Who','Age','Male','Income','GA','av.Train','av.Car','av.SM','TT.Train','CO.Train','HE.Train','TT.SM','CO.SM','HE.SM','Seats.SM','TT.Car','CO.Car','CHOICE')

metro_data$HE.Car <- NA

metro_data['Seats.SM'] <- NULL

choice <- metro_data['CHOICE']
metro_data['CHOICE'] <- metro_data['HE.Car']

metro_data['HE.Car'] <- choice

colnames(metro_data) <- c('Group',"ID",'First','Who','Age','Male','Income','GA','av.Train','av.Car','av.SM','TT.Train','CO.Train','HE.Train','TT.SM','CO.SM','HE.SM','TT.Car','CO.Car','HE.Car','CHOICE')

metro.logit <- mlogit.data(metro_data,varying = 9:20, shape = "wide", choice = "CHOICE")

m <- mlogit(CHOICE ~ TT + CO | Male + Who + Age + First + Income, data = metro.logit)

library(caret)
library(rpart)

fit <- rpart(CHOICE~., data = metro.logit, method = 'class', control=rpart.control(xval=0, minsplit=1000))
churn.pred = predict(fit, metro.logit, type="class")
churn.actual = metro.logit$CHOICE

confusion.matrix = table(churn.pred, churn.actual)
