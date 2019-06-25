# HW4
rm(list = ls())
library(readxl)
df <- read_excel("LTV Service Data.xlsx")
#-----------------数据处理---------------
#1.phone: NA<-0
#2.revenue，unit，delivery三列单独算每个人的均值（不配对），再算group的均值
#4.start date删掉非那三个月的ID
df$`Number of Customer Service Calls`[is.na(df$`Number of Customer Service Calls`)] <- 0

ID <- unique(df$`Customer ID`)
retention <- data.frame(matrix(NA,length(ID),7))
colnames(retention) <- c("ID", "Months", "Start", "Revenue", "Unit", "Delivery", "Call")
for(i in 1:length(ID)){
        retention$ID[i] <- ID[i]
        t <- 1
        temp_date <- NULL
        temp_rev <- NULL
        temp_unit <- NULL
        temp_deli <- NULL
        temp_call <- NULL
        for(j in 1:nrow(df)){
                if(df$`Customer ID`[j] == ID[i]){
                        temp_date[t] <- df$YearMonth[j]
                        temp_rev[t] <- df$Revenue[j]
                        temp_unit[t] <- df$Units[j]
                        temp_deli[t] <- df$`Number of Deliveries`[j]
                        temp_call[t] <- df$`Number of Customer Service Calls`[j]
                        t <- t+1
                }
        }
        temp_date <- sort(temp_date)
        retention$Start[i] <- temp_date[1]
        retention$Months[i] <- length(temp_date)
        
        retention$Revenue[i] <- mean(temp_rev[!is.na(temp_rev)])
        retention$Unit[i] <- mean(temp_unit[!is.na(temp_unit)])
        retention$Delivery[i] <- mean(temp_deli[!is.na(temp_deli)])
        retention$Call[i] <- mean(temp_call[!is.na(temp_call)])
}

retention <- retention[retention$Start == "201601" |retention$Start == "201602" |retention$Start == "201603", ]
# write.csv(retention, file = "retention.csv")
summary(retention)
#------------------需要的变量-------------
#1.retention rate 每个月的
#2.discount rate 16-18平均，再平均到每个月
#3.M:average gross margin = revenue - delivery - product, 每个人的平均
        # average revenue 先每个人自己的，再所有人的均值（三组）
        # deliver, product同理
#4.c:call cost 同理
remove(list = ls())
retention <- read.csv("retention.csv",header = T)

avg_revenue <- aggregate(Revenue ~ Start, retention, mean)
avg_unit <- aggregate(Unit ~ Start, retention, mean)
avg_months <- aggregate(Months ~ Start, retention, mean)
avg_delivery <- aggregate(Delivery ~ Start, retention, mean)
avg_call <- aggregate(Call ~ Start, retention, mean)

variables <- cbind(avg_revenue,avg_unit[,2],avg_months[,2],avg_delivery[,2],avg_call[,2])
colnames(variables) <- c("Start_date","Avg_revenue","Avg_unit","Avg_stay_months","Avg_delivery","Avg_call")

#write.csv(variables, file = "variables_data.csv")

##---------------算retention rate---------------
##----201601组------
remove(list = ls())
retention <- read.csv("retention.csv",header = T)

stay_Jan <- aggregate(ID ~ Months, retention[retention$Start=="201601",], length)
colnames(stay_Jan) <- c("leave_after_months", "num_of_people")
sum(stay_Jan$num_of_people)
all_people <- sum(stay_Jan$num_of_people)
reten_rate_Jan <- NULL

for(i in 1:max(stay_Jan$leave_after_months)){
        reten_rate_Jan[i] <- 1
        for(j in 1:nrow(stay_Jan)){
                if(stay_Jan$leave_after_months[j]==i){
                        reten_rate_Jan[i] <- (all_people - stay_Jan$num_of_people[j])/all_people
                        all_people <- all_people - stay_Jan$num_of_people[j]
                }
        }
}

#-------201602组--------
stay_Feb <- aggregate(ID ~ Months, retention[retention$Start=="201602",], length)
colnames(stay_Feb) <- c("leave_after_months", "num_of_people")
sum(stay_Feb$num_of_people)
all_people <- sum(stay_Feb$num_of_people)
reten_rate_Feb <- NULL

for(i in 1:max(stay_Feb$leave_after_months)){
        reten_rate_Feb[i] <- 1
        for(j in 1:nrow(stay_Feb)){
                if(stay_Feb$leave_after_months[j]==i){
                        reten_rate_Feb[i] <- (all_people - stay_Feb$num_of_people[j])/all_people
                        all_people <- all_people - stay_Feb$num_of_people[j]
                }
        }
}
#-------201603组--------
stay_Mar <- aggregate(ID ~ Months, retention[retention$Start=="201603",], length)
colnames(stay_Mar) <- c("leave_after_months", "num_of_people")
sum(stay_Mar$num_of_people)
all_people <- sum(stay_Mar$num_of_people)
reten_rate_Mar <- NULL

for(i in 1:max(stay_Mar$leave_after_months)){
        reten_rate_Mar[i] <- 1
        for(j in 1:nrow(stay_Mar)){
                if(stay_Mar$leave_after_months[j]==i){
                        reten_rate_Mar[i] <- (all_people - stay_Mar$num_of_people[j])/all_people
                        all_people <- all_people - stay_Mar$num_of_people[j]
                }
        }
}

### 整理Jan
Jan <- data.frame(matrix(NA,max(stay_Jan$leave_after_months),3))
colnames(Jan) <- c("month", "r", "num_of_people_left")
Jan$month <- 1:max(stay_Jan$leave_after_months)
Jan$r <- reten_rate_Jan
all_people <- sum(stay_Jan$num_of_people)
for(i in 1:nrow(Jan)){
        for(j in 1:nrow(stay_Jan)){
                if(stay_Jan$leave_after_months[j]==i){
                        Jan$num_of_people_left[i] = all_people - stay_Jan$num_of_people[j]
                        all_people <- all_people - stay_Jan$num_of_people[j]
                        break
                }else{
                        Jan$num_of_people_left[i] = Jan$num_of_people_left[i-1]
                }
        }
}

### 整理Feb
Feb <- data.frame(matrix(NA,max(stay_Feb$leave_after_months),3))
colnames(Feb) <- c("month", "r", "num_of_people_left")
Feb$month <- 1:max(stay_Feb$leave_after_months)
Feb$r <- reten_rate_Feb
all_people <- sum(stay_Feb$num_of_people)
for(i in 1:nrow(Feb)){
        for(j in 1:nrow(stay_Feb)){
                if(stay_Feb$leave_after_months[j]==i){
                        Feb$num_of_people_left[i] = all_people - stay_Feb$num_of_people[j]
                        all_people <- all_people - stay_Feb$num_of_people[j]
                        break
                }else{
                        Feb$num_of_people_left[i] = Feb$num_of_people_left[i-1]
                }
        }
}
### 整理Mar
Mar <- data.frame(matrix(NA,max(stay_Mar$leave_after_months),3))
colnames(Mar) <- c("month", "r", "num_of_people_left")
Mar$month <- 1:max(stay_Mar$leave_after_months)
Mar$r <- reten_rate_Mar
all_people <- sum(stay_Mar$num_of_people)
for(i in 1:nrow(Mar)){
        for(j in 1:nrow(stay_Mar)){
                if(stay_Mar$leave_after_months[j]==i){
                        Mar$num_of_people_left[i] = all_people - stay_Mar$num_of_people[j]
                        all_people <- all_people - stay_Mar$num_of_people[j]
                        break
                }else{
                        Mar$num_of_people_left[i] = Mar$num_of_people_left[i-1]
                }
        }
}

# write.csv(Jan, "Jan_retention_rate.csv")
# write.csv(Feb, "Feb_retention_rate.csv")
# write.csv(Mar, "Mar_retention_rate.csv")

