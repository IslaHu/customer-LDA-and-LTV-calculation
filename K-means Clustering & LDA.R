rm(list=ls())
df0 <- read.csv("Museum Data.csv", header = T)
#Q1
# change all N/A into 0
df <- df0
df[is.na(df)] <- 0
# change F to 1 and M to 2 in gender column
df$gender <- as.numeric(factor(df$gender, levels = c("F","M"), labels = c(1,2)))
# change M to 1 and S to 2 in marital_stats column
df$marital_status <- as.numeric(factor(df$marital_status, levels = c("M","S"), labels = c(1,2)))
# change A to 1 and S to 2 in dwelling_type column
df$dwelling_type <- as.numeric(factor(df$dwelling_type, levels = c("A","S"), labels = c(1,2)))

# correlations
library(corrplot)
cor(df)
corrplot(cor(df[,-1]))

df1 <- df0[df0$Target_Customer == 1,]
df2 <- df[df$Target_Customer ==1,]
# univariate visual analysis of interesting variables
plot(df1$gender, main = "Gender Distribution")
hist(df2$age, main = "Age Distribution", xlab = "Age")
hist(df2$median_income, main = "Income Distribution", xlab = "Income", breaks = 24)
# hist(df$num_in_hhld)
hist(df2$num_of_children, main = "Number of Children Distribution", 
     xlab = "Number of Children", breaks = 8)
plot(df1$marital_status, main = "Marital Status")
plot(df1$dwelling_type, main = "Dwelling Types")
hist(df2$pets)
hist(df2$sports)
hist(df2$casino_gambling)
hist(df2$family)
hist(df2$music)
hist(df2$travel)
hist(df2$art_craft)
hist(df2$cars)
hist(df2$num_in_hhld, main = "Number of People in Household", xlab = "Number of People")



# Q2
# Cluster the individuals using the K-Means algorithm 
# based on demographic variables. 
# You must identify at least 3 clusters (segments) 
# based on at least 3 demographic variables. 
# This will involve running multiple cluster analysis 
# which result in a final “model” chosen. 
# Explain how you ended up with this final “model”. 
# Explain why you did or did not normalize the demographic variables.
require(BBmisc)
library(cluster) 
library(fpc)

# use age, number of children, marital status, dwelling type to segment current customers
toclust <- normalize(df[df$Target_Customer==1, c(3,6,7,8)])
set.seed(123456) 
km1 = kmeans(toclust,4,nstart = 25)
mean(km1$withinss)
clusplot(toclust, km1$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
km1$centers

# intergrate the cluster output into LDA input
toLDA <- df[df$Target_Customer==1, 9:16]
toLDA$cluster <- km1$cluster

# LDA
library(MASS)
library(caret)
km2 = lda(cluster ~ pets + sports + casino_gambling + family + music + travel
          + art_craft + cars, toLDA)
t <- table(predict(km2)$class,toLDA$cluster)
confusionMatrix(t)
## accuracy = 0.4957
km2
plot(km2)

# Q3
# predict the potential customers using demegraphic variables
library(flexclust)
km3 <- as.kcca(km1, toclust)
toPredict <- normalize(df[df$Target_Customer==0, c(3,6,7,8)])
result <- predict(km3, toPredict)
table(result)

