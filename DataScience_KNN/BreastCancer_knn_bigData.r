install.packages("class")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("ggplot2")
library(class)
library(rpart)
library(rpart.plot)
library(ggplot2)

setwd("C:\\Users\\Administrator\\Documents\\dataScienceDay1")

raw_data_big<- read.table("BCD_Large.data", sep = ",", stringsAsFactors = FALSE)

table(is.na(raw_data_big))

names(raw_data_big)<- c("ID","Diagnosis","Radius","Texture","Perimeter","Area","Smoothness","Compactness","Concavity","Concave Points","Symmetry","Fractal Dimensions","Radius_SE","Texture_SE","Perimeter_SE","Area_SE","Smoothness_SE","Compactness_SE","Concavity_SE","Concave Points_SE","Symmetry_SE","Fractal Dimensions","Radius_worst","Texture_worst","Perimeter_worst","Area_worst","Smoothness_worst","Compactness_worst","Concavity_worst","Concave Points_worst","Symmetry_worst","Fractal Dimensions_worst")
act_data_big<-raw_data_big[,-1]
results_big<-act_data_big[,1]
train_data_big<-act_data_big[,-1]

#Normalisazation
normalise<- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalised_train_big<-as.data.frame(lapply(train_data_big,normalise))

k_value<-floor(sqrt(469))

sim_big<-knn(normalised_train_big[1:469,],normalised_train_big[470:569,],results_big[1:469],k_value) 
table(results_big[470:569], sim_big)

errorSum<-c()
for(i in seq(1,100)){
  sim<-knn(normalised_train_big[1:469,],normalised_train_big[470:569,],results_big[1:469],i) 
  
  tbl<-table(results_big[470:569], sim_big)
  errorSum<-c(errorSum,(tbl[1,2]+tbl[2,1]))
}

errorTable_big<- data.frame(seq(1:100),errorSum)

ggplot(data= errorTable_big,aes(x=errorTable_big[,1],y=errorTable_big[,2])) + geom_smooth(method = "loess") + geom_line() + ggtitle("Sum of errors against K-value - Big Data") + xlab("K - Value") + ylab("Sum of Errors")

k_best<-which(errorTable_big[,2]==min(errorTable_big[,2]))

sim<-knn(normalised_train_big[1:469,],normalised_train_big[470:569,],results_big[1:469],k_best) 

table(results_big[470:569], sim_big)
