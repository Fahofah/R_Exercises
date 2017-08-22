install.packages("class")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("ggplot2")
library(class)
library(rpart)
library(rpart.plot)
library(ggplot2)

setwd("C:\\Users\\Administrator\\Documents\\dataScienceDay1")

raw_data<- read.table("BCD_Small.data", sep = ",", fill= TRUE, stringsAsFactors = FALSE)
table(is.na(raw_data))

names(raw_data)<- c("r_ID","ID","Diagnosis","Radius","Texture","Perimeter","Area","Smoothness","Compactness","Concavity","Concave Points","Symmetry","Fractal Dimensions","Radius_SE","Texture_SE","Perimeter_SE","Area_SE","Smoothness_SE","Compactness_SE","Concavity_SE","Concave Points_SE","Symmetry_SE","Fractal Dimensions_SE","Radius_worst","Texture_worst","Perimeter_worst","Area_worst","Smoothness_worst","Compactness_worst","Concavity_worst","Concave Points_worst","Symmetry_worst","Fractal Dimensions_worst")

act_data<-raw_data[-1,-1]
results<-act_data[,2]
train_data<-act_data[,-2]
train_data<-train_data[,-1]
train_data<-as.data.frame(sapply(train_data,as.numeric))

which(is.na(train_data), arr.ind=TRUE)

#find relationship of radius with other parameter,  perimeter found
plot(train_data$Radius,train_data$Perimeter)
#get numerical relationship
Perim_factor<-median(train_data$Perimeter[-115]/train_data$Radius[-115])
# apply relationship to fill missing radius values
train_data[115,1]<-train_data$Perimeter[115]/Perim_factor

#find relationship of smoothness with other parameter, compactness found closest
plot(train_data$Compactness,train_data$Smoothness)
mean(train_data$Smoothness[-86])
#get numerical relationship
smoothness_to_compact<-median(train_data$Smoothness[-86]/train_data$Compactness[-86])
#apply relationship to missing values of smoothness
train_data$Smoothness[86]<-smoothness_to_compact*train_data$Compactness[86]

#confirm relationship between concavity and concave points 
plot(train_data$Concavity,train_data$`Concave Points`)
#ger numerical realtionship
point_to_concavity<-median(train_data$Concavity/train_data$`Concave Points`,na.rm=TRUE)

#apply relationship to each missing value
train_data$Concavity[22]<-point_to_concavity*train_data$`Concave Points`[22]
train_data$Concavity[113]<-point_to_concavity*train_data$`Concave Points`[113]
train_data$Concavity[183]<-point_to_concavity*train_data$`Concave Points`[183]

train_data$`Concave Points`[7]<-train_data$Concavity[7]/point_to_concavity
train_data$`Concave Points`[10]<-train_data$Concavity[10]/point_to_concavity
train_data$`Concave Points`[43]<-train_data$Concavity[43]/point_to_concavity


#no satisfactory relationship found for symmetry so local meadian is used
train_data$Symmetry[25]<-median(train_data$Symmetry, na.rm = T)

#Fractal dimensions with smoothness
plot(train_data$`Fractal Dimensions`,train_data$Smoothness)
#math relation
smooth_to_fractal<-median(train_data$Smoothness/train_data$`Fractal Dimensions`,na.rm=TRUE)
#apply realtionship to NAs
train_data$`Fractal Dimensions`[69]<-train_data$Smoothness[69]/smooth_to_fractal
train_data$`Fractal Dimensions`[111]<-train_data$Smoothness[111]/smooth_to_fractal
train_data$`Fractal Dimensions`[151]<-train_data$Smoothness[151]/smooth_to_fractal


plot(train_data$Radius_SE,train_data$Perimeter_SE)
r_to_per_SE<-median(train_data$Perimeter_SE/train_data$Radius_SE,na.rm = T)
train_data$Perimeter_SE[28]<-r_to_per_SE*train_data$Radius_SE[28]

for( i in seq(11,30)){
  train_data[36,i]<-median(train_data[,i],na.rm = T)
  train_data[69,i]<-median(train_data[,i],na.rm = T)
}

plot(train_data$Texture_SE,train_data$Concavity_SE)
conv_to_text<-median(train_data$Concavity_SE/train_data$Texture_SE, na.rm = T)
train_data$Texture_SE[28]<-train_data$Concavity_SE[28]/conv_to_text


conc_to_points_SE<-median(train_data$Concavity_SE/train_data$`Concave Points_SE`,na.rm=TRUE)
train_data[33,17]<-conc_to_points_SE*train_data[33,18] 
train_data$`Concave Points_SE`[59]<-train_data$Concavity_SE[59]/conc_to_points_SE

train_data$Symmetry_SE[4]<-median(train_data$Symmetry_SE, na.rm = T)

plot(train_data$Radius_worst,train_data$Perimeter_worst)
r_to_per_SE<-median(train_data$Perimeter_SE/train_data$Radius_SE,na.rm = T)
train_data$Perimeter_SE[28]<-r_to_per_SE*train_data$Radius_SE[28]

plot(train_data$`Fractal Dimensions_SE`,train_data$Symmetry_SE)
frac_to_sym<-median(train_data$Symmetry_SE/train_data$`Fractal Dimensions_SE`,na.rm=TRUE)
train_data[8,20]<-train_data$Symmetry_SE[8]/frac_to_sym
train_data[37,20]<-train_data$Symmetry_SE[37]/frac_to_sym
train_data[44,20]<-train_data$Symmetry_SE[44]/frac_to_sym
train_data[128,20]<-train_data$Symmetry_SE[128]/frac_to_sym
train_data[132,20]<-train_data$Symmetry_SE[132]/frac_to_sym

train_data<-train_data[,1:20]
colnames(train_data[18])


#confirm all NAs dealt with
table(is.na(train_data)) 

#Normalisazation
normalise<- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalised_train<-as.data.frame(lapply(train_data,normalise))

k_val<-sqrt(150)


errorSum<-c()
for(i in seq(1,100)){
  sim<-knn(normalised_train[1:150,],normalised_train[151:200,],results[1:150],i)
  
  tbl<-table(results[151:200], sim)
  errorSum<-c(errorSum,(tbl[1,3]+tbl[2,1]))
}

errorTable<- data.frame(seq(1:100),errorSum)
ggplot(data= errorTable,aes(x=errorTable[,1],y=errorTable[,2])) + geom_smooth(method = "loess") + geom_line() + ggtitle("Sum of errors against K-value - Small Data") + xlab("K - Value") + ylab("Sum of Errors")

k_best<-which(errorTable[,2]==min(errorTable[,2]))
#best k value is 3
sim<-knn(normalised_train[1:150,],normalised_train[151:200,],results[1:150],k_best) 

table(results[151:200], sim)

### Big

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
  sim_big<-knn(normalised_train_big[1:469,],normalised_train_big[470:569,],results_big[1:469],i) 
  
  tbl<-table(results_big[470:569], sim_big)
  errorSum<-c(errorSum,(tbl[1,2]+tbl[2,1]))
}

errorTable_big<- data.frame(seq(1:100),errorSum)

ggplot(data= errorTable_big,aes(x=errorTable_big[,1],y=errorTable_big[,2])) + geom_smooth(method = "loess") + geom_line() + ggtitle("Sum of errors against K-value - Big Data") + xlab("K - Value") + ylab("Sum of Errors")

k_best<-which(errorTable_big[,2]==min(errorTable_big[,2]))

##k value 6 gives best solytion for the occation
sim_big_opt<-knn(normalised_train_big[1:469,],normalised_train_big[470:569,],results_big[1:469],6) 

table(results_big[470:569], sim_big_opt)

errorSS<-data.frame(errorTable[,2],errorTable_big[,2])
names(errorSS)<-c("Small Data","Big Data")
 
ggplot(data= errorSS,aes(x=1:100)) + geom_smooth(aes(y=errorSS[,2], colour = "Big Data"),method = "loess") + 
  geom_smooth(aes(y=errorSS[,1], colour = "Small Data"),method = "loess") + 
  labs(title="Sum of errors against K-value - Big Data & Small Data Comparison", x="K - Value",y="Sum of Errors") +
  scale_colour_manual(name="Data Sets", values=c("deepskyblue3", "coral1"))

