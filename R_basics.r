## CTRL+L --> CLEARS CONSOLE
## rm(list = ls())

#BASIC
#...4
function(theString){
    theString
}


#5,6,7
addThese <- function(num1,num2,isSum){
  if(num1==0){
    ans <- num2
  }else if(num2==0){
    ans <- num1
  }else{
    if(isSum){
      ans <- num1+num2
    }else{
      ans <- num1*num2
    }
  }
  
  print(ans)
}

#8
quickFunc<-function(func,data){
    func(data)
}

quickFunc(function(vec){vec[[1]]},vec1)

#INTERMIDIATE
#Blackjack
blackjack<-function(hand1,hand2){
  if(hand1==0 | hand2 ==0){
    print("Invalid cards in deck, please clean deck and roll again")
  }
  else{
    if(hand1>21 & hand2>21){
      print("Both hands blown out")
    }else if(hand2>21){
      hand1
    }else if(hand1>21){
      hand2
    }else{
      if(hand1>hand2){
        hand1
      }else if(hand2>hand1){
        hand2
      }else if( hand1== hand2){
        print("It's a draw")
      }
    }
  }
}

#uniquesum
uniqueSum<- function(num1,num2,num3){
  if(num1==num2 & num2==num3){
    0
  }else if(num1==num2){
    num1+num3
  }else if(num1==num3){
    num1+num2
  }else if(num2==num3){
    num1+num2
  }else{
    num1+num2+num3
  }
}
#TooHot
toohot<- function(temp,isSummer){
  hiLim<-90
  if(isSummer){
    hiLim<-100
  }
  if(temp>=60 & temp<=hiLim){
    T
  }else {
    F
  }
}
#LeapYear
leapYear<-function (year){
  if(year%%4==0){
    T
  }else{1
    F
  }
}
#WriteToFile
evens<-c()
for(i in seq(2,100,2)){
    evens<-c(evens,i)
}
write.csv(evens,"evens.csv")
inNums<-read.csv("evens.csv")
odds=inNums[2]+1
write.csv(odds,"odds.csv")
#plot
data(iris)
boxplot(Sepal.Width~Species,iris,ylab="Sepal Width")
plot(iris$Petal.Length,iris$Petal.Width,
     pch=c(15,16,17)[iris$Species],
     col=c("black","red","blue")[iris$Species],
     xlab = " Petal Lenght",ylab="Petal Width")
legend("topleft", title = "Species",legend = levels(iris$Species), 
       pch=c(15,16,17),
       col=c("black","red","blue"))
#CO2
#1 Ordered Factor
#2
mean_uptake<-mean(CO2$uptake)
#3
boxplot(uptake~Type,CO2, ylab="uptake", main="CO2 Uptake per City")
#4
quebec_CO2<-subset(CO2,Type=="Quebec",select=names(CO2)!="Type")
missi_CO2<-subset(CO2,Type=="Missisipi",select=names(CO2)!="Type")
#5 **input (quebec_CO2$uptake,missi_CO2$uptake)**
mean_checker<- function(vec1,vec2){
  if(mean(vec1)>mean(vec2)){
    print("first set has bigegr mean")
  }else if(mean(vec2)>mean(vec1)){
    print("second set has bigegr mean")
  }else{
    print("Both sets seem equal")
  }
}



#OrchardSprays
byTreat=aggregate(OrchardSprays$decrease,list(OrchardSprays$treatment), max)
colnames(byTreat)<-c("Treatment","Decrease")
max_decrease= byTreat$Treatment[which.max(byTreat$Decrease)]
boxplot(byTreat$Decrease~byTreat$Treatment, main="Decrease Per Treatment", xlab="Treatment",ylab="Decrease")

#Chicks
data(ChickWeight)
chicNweight<-subset(ChickWeight,select = names(ChickWeight)=="weight" | names(ChickWeight)=="Chick" | names(ChickWeight)=="Diet")

#~ overall diet performance, clues biggest wieght obtained by diet 3 
plot(chicNweight$weight,
     pch=c(16:19)[ChickWeight$Diet],
     col=c(1:4)[ChickWeight$Diet],
     xlab="Population", ylab="Chick Weight",main="Overall Diet Results")
legend("topleft", title = "Diet",legend = levels(ChickWeight$Diet), 
       pch=c(16:19),
       col=c(1:4))

#group by chicks
byChick<-aggregate(weight ~ factor(Chick, levels = c(1:50)), data = ChickWeight, FUN =  diff )
names(byChick)<-c("Chick","WeightChanges")
##calculating overall weight change from timestamp weightchanges
weight_change<-c()
for (i in 1:length(byChick$WeightChanges)){
  x<-byChick$WeightChange[i]
  xx<-Reduce("+",x)/length(x)
  weight_change=c(weight_change, sum(xx))
}
byChick$WeightChange<-weight_change

diet1<-subset(ChickWeight,Diet==1,select = names(ChickWeight)== "weight" | names(ChickWeight)== "Time" | names(ChickWeight)=="Chick")
diet2<-subset(ChickWeight,Diet==2,select = names(ChickWeight)== "weight" | names(ChickWeight)== "Time" | names(ChickWeight)=="Chick")
diet3<-subset(ChickWeight,Diet==3,select = names(ChickWeight)== "weight" | names(ChickWeight)== "Time" | names(ChickWeight)=="Chick")
diet4<-subset(ChickWeight,Diet==4,select = names(ChickWeight)== "weight" | names(ChickWeight)== "Time" | names(ChickWeight)=="Chick")

install.packages(cowplot)
library(cowplot)

#for reordering according to factors, so diplayed sorted in legend
diet1$Chick<- factor(diet1$Chick, levels=(min(diet1$Chick):max(diet1$Chick)))
diet2$Chick<- factor(diet2$Chick, levels=(min(diet2$Chick):max(diet2$Chick)))
diet3$Chick<- factor(diet3$Chick, levels=(min(diet3$Chick):max(diet3$Chick)))
diet4$Chick<- factor(diet4$Chick, levels=(min(diet4$Chick):max(diet4$Chick))) 

p1<-ggplot(diet1,aes(x=Time,y=weight, group=Chick)) + geom_line(aes(color=Chick)) + ggtitle("Diet 1 - Weight Gain Patterns Over Time") + xlab("Days") + ylab("Weight (g)")
p2<-ggplot(diet2,aes(x=Time,y=weight, group=Chick)) + geom_line(aes(color=Chick)) + ggtitle("Diet 2 - Weight Gain Patterns Over Time") + xlab("Days") + ylab("Weight (g)")
p3<-ggplot(diet3,aes(x=Time,y=weight, group=Chick)) + geom_line(aes(color=Chick)) + ggtitle("Diet 3 - Weight Gain Patterns Over Time") + xlab("Days") + ylab("Weight (g)")
p4<-ggplot(diet4,aes(x=Time,y=weight, group=Chick)) + geom_line(aes(color=Chick)) + ggtitle("Diet 4 - Weight Gain Patterns Over Time") + xlab("Days") + ylab("Weight (g)")

#multiplot
plot_grid(p1,p2,p3,p4)

#get relative diet types into byChick
diets<-c()
for (i in 1:max(byChick$Chick)){
  set<-ChickWeight$Diet[which(ChickWeight$Chick==i)]
  diets<-c(diets,set[1])
}
byChick$diet<-diets
#barplot
ggplot(byChick,aes(x=Chick,y=WeightChange, fill=diet))+geom_bar(stat="identity")+ylab("Weight Change") + ggtitle("Overall Weight Change of Chicks")
 
#Primes
primlist<-c(2)
for(x in seq(1,300000,2)){
  if(x>1){
    isnotprime<-0
    for(p in seq(2,ceiling(sqrt(x))+1,2)){
      if (x%%p == 0){
        isnotprime<-isnotprime+1
      }
    }
    if(isnotprime==0){
      primlist<-c(primlist,x)
    }
  }
}
print(primlist)

##Salary Predict
train<-read.csv("censusData_train.csv")
colnames(train)<-c("Age","Workclass","Census","EducationLevel","EducationYears","MaritalStatus","Occupation","Relationship","Race","Sex","CapitalGain","CapitalLoss","HoursPerWeek","NativeCountry","Salary")

table(is.na(train)) #chekc if empty values present/ was none

#shifting ? to NA
trainNA<-train
trainNA[trainNA == " ?"]<-NA #eg for 1 col --raw_data$Electrical[is.na(raw_data$Electrical)]<-"SBrkr"


##models
fit <- rpart(Salary ~ Sex +Workclass +Occupation + EducationLevel +CapitalGain + EducationYears + MaritalStatus, data = train[1:25000,], method="class")
fitNA <- rpart(Salary ~ Sex +Workclass +Occupation + EducationLevel +CapitalGain + EducationYears + MaritalStatus, data = trainNA[1:25000,], method="class")
fitwo_NA<- rpart(Salary ~ Workclass +Occupation  +CapitalGain  + MaritalStatus,data = trainWO_na[1:25000,],method = "class")

fit1
rpart.plot(fit, type = 3, extra = 101)



checkfit<-predict(fit,train[25001:32560,],type="class")
table(train[25001:32560,15],predicted=checkfit)

checkfitNA<-predict(fitNA,train[25001:32560,],type="class")
table(train[25001:32560,15],predicted=checkfitNA)

checkfitwo_NA<-predict(fitwo_NA,train[25001:32560,],type="class")
table(train[25001:32560,15],predicted=checkfitwo_NA)

#Replace Unknown (?) values with NA
trainNA[train == " ?"]<- NA

#Create subset without NA values
trainWO_na<-train[complete.cases(train),]

##Random Forest Model

rf<-randomForest(Salary~ MaritalStatus+EducationYears+  CapitalGain+ Age + HoursPerWeek+ Workclass +EducationLevel + Occupation + Sex  ,data=trainWO_na[1:25000,])
p1<-predict(rf,trainWO_na[25001:30161,])

confusionMatrix(p1,trainWO_na$Salary[25001:30161])


# ****** custom function for tabulating all colmn in a dataframe ****
tabul_cols<- function(dataFrame){
  colmnnames<-colnames(dataFrame)
  for(i in 1:length(colmnnames)){
    print(colmnnames[i])
    print(table(dataFrame[colmnnames[i]]))
  }

  ##Day 2
  factored_data<-read.csv("C:\\Users\\Administrator\\Documents\\HousePrices Comp\\train.csv")
test_data<-read.csv("C:\\Users\\Administrator\\Documents\\HousePrices Comp\\test.csv")


table_cols<- function(dataFrame){
  colmnnames<-colnames(dataFrame)
  for(i in 1:length(colmnnames)){
    print(colmnnames[i])
    print(table(dataFrame[colmnnames[i]]))
  }
}

table(is.na(raw_data))
WoNA_data<-raw_data[complete.cases(raw_data),]

table_nas<- function(dataFrame){
  colmnnames<-colnames(dataFrame)
  for(i in 1:length(colmnnames)){
    print(colmnnames[i])
    print(table(is.na(dataFrame[colmnnames[i]])))
  }
}
raw_data$LotFrontage[is.na(raw_data$LotFrontage)]<-0
sub_data$GarageYrBlt[is.na(sub_data$GarageYrBlt)]<- 0 
raw_data[is.na(raw_data)]<-"None"

factored_data<- factored_data[-nrow(factored_data),]

rf1<-randomForest(raw_data$SalePrice[1:1000] ~  as.factor(raw_data$MSZoning[1:1000]) + raw_data$TotRmsAbvGrd[1:1000] + raw_data$YearBuilt[1:1000] + as.factor(raw_data$OverallQual[1:1000]),data=raw_data[1:1000,])
rff<-randomForest(SalePrice ~ +GarageYrBlt+OverallQual+HouseStyle+BldgType+  +Condition1+Condition2 +  Neighborhood + LotArea,data=sub_data)

p1<-predict(rf1,raw_data[1001:1459,1:80])

write.csv(ps1, "C:\\Users\\Administrator\\Documents\\HousePrices Comp\\p5.csv")

sub_data<-subset(factored_data,select = names(factored_data)!= "Alley" & names(factored_data)!= "PoolQC" & names(factored_data)!= "Fence" & names(factored_data)!= "MiscFeature" & names(factored_data)!= "FireplaceQu" & names(factored_data)!= "GarageType" & names(factored_data)!= "GarageFinish" & names(factored_data)!= "GarageQual" & names(factored_data)!= "GarageCond" & names(factored_data)!= "BsmtQual"  & names(factored_data)!= "BsmtCond"  & names(factored_data)!= "BsmtExposure" & names(factored_data)!= "BsmtFinType1"  & names(factored_data)!= "BsmtFinType2" )
sub_data$GarageYrBlt[is.na(sub_data$GarageYrBlt)==T]<-0
addNA(sub_data$MasVnrArea) #to all na cols

rfsd<-randomForest(SalePrice ~ MSZoning + TotRmsAbvGrd + YearBuilt + OverallQual, data = sub_data, ntree=10000)
na.omit(rfsd)
ps1<-predict(rfsd,test_data)
