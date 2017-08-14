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
  }else{
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
     pch=c(15,16,17)[unclass(iris$Species)],
     col=c("black","red","blue")[unclass(iris$Species)],
     xlab = " Petal Lenght",ylab="Petal Width")
legend("topleft",legend = levels(iris$Species), 
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