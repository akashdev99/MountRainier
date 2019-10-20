#import the dataset
dataset = read.csv('testing1.csv')

#adding score to each column
score =vector()

#set attributes according to the dataset 
for(x in 1:nrow(dataset)){
  sum=0
  if(dataset$Temperature[x]<=35 && dataset$Temperature[x]>=25){
    sum=sum+0.25
  }
  if(dataset$Relative.Humidity.AVG[x]<=30 && dataset$Relative.Humidity.AVG[x]>=77){
    sum=sum+0.25
  }
  if(dataset$Wind.Speed.Daily.AVG[x]<=28){
    sum=sum+0.25
  }
  if(dataset$Solare.Radiation.AVG[x]<=80){
    sum=sum+0.25
  }
  score[x]<-sum
}

dataset = cbind(dataset,success_rate=score)