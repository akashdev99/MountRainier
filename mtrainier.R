###########################START###################################
# Importing the dataset
dataset = read.csv('Rainier_Weather.csv')


#splitting test and train
training_set = dataset[1:364,]
test_set = dataset[365:464,]


#ordering dataset
training_set<- training_set[seq(dim(training_set)[1],1),]
test_set<- test_set[seq(dim(test_set)[1],1),]

date=vector()
#turning date into increment of days
training_set<-training_set[,-1]
for(x in 1:nrow(training_set)){
  date[x]<-x
}
training_set = cbind(training_set,day=date)

dates=vector()
test_set<-test_set[,-1]
c=265
for(x in 1:100){
  dates[x]<-(c+x)
  
}
test_set = cbind(test_set,day=dates)


#Polynomial regressor
training_set$day2 =training_set$day^2
training_set$day3 =training_set$day^3
training_set$day4 =training_set$day^4
#temeprature poly
regressor_temp = lm(formula = Temperature.AVG ~ day+day2+day3+day4,
               data = training_set)
#solar SVM
library(e1071)
regressor_solar = svm(formula =Solare.Radiation.AVG ~ day,
                data =training_set,
                type = 'eps-regression',
                kernel = 'radial')
############################END#############################

#Ranom Forest humidity
library(randomForest)
set.seed(1234)
training_set_humid =training_set[,-1:-2] 
training_set_humid =training_set_humid[,-2:-4]
training_set_humid =training_set_humid[,-3:-5]
regressor_humid = randomForest(x = training_set_humid,
                         y = training_set$Relative.Humidity.AVG,
                         ntree = 500)
#Ranom Forest windspeed
library(randomForest)
set.seed(1234)
training_set_wind =training_set[,-1:-3] 
training_set_wind =training_set_wind[,-2:-3]
training_set_wind =training_set_wind[,-3:-5]
regressor_wind = randomForest(x = training_set_wind,
                              y = training_set$Wind.Speed.Daily.AVG,
                              ntree = 500)

# Visualising the Training set results solar radiation
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$day, y = training_set$ Solare.Radiation.AVG ),
             colour = 'red') +
  geom_line(aes(x = training_set$day, y = predict(regressor_solar, newdata = training_set)),
            colour = 'blue') +
  ggtitle('date vs Radiation (Training set)') +
  xlab('Date') +
  ylab('Radiation')

# Visualising the Test set results solar radiation
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$day, y = test_set$Solare.Radiation.AVG),
             colour = 'red') +
  geom_line(aes(x = training_set$day, y = predict(regressor_solar, newdata = training_set)),
            colour = 'blue') +
  ggtitle('date vs Radiation (testing))') +
  xlab('Date') +
  ylab('radiation')



# Visualising the Training set results temperature
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$day, y = training_set$Temperature.AVG),
             colour = 'red') +
  geom_line(aes(x = training_set$day, y = predict(regressor_temp, newdata = training_set)),
            colour = 'blue') +
  ggtitle('date vs temperature (Training set)') +
  xlab('Date') +
  ylab('temperature')


# Visualising the Test set results temperature
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$day, y = test_set$Temperature.AVG),
             colour = 'red') +
  geom_line(aes(x = training_set$day, y = predict(regressor_temp, newdata = training_set)),
            colour = 'blue') +
  ggtitle('date vs temperature (Training set)') +
  xlab('Date') +
  ylab('temperature')

# Visualising the Training set results humidity
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$day, y = training_set$ Relative.Humidity.AVG ),
             colour = 'red') +
  geom_line(aes(x = training_set$day, y = predict(regressor_humid, newdata = training_set)),
            colour = 'blue') +
  ggtitle('date vs Humidity (Training set)') +
  xlab('Date') +
  ylab('humidity')


# Visualising the Test set results humidity
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$day, y = test_set$Relative.Humidity.AVG),
             colour = 'red') +
  geom_line(aes(x = training_set$day, y = predict(regressor_humid, newdata = training_set)),
            colour = 'blue') +
  ggtitle('date vs Humidity (testing))') +
  xlab('Date') +
  ylab('Humidity')

# Visualising the Training set results windspeed
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$day, y = training_set$ Wind.Speed.Daily.AVG ),
             colour = 'red') +
  geom_line(aes(x = training_set$day, y = predict(regressor_wind, newdata = training_set)),
            colour = 'blue') +
  ggtitle('date vs Windspeed (Training set)') +
  xlab('Date') +
  ylab('Windspeed')


# Visualising the Test set results windspeed
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$day, y = test_set$Wind.Speed.Daily.AVG),
             colour = 'red') +
  geom_line(aes(x = training_set$day, y = predict(regressor_wind, newdata = training_set)),
            colour = 'blue') +
  ggtitle('date vs Windspeed (testing))') +
  xlab('Date') +
  ylab('Windspeed')



#INput from the user
rang.start = readline(prompt="please enter the start=")
rang.end = readline(prompt="please enter the end=")
starts=strsplit(rang.start,split="/",fixed=TRUE)
start.day =starts[[1]][1]
start.month=starts[[1]][2]

ends=strsplit(rang.end,split="/",fixed=TRUE)
end.day = ends[[1]][1]
end.month=ends[[1]][2]
############################START#########################
#predicting attributes
#############Cchange value for different dates below######################
pred_start = 1
pred_end = 6

dates_1 = vector()
dates_2 =vector()
dates_3 =vector()
dates_4 =vector()
x=1
for(i in pred_start:pred_end){
  dates_1[x]<-i
  dates_2[x]<-i^2
  dates_3[x]<-i^3
  dates_4[x]<-i^4
  x=x+1
}
day=dates_1
day2=dates_2
day3=dates_3
day4=dates_4
user_set_temp = data.frame(day,day2,day3,day4)
user_set_solar = data.frame(day)
range=pred_end - pred_start +1
#randomforest data generation
if(pred_start==1){
  training_set_wind=training_set_wind[-(range+1):-365,]
  training_set_humid=training_set_humid[-(range+1):-365,]
} else if(pred_end ==365 ){
  training_set_wind=training_set_wind[-1:-(pred_start-1),]
  training_set_humid=training_set_humid[-1:-(pred_start-1),]
} else {
  training_set_wind=training_set_wind[-1:-(pred_start-1),]
  training_set_wind=training_set_wind[-(range+1 ):-365,]
  training_set_humid=training_set_humid[-1:-(pred_start-1),]
  training_set_humid=training_set_humid[-(range +1 ):-365,]
}


#pedicting attributes
pred_temp=predict(regressor_temp, data.frame(train_x =user_set_temp ))
pred_solar=predict(regressor_solar, data.frame(train_x =user_set_solar ))
#364 == 1,363 == 2 ...
pred_wind = predict(regressor_wind, newdata = training_set_wind)
pred_humid = predict(regressor_humid, newdata = training_set_humid)

score =vector()
#Scoring the date range
for( x in 1:range){
  sum=0
  if(pred_temp[x]<=35 && pred_temp[x]>=25){
    sum=sum+0.25
  }
  if(pred_humid[x]<=30 && pred_humid[x]>=77){
    sum=sum+0.25
  }
  if(pred_wind[x]<=28){
    sum=sum+0.25
  }
  if(pred_solar[x]<=80){
    sum=sum+0.25
  }
  print(sum)
  score[x]<-sum
}


######################END##############################








