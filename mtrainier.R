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
regressor = lm(formula = Temperature.AVG ~ day+day2+day3+day4,
               data = training_set)

# library(e1071)
# regressor2 = svm(formula =Solare.Radiation.AVG ~ day,
#                 data =training_set,
#                 type = 'eps-regression',
#                 kernel = 'radial')

# 
# #svr
# # library(e1071)
# # regressor = svm(formula = Temperature.AVG ~ day,
# #                 data = training_set,
# #                 type = 'eps-regression',
# #                 kernel = 'radial')

# library(randomForest)
# set.seed(1234)
# regressor = randomForest(x = training_set,
#                          y = training_set$Temperature.AVG,
#                          ntree = 500)


#month wise prediction






# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$day, y = training_set$Temperature.AVG),
             colour = 'red') +
  geom_line(aes(x = training_set$day, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('date vs temperature (Training set)') +
  xlab('Date') +
  ylab('temperature')

# Visualising the Training set results for solar radiation
# library(ggplot2)
# ggplot() +
#   geom_point(aes(x = training_set$day, y = training_set$Solare.Radiation.AVG),
#              colour = 'red') +
#   geom_line(aes(x = training_set$day, y = predict(regressor, newdata = training_set)),
#             colour = 'blue') +
#   ggtitle('date vs Solar Radiation (Training set)') +
#   xlab('Date') +
#   ylab('Radiation')



# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$day, y = test_set$Temperature.AVG),
             colour = 'red') +
  geom_line(aes(x = training_set$day, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('date vs temperature (Training set)') +
  xlab('Date') +
  ylab('temperature')




