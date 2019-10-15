#importing libraries
library(e1071)
library(ggplot2)


# Importing the dataset
jan = read.csv('January.csv')
feb = read.csv('Febuary.csv')
mar = read.csv('March.csv')
apr = read.csv('April.csv')
may = read.csv('May.csv')
jun = read.csv('June.csv')
jul = read.csv('July.csv')
aug = read.csv('August.csv')
sep = read.csv('September.csv')
oct = read.csv('Oct.csv')
nov = read.csv('November.csv')
dec = read.csv('December.csv')

sep_t = read.csv('September_test.csv')
oct_t = read.csv('October_test.csv')
nov_t = read.csv('November_test.csv')
dec_t = read.csv('December_test.csv')

#making list of months
months=list(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)

months_test=list(oct_t,nov_t,dec_t)

#adding higher degree variables
i=1
while(i<=12){
  print(i)
  months[[i]]$Date2 =months[[i]]$Date^2
  months[[i]]$Date3 =months[[i]]$Date^3
  months[[i]]$Date4 =months[[i]]$Date^4
  i=i+1
}


#building regressors for each month(svm)
regressor1 = svm(formula =Temperature ~ Date+Date2+Date3+Date4,
                 data =months[[1]],
                 type = 'eps-regression',
                 kernel = 'radial')
regressor2 = svm(formula =Temperature ~ Date+Date2+Date3+Date4,
                 data =months[[2]],
                 type = 'eps-regression',
                 kernel = 'radial')
regressor3 = svm(formula =Temperature ~ Date+Date2+Date3+Date4,
                 data =months[[3]],
                 type = 'eps-regression',
                 kernel = 'radial')
regressor4 = svm(formula =Temperature ~ Date+Date2+Date3+Date4,
                 data =months[[4]],
                 type = 'eps-regression',
                 kernel = 'radial')
regressor5 = svm(formula =Temperature ~ Date+Date2+Date3+Date4,
                 data =months[[5]],
                 type = 'eps-regression',
                 kernel = 'radial')
regressor6 = svm(formula =Temperature ~ Date+Date2+Date3+Date4,
                 data =months[[6]],
                 type = 'eps-regression',
                 kernel = 'radial')
regressor7 = svm(formula =Temperature ~ Date+Date2+Date3+Date4,
                 data =months[[7]],
                 type = 'eps-regression',
                 kernel = 'radial')
regressor8 = svm(formula =Temperature ~ Date+Date2+Date3+Date4,
                 data =months[[8]],
                 type = 'eps-regression',
                 kernel = 'radial')
regressor9 = svm(formula =Temperature ~ Date+Date2+Date3+Date4,
                 data =months[[9]],
                 type = 'eps-regression',
                 kernel = 'radial')
regressor10 = svm(formula =Temperature ~ Date+Date2+Date3+Date4,
                 data =months[[10]],
                 type = 'eps-regression',
                 kernel = 'radial')
regressor11 = svm(formula =Temperature ~ Date+Date2+Date3+Date4,
                 data =months[[11]],
                 type = 'eps-regression',
                 kernel = 'radial')
regressor12 = svm(formula =Temperature ~ Date+Date2+Date3+Date4,
                 data =months[[12]],
                 type = 'eps-regression',
                 kernel = 'radial')
# Visualising the Training set results
models=list(regressor1,regressor2,regressor3,regressor4,regressor5,regressor6,regressor7,regressor8,regressor9,regressor10,
            regressor11,regressor12)

#change vlaue for different month;'s graph
i=12

#check why while loop not plotting
#while(i<=12){
  ggplot() +
  geom_point(aes(x = months[[i]]$Date, y = months[[i]]$Temperature),
             colour = 'red') +
  geom_line(aes(x = months[[i]]$Date, y = predict(models[[i]], newdata = months[[i]])),
            colour = 'blue') +
  ggtitle('date vs temperature (Training set)') +
  xlab('Date') +
  ylab('temperature')
  #i=i+1}
  
j=i-9
#test set
  ggplot() +
    geom_point(aes(x = months_test[[j]]$Date, y = months_test[[j]]$Temperature.AVG),
               colour = 'red') +
    geom_line(aes(x = months[[i]]$Date, y = predict(models[[i]], newdata = months[[i]])),
              colour = 'blue') +
    ggtitle('date vs temperature (Test set)') +
    xlab('Date') +
    ylab('temperature')
  
  

  
