library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrplot)
library(corrgram)

#read file and check the head
bike <- read.csv('.../Documents/Math 151/bikeshare.csv')
print(head(bike))

#plot 1
pl <- ggplot(bike, aes(x=temp, y=count, color=temp)) + geom_jitter(size=0.7, alpha=0.2) + theme_bw()
pl

#convert datetime before plotting
bike[['datetime']] <- as.POSIXct(bike[['datetime']])

#plot 2
pl2 <- ggplot(bike, aes(x=datetime, y=count, color=temp)) + geom_jitter(size=0.7, alpha=0.4) + theme_bw() + scale_color_gradient(low = 'turquoise', high = 'orangered')
pl2

#correlation between temp and count
cor.data <- cor(select(bike, temp, count))
print(cor.data)

#convert season to factor before plotting
bike[['season']] <- as.factor(bike[['season']])

#plot 3
pl3 <- ggplot(bike, aes(x=season, y=count, color=season)) + geom_boxplot() + theme_bw()
pl3

#add hour as new column from datetime
hour <- format(bike$datetime, "%H")
bike$hour <- hour
print(head(bike))

#plot 4 workingday == 1
pl4 <- ggplot(bike[bike$workingday==1,], aes(x=hour, y=count, color=temp)) + geom_jitter(size=0.7, alpha=0.5) + theme_bw() + scale_color_gradientn(colors=c('navyblue', 'blue', 'turquoise' , 'chartreuse', 'yellow', 'orange', 'red'))
pl4

#plot 5 workingday == 0
pl5 <- ggplot(bike[bike$workingday==0,], aes(x=hour, y=count, color=temp)) + geom_jitter(size=0.7, alpha=0.5) + theme_bw() + scale_color_gradientn(colors=c('navyblue', 'blue', 'turquoise' , 'chartreuse', 'yellow', 'orange', 'red'))
pl5

#build the model
temp.model <- lm(count ~ temp, bike)
print(summary(temp.model))

#method 1
coef <- as.data.frame(coefficients(temp.model))
check_temp <- 25
rental <- coef[1,1] + coef[2,1]*check_temp
print(paste('Method 1: ',round(rental,4)))

#method 2
rental.predict <- predict(temp.model, data.frame(temp=25))
print(paste('Method 2: ', round(rental.predict, 4)))

bike$hour <- sapply(bike$hour, as.numeric)
str(bike)

final <- lm(count~.-casual -registered -datetime -atemp, bike)
print(summary(final))