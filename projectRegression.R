library(readxl)
library(dplyr)

data <- read_excel('projectRegressionData.xlsx')
head(data)
data <- data %>% mutate(volumeSqd = volume^2)
data <- data %>% mutate(volumeCu = volume^3)
data <- data %>% mutate(casesSqd = cases^2)
data <- data %>% mutate(casesCu = cases^3)
data <- data %>% mutate(casesLn = log(cases))
data <- data %>% mutate(deathsSqd = deaths^2)
data <- data %>% mutate(vaccSqd = vacc^2)

library(ggplot2)
hist(data$vacc)
data %>% ggplot(aes(vacc,close)) + geom_point() + geom_smooth(se=FALSE)+
  ggtitle('We see an upward trend in the closing price
          and the vaccination rate') + theme_bw()
data %>% ggplot(aes(cases,close)) + geom_point() + geom_smooth()
data %>% ggplot(aes(volume,close)) + geom_point() + geom_smooth(se=FALSE) +
  ggtitle(' Downward trend b/w closing price 
          and volume ') + theme_bw()
  
data %>% ggplot(aes(vacc,cases)) + geom_point() + geom_smooth(se=FALSE) +
  ggtitle('Decreasing trend in the interaction b/w 
          no of cases & vaccination rate') + theme_bw()

attach(data)

ggplot(data, aes(close,cases)) + geom_point()
# linefit1 <- lm(close ~ volume + volumeSqd + volumeCu + cases + casesSqd + deaths + deathsSqd + vacc + cases*vacc + stim_month + stim_month_plusone)
linefit1 <- lm(close ~ volume + volumeSqd + volumeCu + cases + casesSqd + vacc + cases*vacc + stim_month + stim_month_plusone)

summary(linefit1)

plot(vacc, close)
cor(data[,3:14])

linefit2 <- lm(close ~ volume + cases + vacc)
summary(linefit2)




linefit1.stres <- rstandard(linefit1)
plot(linefit1$fitted.values, linefit1.stres, pch = 16)
abline(0,0,lty=2,col="red")

h <- hist(linefit2.stres)
x <- linefit2.stres
xfit <- seq(min(x), max(x), length = 50)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue")

qqnorm(linefit2.stres, main = "Normal Probability Plot", xlab = "Normal Scores", ylab = "Standardized Residuals")
qqline(linefit2.stres, col = "red")

shapiro.test(linefit2.stres)

linefit2 =  lm(close ~ volume  + cases + casesSqd + casesCu + vacc + cases*vacc + stim_month + stim_month_plusone)
summary(linefit2)

linefit2.stres <- rstandard(linefit2)
plot(linefit2$fitted.values, linefit2.stres, pch = 16 , ylab = "Standard residuals",
     xlab = "Fitted Close Price")
abline(0,0,lty=2,col="red")

h <- hist(linefit2.stres , xlab = "Standart Residuals")
x <- linefit2.stres
xfit <- seq(min(x), max(x), length = 50)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue")

qqnorm(linefit2.stres, main = "Normal Probability Plot", xlab = "Normal Scores", ylab = "Standardized Residuals")
qqline(linefit2.stres, col = "red")

shapiro.test(linefit2.stres)

