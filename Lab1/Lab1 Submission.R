library(readr)
library(EnvStats)

# set working directory (relative path)
setwd("C:\\Users\\tony f\\data-anal-lab1\\Lab 1")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(epi.data)

#variable summaries
summary(epi.data$ECO.new)
summary(epi.data$BDH.new)

#variable boxplots
boxplot(epi.data$ECO.new,names = c("ECO"))
boxplot(epi.data$BDH.new,names = c("BDH"))

#histograms
x <- seq(20, 85, 5)
y <- seq(0, 90, 5)
hist(epi.data$ECO.new, x, prob=TRUE)
lines(x ,dnorm(x, mean=mean(epi.data$ECO.new, na.rm=TRUE), sd=sd(epi.data$ECO.new, na.rm=TRUE)), col="gold")

hist(epi.data$BDH.new, y, prob=TRUE)
lines(y ,dnorm(y, mean=mean(epi.data$BDH.new, na.rm=TRUE), sd=sd(epi.data$BDH.new, na.rm=TRUE)), col="purple")

#ECDF Plots
plot(ecdf(epi.data$ECO.new), do.points=FALSE, verticals=TRUE) 
plot(ecdf(epi.data$BDH.new), do.points=FALSE, verticals=TRUE) 

##QQ plots against normal dist
qqnorm(epi.data$ECO.new); qqline(epi.data$ECO.new)
qqnorm(epi.data$BDH.new); qqline(epi.data$BDH.new)

##QQ Plot of 2 vars against each other
qqplot(epi.data$ECO.new, epi.data$BDH.new, xlab = "Q-Q plot for ECO vs BDH") 

##Normality statistical tests
shapiro.test(epi.data$ECO.new)
shapiro.test(epi.data$BDH.new)

#Statistical test
ks.test(epi.data$ECO.new, epi.data$BDH.new)


