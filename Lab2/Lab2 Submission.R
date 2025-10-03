####### Data Analytics Fall 2025 Lab 1 ######

library(ggplot2)

### set working directory
setwd("C:\\Users\\tony f\\data-anal-lab1\\Lab 2")

### read in data
ny.data <- read.csv("NY-House-Dataset.csv", header=TRUE)

View(ny.data)

#A home having more than 8 bathrooms is pretty extreme. I think it's safe to filter
ny.data <- subset(ny.data, BATH <= 8)
#Doing similar exclusion here to get rid of outliers
ny.data <- subset(ny.data, BEDS <= 17)

PRICE <- ny.data$PRICE
PROPERTYSQFT <- ny.data$PROPERTYSQFT
BEDS <- ny.data$BEDS
BATH <- ny.data$BATH

#heard this was a safer way to omit NA's
ny.data <- na.omit(ny.data[, c("PRICE", "PROPERTYSQFT", "BEDS", "BATH")])

ny.data$log_PRICE <- log10(PRICE)
ny.data$log_PROPERTYSQFT <- log10(PROPERTYSQFT)
#na.indices <- is.na(PRICE) 
#PRICE.compl <- PRICE[!na.indices]

#na.indices <- is.na(BEDS) 
#BEDS <- BEDS[!na.indices]

#na.indices <- is.na(BATH) 
#BATH <- BATH[!na.indices]

##fivenum(PRICE,na.rm=TRUE)

#three models creation
lin.mod0 <- lm(log_PRICE~log_PROPERTYSQFT,ny.data)
lin.mod1 <- lm(log_PRICE~BATH,ny.data)
lin.mod2 <- lm(log_PRICE~BEDS + BATH,ny.data)

summary(lin.mod0)
summary(lin.mod1)
summary(lin.mod2)

#mod0 plotting
ggplot(ny.data, aes(x = log_PROPERTYSQFT, y = log_PRICE)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod0, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

#mod1 plotting
ggplot(ny.data, aes(x = BATH, y = log_PRICE)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

#mod2 plotting
ggplot(ny.data, aes(x = BEDS, y = log_PRICE)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')