rm(list = ls())
tombstone <-
  read.table(
    "C:/Users/Abhay/Google Drive/MSIS/BANA 7038/Homework 2/tombstone.csv",
    header = TRUE,
    sep = ","
  )
#heyyyy
head(tombstone)
summary(tombstone)
names(tombstone) <- c("city", "covariate", "response")

#2. Plot data, explore data, and briefly describe what you observe.
summary(tombstone)
nrow(tombstone)
colSums(is.na(tombstone))
mean(tombstone$covariate)
mean(tombstone$response)
sd(tombstone$covariate)
sd(tombstone$response)
par(mfrow = c(2, 4))
hist(tombstone$covariate)
hist(tombstone$response)
plot(tombstone$covariate, tombstone$response, pch = 20)
cor(tombstone$covariate, tombstone$response)

boxplot(tombstone$covariate)
boxplot(tombstone$response)

#3. Perform linear regression using lm() function
#3.1. Obtain coefficient estimates
#3.2. Obtain R^2, explain what it means.
lr <- lm(tombstone$response ~ tombstone$covariate, data = tombstone)
summary(lr)
plot(tombstone$covariate, tombstone$response, pch = 20)
abline(lr)
lr$coefficients
sum(lr$residuals)
sum(lr$fitted.values)
points(mean(tombstone$covariate),mean(tombstone$response),pch=20,col="blue",cex=3)
abline(lr,col="blue",lwd=3)



#4. Perform the following hypothesis testing and interval estimation using lm() and other related R functions.
#4.1. Perform t tests, obtain t statistics and p values, interpret the results, make a conclusion 
#(i.e. reject or not reject) and explain why.  Note: please explain what the null hypothesis is.

#4.2. Perform ANOVA test (F test), obtain F statistic and p value, 
#interpret the results, make conclusion (i.e. reject or not reject) and explain why.  
#Note: please explain what the null hypothesis is.
anova(lr)
#4.3. Compute confidence interval for coefficients, fitted values (mean response), 
#interpret the meanings of these quantities.
confint(lr,level=0.95)
predict.lm(lr, interval="confidence") 

#4.4. Plot data points, the regression line, the confidence interval for fitted values.

par(mfrow = c(2, 4))
#plot(tombstone$covariate, tombstone$response, pch = 20)
#abline(lr)

# predicts + interval
newx <-
  seq(
    min(tombstone$covariate),
    max(tombstone$covariate),
    length.out = length(tombstone$covariate)
  )
preds <-
  predict(
    lr,
    newdata = data.frame(x = newx),
    interval = c("confidence"),
    level = 0.95,
    type = "response"
  )

#preds <- predict(lr, newdata = data.frame(x = newx),                 interval = 'confidence')

# plot
plot(tombstone$response ~ tombstone$covariate, data = tombstone)
# add fill
polygon(c(rev(newx), newx),
        c(rev(preds[, 3]), preds[, 2]),
        col = 'grey80',
        border = NA)
# model
abline(lr)
# intervals
lines(newx, preds[, 3], lty = 'dashed', col = 'red')
lines(newx, preds[, 2], lty = 'dashed', col = 'red')



#5. Repeat the same questions (1-5) but with much more concise descriptions of results for the date set 
#<bus.csv> using different covariates. 
#Description: Cross-sectional analysis of 24 British bus companies (1951).

#5.1 Use response variable = Expenses per car mile (pence), covariate = Car miles per year (1000s).
bus <-
  read.table(
    "C:/Users/Abhay/Google Drive/MSIS/BANA 7038/Homework 2/bus.csv",
    header = TRUE,
    sep = ","
  )
head(bus)
nrow(bus)
summary(bus)
colSums(is.na(bus))
names(bus) <-
  c("response",
    "covariate1",
    "covariate2",
    "covariate3",
    "covariate4")

mean(bus$covariate1)
mean(bus$response)
sd(bus$covariate1)
sd(bus$response)
par(mfrow = c(2, 4))
hist(bus$covariate1)
hist(bus$response)
plot(bus$covariate1, bus$response, pch = 20)
cor(bus$covariate1, bus$response)

boxplot(bus$covariate1)
boxplot(bus$response)

lrbus <- lm(bus$response ~ bus$covariate1, data = bus)
summary(lrbus)
plot(bus$covariate1, bus$response, pch = 20)
abline(lrbus)
lrbus$coefficients
sum(lrbus$residuals)
sum(lrbus$fitted.values)

anova(lrbus)

confint(lrbus,level=0.95)
predict.lm(lrbus, interval="confidence") 

par(mfrow = c(1, 1))

#abline(lr)

# predicts + interval
newx <-
  seq(
    min(bus$covariate1),
    max(bus$covariate1),
    length.out = length(bus$covariate1)
  )
preds <-
  predict(
    lrbus,
    newdata = data.frame(x = newx),
    interval = c("confidence"),
    level = 0.95,
    type = "response"
  )

# plot
plot(bus$response ~ bus$covariate1, data = bus)
abline(lrbus)
# intervals
lines(newx, preds[, 3], lty = 'dashed', col = 'red')
lines(newx, preds[, 2], lty = 'dashed', col = 'red')

#5.2. Use response variable = Expenses per car mile (pence), covariate = Percent of Double Deckers in fleet.

#5.3. Use response variable = Expenses per car mile (pence), covariate = Percent of fleet on fuel oil.

#5.4. Use response variable = Expenses per car mile (pence), covariate = Receipts per car mile (pence).

#5.5. What is your observations on all these analysis?




#
set.seed(1234)
x <- rnorm(20)
df <- data.frame(x = x,
                 y = x + rnorm(20))

plot(y ~ x, data = df)

# model
mod <- lm(y ~ x, data = df)

# predicts + interval
newx <- seq(min(df$x), max(df$x), length.out=100)
preds <- predict(mod, newdata = data.frame(x=newx), 
                 interval = 'confidence')

# plot
plot(y ~ x, data = df, type = 'n')
# add fill
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
# model
abline(mod)
# intervals
lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
#


