#question 1 - .05296
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

fit <- lm(y~x)
summary(fit)

#question 2 - .223
summary(fit)

#question 3 - 18.991
data(mtcars)


n <- nrow(mtcars)
x <- mtcars$wt
y <- mtcars$mpg
fit3 <- lm(y ~ x)
mwt <- mean(x)
newdata<-data.frame(x=mwt)
p2 <- predict(fit3, newdata, interval = ("confidence"))
summary(p2)

#question 4 - change in mpg per 1000

#question 5 - 27.57
p2 <- predict(fit3, data.frame(x=3), interval = ("prediction"))
summary(p2)

#question 6 - -12.973
fit4 <- lm(y ~ I(x/2))
sumcoef <- summary(fit4)$coefficients
sumcoef[2,1] + c(-1, 1) * qt(.975, df = fit4$df) * sumcoef[2, 2]

#question 7 - multiplied by 100

#question 8 - slope c * beta hat  <--- wrong

#question 9 - .25
fitintonly <- lm(y ~ 1)
ssint <- sum((y - fitintonly$fitted.values)^2)
sse <- sum((y - fit3$fitted.values)^2)
sse / ssint

#question 10 - if intercept then 0
