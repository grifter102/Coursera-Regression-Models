#q1 - answer .1471
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
mu <- mean(x)

#q2 - answer .8263
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ x - 1)

#q3 - answer -5.344
data(mtcars)
lm(mpg ~ wt, data = mtcars)

#q4 - answer 1

#q5 - answer .6
1.5 * .4

#q6 - answer -.9719
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
scale(x)

#q7 - answer 1.567
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ x)

#q8 - answer Must be 0

#q9 - answer .573
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

#q10 - answer Var(Y)/Var(X)
