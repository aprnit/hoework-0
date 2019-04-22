## Machine Learning

library(caret)
library(dslabs)
library(dplyr)

data(heights)
x <- heights$height
y <- heights$sex
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights[-test_index,]
test_set <- heights[test_index,]
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
    factor(levels = levels(test_set$sex))
# Overall Accuracy
mean( y_hat == test_set$sex)
# Other Methods
y_hat <- ifelse(train_set$height > 62, "Male", "Female") %>%
    factor(levels = levels(train_set$sex))
mean(y_hat == train_set$sex)
## Overall Accuracy can be deceptive
table(predicted = y_hat, actual = test_set$sex)
# Compiute the accuracy seperately for each sex
test_set %>%
    mutate(y_hat = y_hat) %>%
    group_by(sex) %>%
    summarize(accuracy = mean(y_hat == sex))

confusionMatrix(data=y_hat, reference = test_set$sex)

## Questions
library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data("reported_heights")
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
    filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
    mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass", "online")) %>%
    select(sex, type)
y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# Answer-1
dat %>% filter(type == "inclass") %>% count(sex == "Female")
# Answer-2 (TBD)
y_hat <- ifelse(x == "inclass", "Female", "Male") %>%
    factor(levels = levels(y))
#y_hat <- ifelse(x == "inclass", sample(c("Female", "Male"), length(y), replace = TRUE, prob = c(0.6667, 1 - 0.6637)),
#                sample(c("Female", "Male"), length(y), replace = TRUE, prob = c(0.3783, 1 - 0.3783))) %>%
#                 factor(levels = levels(y))
mean(y_hat == y)

table(predicted = y_hat, actual = y)
confusionMatrix(data = y_hat, reference = y)

## Questions Set 2
library(caret)
library(purrr)
data(iris)
iris <- iris[-which(iris$Species == 'setosa'),]
y <- iris$Species
y <- droplevels(y)
set.seed(2)
# line of code
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) ## Answer-1
test_set <- iris[test_index,]
test_set <- droplevels(test_set)
train_set <- iris[-test_index,]
train_set <- droplevels(train_set)
## 1: greatest overall accuracy
cutoff <- seq(5.1, 7.6, 0.1) # Sepal.Length
F_1 <- map_dbl(cutoff, function(x) {
    y_hat <- ifelse(test_set$Sepal.Length >= x, "virginica", "versicolor") %>%
    factor(levels = levels(test_set$Species))
    F_meas(data = y_hat, reference = factor(test_set$Species))
})
print("Sepal_Length:")
max(F_1)
cutoff <- seq(2.1, 3.6, 0.1) # Sepal.Width
print("Sepal_Width:")
F_1 <- map_dbl(cutoff, function(x) {
    y_hat <- ifelse(test_set$Sepal.Width >= x, "virginica", "versicolor") %>%
    factor(levels = levels(test_set$Species))
    F_meas(data = y_hat, reference = factor(test_set$Species))
})
max(F_1)
cutoff <- seq(3.1, 6.9, 0.1) # Petal.Length $$ Max at 4.9
print("Petal_Length:")
F_1 <- map_dbl(cutoff, function(x) {
    y_hat <- ifelse(test_set$Petal.Length >= x, "virginica", "versicolor") %>%
    factor(levels = levels(test_set$Species))
    F_meas(data = y_hat, reference = factor(test_set$Species))
})
max(F_1)
##
cutoff <- seq(1.1, 2.4, 0.1) # Petal.Width ## Max at 1.7
print("Petal_Width:")
F_1 <- map_dbl(cutoff, function(x) {
    y_hat <- ifelse(train_set$Petal.Width >= x, "virginica", "versicolor") %>%
    factor(levels = levels(train_set$Species))
    F_meas(data = y_hat, reference = factor(train_set$Species))
})
max(F_1)
##
y_hat <- ifelse(test_set$Petal.Length >= 4.9, "virginica", "versicolor") %>%
    factor(levels = levels(test_set$Species))
confusionMatrix(data = y_hat, reference = test_set$Species)

#ggplot(aes(cutoff, F_1)) + geom_line()
y_hat <- ifelse(test_set$Petal.Length >= 4.9 | test_set$Petal.Width >= 1.7, "virginica", "versicolor") %>%
    factor(levels = levels(test_set$Species))
confusionMatrix(data = y_hat, reference = test_set$Species)

## Answer from the website below
library(caret)
data(iris)
iris <- iris[-which(iris$Species == 'setosa'),]
y <- iris$Species

plot(iris, pch = 21, bg = iris$Species)

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train[, 3])[1], range(train[, 3])[2], by = 0.1)
petalWidthRange <- seq(range(train[, 4])[1], range(train[, 4])[2], by = 0.1)
cutoffs <- expand.grid(petalLengthRange, petalWidthRange)

id <- sapply(seq(nrow(cutoffs)), function(i) {
    y_hat <- ifelse(train[, 3] > cutoffs[i, 1] | train[, 4] > cutoffs[i, 2], 'virginica', 'versicolor')
    mean(y_hat == train$Species)
}) %>% which.max

optimalCutoff <- cutoffs[id,] %>% as.numeric
y_hat <- ifelse(test[, 3] > optimalCutoff[1] & test[, 4] > optimalCutoff[2], 'virginica', 'versicolor')
mean(y_hat == test$Species)
## End of Answer

## Conditional Probability
set.seed(1)
disease <- sample(c(0, 1), size = 1e6, replace = TRUE, prob = c(0.98, 0.02))
test <- rep(NA, 1e6)
test[disease == 0] <- sample(c(0, 1), size = sum(disease == 0), replace = TRUE, prob = c(0.90, 0.10))
test[disease == 1] <- sample(c(0, 1), size = sum(disease == 1), replace = TRUE, prob = c(0.15, 0.85))

# Q2: Probability that the test is positive
test_positive <- mean(test)

# Q3: Probability that the individual has the disease when the test is negative
test_negative <- 1 - test_positive
# p(disease|test-) = p(test-|disease)*p(disease)/p(test-)

#Q4
mean(disease[test == 1])

#Q5: Normalize (TBD)
mean(disease[test == 1])/mean(disease)

#Q1
library(dslabs)
library(dplyr)
library(ggplot2)
data("heights")
heights %>%
    mutate(height = round(height)) %>%
    group_by(height) %>%
    summarize(p = mean(sex == "Male")) %>%
qplot(height, p, data = .)

#Q2
ps <- seq(0, 1, 0.1)
heights %>%
    mutate(g = cut(height, quantile(height, ps))) %>%
    group_by(g) %>%
    summarize(p = mean(sex == "Male"), height = mean(height)) %>%
    qplot(height, p, data = .)

#Q3
Sigma <- 9 * matrix(c(1, 0.5, 0.5, 1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
plot(dat)
ps <- seq(0, 1, 0.1)
dat %>%
    mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
    group_by(g) %>%
    summarize(y = mean(y), x = mean(x)) %>%
    qplot(x, y, data = .)

## Linear Regression for Prediction
library(HistData)
galton_heights <- GaltonFamilies %>%
    filter(childNum == 1 & gender == "male") %>%
    select(father, childHeight) %>%
    rename(son = childHeight)
# Predict Son's height Y using Fathers Height X
library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)
# Prediction Guess - would be average of son's height
avg <- mean(train_set$son)
# R squared Loss is:
mean((avg - test_set$son)^2)
## Linear Regression based prediction
fit <- lm(son ~ father, data = train_set)
fit$coeff
# Calculating Loss Function
y_hat <- fit$coeff[1] + fit$coefficients[2] * test_set$father
mean((y_hat - test_set$son) ^ 2)
# Using predict function
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son) ^ 2)

## Question 1
set.seed(1)
count <- 10000
Sigma <- 9 * matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = count, c(69, 69), Sigma) %>%
        data.frame() %>% setNames(c("x", "y"))

set.seed(1)
root_error <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- fit$coeff[1] + fit$coefficients[2] * test_set$x
    sqrt(mean((y_hat - test_set$y) ^ 2))
})

mean(root_error)
sd(root_error)

## Question 2
set.seed(1)
root_error <- function(count) {
    Sigma <- 9 * matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
    dat <- MASS::mvrnorm(n = count, c(69, 69), Sigma) %>%
        data.frame() %>% setNames(c("x", "y"))
    replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- fit$coeff[1] + fit$coefficients[2] * test_set$x
    sqrt(mean((y_hat - test_set$y) ^ 2))
})
}
set.seed(1)
count <- c(100, 500, 1000, 5000, 10000)
results <- sapply(count, root_error)
#100
mean(results[,1])
sd(results[,1])
#500
mean(results[,2])
sd(results[,2])
#1000
mean(results[,3])
sd(results[,3])
#5000
mean(results[,4])
sd(results[,4])
#10000
mean(results[,5])
sd(results[, 5])

#Q4
set.seed(1)
n <- 100
Sigma <- 9 * matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))

set.seed(1)
root_error <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- fit$coeff[1] + fit$coefficients[2] * test_set$x
    sqrt(mean((y_hat - test_set$y) ^ 2))
})

mean(root_error)
sd(root_error)

#Q6 and Q8
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
#cor(dat)
set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fit <- lm(y ~ x_1, data = train_set)
y_hat <- fit$coeff[1] + fit$coefficients[2] * test_set$x_1
root_error_x_1 <- sqrt(mean((y_hat - test_set$y) ^ 2))
print("x_1:")
mean(root_error_x_1)
set.seed(1)
fit <- lm(y ~ x_2, data = train_set)
y_hat <- fit$coeff[1] + fit$coefficients[2] * test_set$x_2
root_error_x_2 <- sqrt(mean((y_hat - test_set$y) ^ 2))
print("x_2:")
mean(root_error_x_2)
set.seed(1)
fit <- lm(y ~ x_1+x_2, data = train_set)
y_hat <- fit$coeff[1] + fit$coefficients[2] * test_set$x_1 + fit$coefficients[3] * test_set$x_2
root_error_x_1_and_2 <- sqrt(mean((y_hat - test_set$y) ^ 2))
print("x_1_and_2:")
mean(root_error_x_1_and_2)

#Q1
set.seed(2)
make_data <- function(n = 1000, p = 0.5,
                mu_0 = 0, mu_1 = 2,
                sigma_0 = 1, sigma_1 = 1) {

    y <- rbinom(n, 1, p)
    f_0 <- rnorm(n, mu_0, sigma_0)
    f_1 <- rnorm(n, mu_1, sigma_1)
    x <- ifelse(y == 1, f_1, f_0)

    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

    list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
    test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
dat$train %>% ggplot(aes(x, color = y)) + geom_density()
delta <- seq(0, 3, len = 25)

## Smoothing
library(tidyverse)
library(purrr)
library(pdftools)
library(lubridate)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package = "dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s) {
    s <- str_trim(s)
    header_index <- str_which(s, "2015")[1]
    tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
    month <- tmp[1]
    header <- tmp[-1]
    tail_index <- str_which(s, "Total")
    n <- str_count(s, "\\d+")
    out <- c(1:header_index, which(n == 1), which(n >= 28), tail_index:length(s))
    s[-out] %>%
        str_remove_all("[^\\d\\s]") %>%
        str_trim() %>%
        str_split_fixed("\\s+", n = 6) %>%
        .[, 1:5] %>%
        as_data_frame() %>%
        setNames(c("day", header)) %>%
        mutate(month = month,
            day = as.numeric(day)) %>%
        gather(year, deaths, - c(day, month)) %>%
        mutate(deaths = as.numeric(deaths))
}) %>%
    mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6,
                          "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
                          mutate(date = lubridate::make_datetime(year, month, day)) %>%
                          filter(date <= "2018-05-01")


library(dslabs)
library(broom)
library(dplyr)
library(ggplot2)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train) + geom_smooth()

## Correct Answer below
mnist_27$train %>%
    mutate(y = ifelse(y == "7", 1, 0)) %>%
    ggplot(aes(x_2, y)) +
    geom_smooth(method = "loess")

## Matrices
x <- matrix(rnorm(100 * 10), 100, 10)

# In R matrix can be defined as series of vectors of same size each forming a column. eg.
x_1 <- 1:5
x_2 <- 6:10
x <- cbind(x_1, x_2)
## Creating a Matrix from a Vector
my_vector <- 1:15
mat <- matrix(my_vector, 3, 5, byrow = TRUE)
t(mat)
#Q3 and Q4: Scalar
x <- x + seq(nrow(x))
x <- sweep(x, 1, 1:nrow(x), "+")
# Columns
x <- sweep(x, 2, 1:ncol(x), FUN = "+")
# averages of rows and columns
rowMeans(x)
colMeans(x)

# Zero out all values in matrix that are between 6 and 12
mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat

# Reading the mnist dataset: 60,000 digits
mnist <- read_mnist()
# taking first 1000 predictors and 1000 labels
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

# Q6
images <- mnist$train$images
new_images <- ifelse(images > 50 & images < 205, 1, 0)
RowSum <- rowMeans(new_images)
mean(RowSum)

# Putting 3rd image from mnist into a 28x 28 matrix (this number is 4)
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid) # Flipped because R plots first pixel at the bollom
# to flip image
image(1:28, 1:28, grid[,28:1])
sums <- rowSums(grid)
avgs <- rowMeans(grid)
# Remove uninformative predictors from our Matrix
new_x <- x[, colSds(x) > 60]
dim(new_x)
