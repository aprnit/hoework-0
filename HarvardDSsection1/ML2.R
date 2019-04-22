## 2nd Part of Machine Learning
# Distance, Knn, Cross-validation, and Generative Models 
library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
## distance
d <- dist(tissue_gene_expression$x)
d_mat <- as.matrix(d)[1:189, 1:189]
image(d_mat)


set.seed(1)
ks <- seq(1, 101, 3)
F1 <- sapply(ks, function(n) {
    test_index <- createDataPartition(heights$height, times = 1, p = 0.5, list = FALSE)
    train_set <- heights %>% slice(-test_index)
    test_set <- heights %>% slice(test_index)
    knn_fit <- knn3(sex ~ height, data = train_set, k = n)
    y_hat <- predict(knn_fit, test_set, type = "class")
    F_val <- F_meas(data = y_hat, reference = test_set$sex)
})
#plot(F1, ks)
ks[which.max(F1)]
max(F1)
ks[which.max(F1)]

library(dslabs)

data("tissue_gene_expression")

x <- tissue_gene_expression$x

y <- tissue_gene_expression$y

set.seed(1)
train_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
train_set_x <- x[train_index,]
test_set_x <- x[-train_index,]
train_set_y <- y[train_index]
test_set_y <- y[-train_index]

knn_fit <- knn3(train_set_x, train_set_y, k = 11) #modify k per assignment
y_hat <- predict(knn_fit, test_set_x, type = "class")
confusionMatrix(data = y_hat, reference = test_set_y, mode = "everything")$overall["Accuracy"]

## Cross Validation
library(dslabs)
library(caret)
library(dplyr)
library(ggplot2)
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n * p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[, sample(p, 100)]
fit <- train(x_subset, y, method = "glm")
fit$results

##
library(genefilter)
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n * p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
tt <- colttests(x, y)
pvals <- tt$p.value
ind <- which(pvals <= 0.01)
x_subset <- x[,ind]
fit <- train(x_subset, y, method = "glm") # train can use df or matrices
fit$results




## Bootstrap Distribution
B < 10 ^ 5
M_Stars <- replicate(B, {
    X_star <- sample(X, N, eplace = TRUE)
    M_star <- median(X_star)
})
quantile(M_star, c(0.05, 0.95))

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
#set.seed(1995)
indexes2 <- createResample(mnist_27$train$y, times=length(mnist_27$train$y))

#Q4, 5
n_val <- 100
B <- 10 ^ 4
set.seed(1)
Q75s <- replicate(B, {
    y_hat <- rnorm(n = n_val, mean = 0, sd = 1)
    q75 <- quantile(y_hat, 0.75)
})
#par(mfrow = c(1, 2))
#hist(Q75s)
#qqno
rm(Q75s)
#qqline(Q75s)
mean(Q75s)
sd(Q75s)

set.seed(1)
y<-Q75s
# Q4
ind10 <- createResample(y, 10, list = FALSE)
df10 <- as.data.frame(as.table(ind10))
df10 <- df10 %>% mutate(y = y[ind10])
Q_stars <- df10 %>% group_by(Var2) %>% summarize(Q_star = quantile(y, 0.75))
mean(Q_stars$Q_star)
sd(Q_stars$Q_star)

## other solution
library(purrr)
n_val <- 100
B <- 10000
set.seed(1)
y <- rnorm(n = n_val, mean = 0, sd = 1)
set.seed(1)
indexes <- createResample(y, B)
Q75s <- map_dbl(indexes, function(ind) {
    y_hat <- y[ind]
    q75 <- quantile(y_hat, 0.75)
})
mean(Q75s)
sd(Q75s)

## LDA
set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind,]
x <- x[, sample(ncol(x), 10)]
train_lda <- train(x, y, method = "lda", preProcessing = "center")
train_lda$finalModel$means
# LDA 2
set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
train_lda <- train(x, y, method = "lda", preProcessing = "center")
train_lda$results$Accuracy

## QDA
set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind,]
x <- x[, sample(ncol(x), 10)]
train_qda <- train(x, y, method = "qda")
train_qda$results$Accuracy
train_qda$finalModel$means

## Decision Tree
library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat)
# Plot the decision tree
library(rpart.plot)
rpart.plot(fit)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
##
dat %>%
    mutate(y_hat = predict(fit)) %>%
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = 2)

## Random Forest
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodes = 50, max = 25)
    dat %>%
    mutate(y_hat = predict(fit)) %>%
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = 2)
plot(fit)


## Parameter Tuning
library(randomForest)
library(tidyverse)
library(Rborist)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
set.seed(1)
fit <- train(y ~ x, method = "Rborist", tuneGrid = data.frame(predFixed = 1, minNode = 50), data = dat)
library(caret)
dat %>%
    mutate(y_hat = predict(fit)) %>%
    ggplot() +
    geom_point(aes(x, y)) +
    geom_smooth(aes(x, y_hat), col = 2)


set.seed(1991)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
fit <- train(x, y, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)), control = rpart.control(minsplit = 0))
#fit <- train(x, y, method = "rf", tuneGrid = data.frame(mtry = seq(50, 200, 25)), nodesize=1)
#plot(fit)
fit$bestTune
#confusionMatrix(fit)
plot(fit$finalModel)
text(fit$finalModel)
tree_terms <- as.character(unique(fit$finalModel$frame$var[!(fit$finalModel$frame$var == "<leaf>")]))
tree_terms

set.seed(1991)
library(randomForest)
fit <- with(tissue_gene_expression,
                train(x, y, method = "rf",
                      nodesize = 1,
                      tuneGrid = data.frame(mtry = seq(50, 200, 25))))
imp <- varImp(fit)
arrange(imp$importance, desc(Overall))

tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
# the list of all the genes and their importance imp$importance). You order it by importance desc to compute the 
# rank of each predictors. You search your CFHR4 gene in this list to have its importance and its global rank.

var_df <- as.data.frame(varImp(fit)$importance) %>%
      rownames_to_column('gene') %>%
      arrange(desc(Overall))
var_df