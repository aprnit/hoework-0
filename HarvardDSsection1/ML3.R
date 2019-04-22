## MNIST Dataset
library(caret)
library(dslabs)
library(mboost)

set.seed(1)
data("mnist_27")

## Running Various Models
models <- c("glm", "lda", "naive_bayes", "svmLinear",
                "gamboost", "gamLoess", "qda",
                "knn", "kknn", "loclda", "gam",
                "rf", "ranger", "wsrf", "Rborist",
                "avNNet", "mlp", "monmlp",
                "adaboost", "gbm",
                "svmRadial", "svmRadialCost", "svmRadialSigma")

fits <- lapply(models, function(model) {
    print(model)
    train(y ~ ., method = model, data = mnist_27$train)
})

names(fits) <- models

fits_predicts <- sapply(fits, function(fits) {
    predict(fits, mnist_27$test)
})

confusionvector <- sapply(models, function(c) {
    confusionMatrix(factor(fits_predicts[, c]), mnist_27$test$y)$overall["Accuracy"]
})
mean(confusionvector)

## Ensemble
library(purrr)
df <- data.frame(matrix(unlist(fits_predicts), nrow = 23, byrow = T))
colnames(df) <- seq(1:200)
rownames(df) <- models
col_index <- seq(1, ncol(df), 1)
predict_vote <- sapply(col_index, function(j) {
    ifelse(sum(df[, j] == 7) > 12, 7, 2)
})

predict_vote <- as.factor(predict_vote) #  as factor
ensemble_accuracy <- confusionMatrix(predict_vote, mnist_27$test$y)$overall["Accuracy"]
#mean(confusionvector[which(confusionvector > ensemble_accuracy)])

#Q7
index_of_high <- which(confusionvector > 0.8)
new_fits <- fits[index_of_high]
new_models <- models[index_of_high]
new_predict_vote <- sapply(index_of_high, function(j) {
    ifelse(sum(df[, j] == 7) > 12, 7, 2)
})
new_predict_vote <- as.factor(predict_vote) #  as factor
confusionMatrix(new_predict_vote, mnist_27$test$y)$overall["Accuracy"]

## Second set of questions
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
pc <- prcomp(tissue_gene_expression$x)


