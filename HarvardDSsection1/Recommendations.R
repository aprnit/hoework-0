## Movie Recommendations

library(dslabs)
library(dplyr)
library(caret)
data("movielens")

## Looking at Data Summary
movielens %>% summarize(n_users = n_distinct(userId),
                        n_movies = n_distinct(movieId))

## Creating a Test Set
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE)
test_set <- movielens[test_index,]
train_set <- movielens[-test_index,]

test_set <- test_set %>% semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")

RMSE <- function(true_rating, predicted_ratings) {
    sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_hat <- mean(train_set$rating)
native_rmse <- RMSE(test_set$rating, mu_hat)

rmse_results <- data.frame(method = "Just the Average", RMSE = native_rmse)

mu <- mean(train_set$rating)
movie_avgs <- train_set %>% group_by(movieId) %>%
    summarize(b_i = mean(rating - mu))

## Using the 'Movie Effect Model'
predicted_ratings <- mu + test_set %>%
    left_join(movie_avgs, by = "movieId") %>%
    .$b_i

model_1_rmse <- RMSE(predicted_ratings, tes_set$ratings)
rmse_results <- bind_rows(rmse_results, data.frame(method = "Movie Effect Model", RMSE = model_1_rmse))
rmse_results %>% knitr::kable()

## User effect
user_avgs <- test_set %>%
    left_join(movie_avgs, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i))

## Predicting again and Calculating the residual Mean
predicted_ratings <- test_set %>%
    left_join(movie_avgs, by = "movieId") %>%
    left_join(user_avgs, by = "userId") %>%
    murate(pred = mu + b_i + b_u) %>%
    .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$ratings)
rmse_results <- bind_rows(rmse_results, data.frame(method = "Movie + User Effect Model", RMSE = model_2_rmse))

##Q1
library(ggplot2)
movielens %>% group_by(movieId, year) %>%
    summarize(n = n()) %>%
    ggplot(aes(x = year, y = sqrt(n), group = year)) +
    geom_boxplot()

## Q2
data("movielens")
sr <- movielens %>% filter(title == "Shawshank Redemption, The")
mean(sr$rating)

library(lubridate)
movielens <- mutate(movielens, date = as_datetime(timestamp))
movielens %>% filter(movieId == 356) %>% group_by(date) 

## Q3
movielens %>%
    filter(year >= 1993) %>%
    group_by(movieId) %>%
    summarize(n = n(), years = 2017 - first(year),
                title = title[1],
                rating = mean(rating)) %>%
                mutate(rate = n / years) %>%
                ggplot(aes(rate, rating)) +
                geom_point() +
                geom_smooth()

