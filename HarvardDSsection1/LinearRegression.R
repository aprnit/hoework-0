## Linear Regression
library(dslabs)
library(tidyverse)
library(Lahman)
ds_theme_set()
?Teams

library(HistData)
data(Galton)
rss <- function(beta0, beta1, data) {
    resid <- Galton$child - (beta0 + beta1 * Galton$parent)
    return(sum(resid^2))
}

beta1 = seq(0, 1, len = nrow(Galton))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() +
    geom_line(aes(beta1, rss), col = 2)

fit <- lm(child ~ parent, data = Galton)
fit


TeamData <- Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(HR = round(HR / G, 1), BB = BB / G, R = R / G)
fit <- lm(R ~ BB+HR, data = TeamData)
fit

B <- 1000
N <- 100
lse <- replicate(B, {
    sample_n(Galton, N, replace = TRUE) %>%
    lm(child ~ parent, data = .) %>% .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])
library(ggplot2)
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) +
    geom_histogram(binwidth = 5, color = "black")
p2 <- lse %>% ggplot(aes(beta_1)) +
    geom_histogram(binwidth = 0.1, color = "black")
grid.arrange(p1, p2, ncol = 2)

Galton %>% ggplot(aes(child, parent)) +
    geom_point() +
    geom_smooth(method = "lm")


dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(HR = round(HR / G, 1), BB = BB / G, R = R / G) %>%
    select(HR, BB, R)

dat %>% group_by(HR) %>% head()

library(broom)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(HR = HR / G,
         R = R / G) %>%
         select(lgID, HR, BB, R)
dat %>%
    group_by(lgID) %>%
    do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>%
    filter(term == "HR")
dat %>%
    group_by(lgID) %>%
    do(glance(lm(R ~ HR, data = .)))
dat %>%
    do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>%
    filter(term == "HR")
dat %>%
    group_by(lgID) %>%
    do(mod = lm(R ~ HR, data = .))
