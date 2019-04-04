## Chapter 4
library(tidyverse)
library(dslabs)
ds_theme_set()
take_poll(25)

# `N` represents the number of people polled
N <- 25
# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length.out = 100)
# Create a variable `se` that contains the standard error of each sample average
se <- sqrt(p * (1 - p)) / sqrt(N)
# Plot `p` on the x-axis and `se` on the y-axis
plot(p, se)


# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)
# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)
# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.
for (i in sample_sizes) {
    se <- sqrt(p * (1 - p)) / sqrt(i)
    plot(p, se, ylim = c(0, max(se)))
}

# `N` represents the number of people polled
N <- 25
# `p` represents the proportion of Democratic voters
p <- 0.45
# Calculate the standard error of the spread. Print this value to the console.
2 * sqrt(p * (1 - p) / N)

X_hat <- 0.48
se <- sqrt(X_hat * (1 - X_hat) / 25)
se
# If we are to be not more than 1% point away from p
pnorm(0.01 / se) - pnorm(-0.01 / se)

## Monte Carlo Simulation for CLT
B <- 10000
N <- 1000
p <- 0.45
X_hat <- replicate(B, {
    X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
    mean(X)
})
mean(X_hat)
sd(X_hat)
library(gridExtra)
library(ggplot2)
p1 <- data.frame(X_hat = X_hat) %>% ggplot2(aes(X_Hat)) +
    geom_histogram(binwidth=0.05, color="black")
p2 <- data.frame(X_hat = X_hat) %>% ggplot2(aes(sample = X_hat)) +
    stat_qq(dparams = list(mean = mean(x = X_hat), sd = sd(X_hat))) +
    geom_abline() +
    ylab("X_hat") +
    xlab("Theoritical Normal")
grid.arrange(p1, p2, nrow=1)


## If we assume p between 0.35 and 0.65, and do polling of 100,000
N <- 100000
p <- seq(0, 1, length = 100)
SE <- sapply(p, function(x) 2 * sqrt(x * (1 - x) / N))
data.frame(p=p, SE=SE) %>% ggplot(aes(p, SE)) + geom_line()

# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.
take_sample <- function(p, N) {
    X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
    mean(X)
}

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45
# Define `N` as the number of people polled
N <- 100
# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p, N)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100
# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B, {
    p - take_sample(p, N)
})
# Calculate the mean of the errors. Print this value to the console.
mean(errors)

# Calculate the standard deviation of `errors`
sqrt(mean(errors ^ 2))

# Define `p` as a proportion of Democratic voters to simulate
p <- 0.45

# Define `N` as the sample size
N <- 100
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
# Define `X_bar` as the average sampled proportion
X_bar <- mean(X)
# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar * (1 - X_bar) / N)

N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p * (1 - p) / N)
plot(N, se)

# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.
se <- sqrt(p * (1 - p) / N)
1 - pnorm(0.5, p, se)

## Confidence Intervals
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temprature = as.numeric(nhtemp)) %>%
    ggplot(aes(year, temprature)) +
    geom_point() +
    geom_smooth() +
    gg_title("Average Yearly Temprature in New Haven")

## Monte Carlo Simulation
p <- 0.45
N <- 1000
inside <- replicate(B, { 
    X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
    X_hat <- mean(X)
    SE_hat <- sqrt(X_hat * (1 - X_hat) / N)
    #c(X_hat - 2 * SE_hat, X_hat + 2 * SE_hat)
    between(p, X_hat - 2 * SE_hat, X_hat + 2 * SE_hat)
})
mean(inside)

z <- sqrt(N) * 0.02 / 0.5
1 - (pnorm(z) - pnorm(-z))


# Load the data
library(dslabs)
library(dplyr)
data(polls_us_election_2016)
# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls <- polls_us_election_2016 %>%
    filter(enddate >= '2016-10-31' & state == "U.S.")
# How many rows does `polls` contain? Print this value to the console.
nrow(polls)
# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
N
# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat <- polls$rawpoll_clinton[1] / 100
X_hat
# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat * (1 - X_hat) / N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
Q <- qnorm(1 - 0.05 / 2)
ci <- c(X_hat - Q * se_hat, X_hat + Q * se_hat)

pollster_results <- polls %>%
    mutate(X_hat = rawpoll_clinton / 100,
 se_hat = sqrt(X_hat * (1 - X_hat) / samplesize),
 lower = X_hat - qnorm(0.975) * se_hat,
 upper = X_hat + qnorm(0.975) * se_hat) %>%
 select(pollster, enddate, X_hat, se_hat, lower, upper)

avg_hit <- pollster_results %>%
    mutate(hit = (lower <= 0.482 & upper >= 0.482)) %>%
    summarize(mean(hit))

# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>%
    mutate(d_hat = (rawpoll_clinton - rawpoll_trump) / 100) %>%
    filter(enddate >= '2016-10-31' & state == "U.S.")
# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
N
# For the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- polls$d_hat[1]
d_hat
# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat + 1) / 2
# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat = sqrt(X_hat * (1 - X_hat) / N)
# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
Q <- qnorm(1 - 0.05 / 2)
ci <- c(X_hat - Q * se_hat, X_hat + Q * se_hat)


# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>%
    mutate(d_hat = rawpoll_clinton / 100 - rawpoll_trump / 100)
# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
# For the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- polls$d_hat[1]
d_hat

# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat + 1) / 2

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2 * sqrt(X_hat * (1 - X_hat) / N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(d_hat - qnorm(0.975) * se_hat, d_hat + qnorm(0.975) * se_hat)


pollster_results <- polls %>%
 mutate(X_hat = (rawpoll_clinton - rawpoll_trump) / 100,
 se_hat = 2 * sqrt(X_hat * (1 - X_hat) / samplesize),
 lower = X_hat - qnorm(0.975) * se_hat,
 upper = X_hat + qnorm(0.975) * se_hat) %>%
 select(pollster, enddate, X_hat, se_hat, lower, upper)

pollster_results <- polls %>%
    mutate(X_hat = (d_hat + 1) / 2,
 se_hat = 2 * sqrt(X_hat * (1 - X_hat) / samplesize),
 lower = d_hat - qnorm(0.975) * se_hat,
 upper = d_hat + qnorm(0.975) * se_hat) %>%
 select(pollster, enddate, d_hat, lower, upper)

avg_hit <- pollster_results %>%
    mutate(hit = (lower <= 0.021 & upper >= 0.021)) %>%
    summarize(mean(hit))

library(ggplot2)
polls %>%
    mutate(errors = d_hat - 0.021) %>%
    ggplot(aes(x = pollster, y = errors)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

polls %>%
    mutate(errors = d_hat - 0.021) %>%
    group_by(pollster) %>%
    filter(n() >= 5) %>%
    ggplot(aes(x = pollster, y = errors)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Obama 2012 Poll
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d + 1) / 2
confidence_intervals <- sapply(Ns, function(N) {
    X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
    X_hat <- mean(X)
    SE_hat <- sqrt(X_hat * (1 - X_hat) / N)
    2 * c(X_hat, X_hat - 2 * SE_hat, X_hat + 2 * SE_hat) - 1
})
polls <- data.frame(poll = 1:ncol(confidence_intervals), t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

d_hat <- polls %>%
    summarize(avg = sum(estimate * sample_size) / sum(sample_size)) %>%
    .$avg
p_hat <- (1 + d_hat) / 2
moe <- 2 * 1.96 * sqrt(p_hat * (1 - p_hat) / sum(polls$sample_size))
moe # Margin of Error
round(d_hat * 100, 1)
round(moe * 100, 1)

## 2016 Election Polling: Clinton vs Trump
polls <- polls_us_election_2016 %>%
    filter(state = "U.S." & endate >= '2016-10-31' & (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))
polls < polls %>%
    mutate(spread = rawpoll_clinton / 100 - rawpoll_trump / 100)
# p: proportion voting for Clinton, spread d = 2p-1
# SE is 2*sqrt(p*(p-1)/N)
# Estimated spread
d_hat <- polls %>%
    summarize(d_hat = sum(estimate * sample_size) / sum(sample_size)) %>%
    .$d_hat
p_hat <- (1 + d_hat) / 2
moe <- 2 * 1.96 * sqrt(p_hat * (1 - p_hat) / sum(polls$sample_size))
moe # Margin of Error
round(d_hat * 100, 1)
round(moe * 100, 1)
## Histrogram of results
polls %>%
    ggplot(aes(spread)) +
    geom_histogram(color = "black", binwidth = 0.01)
## Important observation: the data does not seem to be normally distributed
polls %>% group_by(pollster) %>%
    filter(n() >= 6) %>%
    ggplot(aes(polster, spread)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

polls %>% group_by(pollster) %>%
    filter(n() >= 6) %>%
    summarize(se = 2 * sqrt(p_hat * (1 - p_hat) / median(samplesize)))

##Collect last result reported by each polsster
one_poll_per_polster <- polls %>% group_by(pollster) %>%
    filter(enddate == max(enddate)) %>%
    ungroup()
sd(one_poll_per_polster$spread)
results <- one_poll_per_polster %>%
    summarize(avg = mean(spread), se = sd(spread) / sqrt(length(spread))) %>%
    mutate(start = avg - 1.96 * se, end = avg + 1.96 * se)
round(results * 100, 1)

sigma <- polls %>%
summarize(s = sd(spread)) %>%
select(pollster, s)

# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `N` as the number of people measured
N <- 50
# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)
# Define `se` as the standard error of the estimate. Print this value to the console.
se <- sd(X) / sqrt(N)
se
# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
Q <- qnorm(1 - 0.05 / 2)
ci <- c(mean(X) - Q * se, mean(X) + Q * se)


res <- replicate(B, {
    X <- sample(x, N, replace = TRUE)
    interval <- c(mean(X) - qnorm(0.975) * sd(X) / sqrt(N), mean(X) + qnorm(0.975) * sd(X) / sqrt(N))
    between(mu, interval[1], interval[2])
})

# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")
# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>%
    filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research", "The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>%
           mutate(spread = rawpoll_clinton / 100 - rawpoll_trump / 100)
# Make a boxplot with points of the spread for each pollster
polls %>% ggplot(aes(x = pollster, y = spread)) +
    geom_boxplot() + geom_point()

sigma <- polls %>%
    summarize(s = sd(spread)) %>%
    select(pollster, s)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma <- polls %>%
    group_by(pollster) %>%
    summarize(s = sd(spread)) %>%
    select(pollster, s)


# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>%
    group_by(pollster) %>%
    summarize(average = mean(spread), sd = sd(spread), no_of_polls = n())

# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate <- res$average[2] - res$average[1]
estimate
# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(res$sd[2] ^ 2 / res$no_of_polls[2] + res$sd[1] ^ 2 / res$no_of_polls[1])
se_hat

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(mean(estimate) - qnorm(0.975) * se_hat, mean(estimate) + qnorm(0.975) * se_hat)


# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>%
    filter(state == "Florida" & enddate >= "2016-11-04") %>%
    mutate(spread = rawpoll_clinton / 100 - rawpoll_trump / 100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
results <- polls %>%
    summarize(avg = mean(spread), se = sd(spread) / sqrt(11))
results

# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results
sigma <- results$se

# Define a variable called `Y` that contains the average in the object `results`
Y <- results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B = sigma ^ 2 / (sigma ^ 2 + tau ^ 2)
B
# Calculate the expected value of the posterior distribution
B * mu + (1 - B) * Y

# Compute the standard error of the posterior distribution. Print this value to the console.
posterior_se <- sqrt(1 / (1 / sigma ^ 2 + 1 / tau ^ 2))
posterior_se

# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
posterior_mean <- B * mu + (1 - B) * Y
posterior_se <- sqrt(1 / (1 / sigma ^ 2 + 1 / tau ^ 2))
posterior_mean + c(-1.96, 1.96) * posterior_se

pnorm(0, posterior_mean, posterior_se)

# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc <- function() {
    B = sigma ^ 2 / (sigma ^ 2 + tau ^ 2)
    pnorm(0, B * mu + (1 - B) * Y, sqrt(1 / (1 / sigma ^ 2 + 1 / tau ^ 2)))
}

# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps <- sapply(taus, p_calc)

# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus, ps)

# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)
# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc <- function(tau) {
    B <- sigma ^ 2 / (sigma ^ 2 + tau ^ 2)
    pnorm(0, B * mu + (1 - B) * Y, sqrt(1 / (1 / sigma ^ 2 + 1 / tau ^ 2)))
}
# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps <- p_calc(taus)
# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus, ps)

# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>%
    filter(state != "U.S." & enddate >= "2016-10-31") %>%
    mutate(spread = rawpoll_clinton / 100 - rawpoll_trump / 100)

# Create an object called `cis` that has the columns indicated in the instructions
cis <- polls %>% mutate(X_hat = (spread + 1) / 2, se = 2 * sqrt(X_hat * (1 - X_hat) / samplesize),
                 lower = spread - qnorm(0.975) * se, upper = spread + qnorm(0.975) * se) %>%
                 select(state, startdate, enddate, pollster, grade, spread, lower, upper)


p_hits <- cis %>%
    mutate(hit = lower < actual_spread & upper > actual_spread) %>%
    summarise(proportion_hits = mean(hit))

p_hits <- cis %>%
    mutate(hit = lower < actual_spread & upper > actual_spread) %>%
    group_by(pollster) %>%
    filter(n() > 5) %>%
    summarise(proportion_hits = mean(hit)) %>%
    mutate(no_of_polls = n()) %>%
    arrange(desc(proportion_hits)) %>%
    select(pollster, proportion_hits, no_of_polls, grade)
p_hits <- cis %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>%
    group_by(pollster) %>%
    filter(n() >= 5) %>%
    summarize(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>%
    arrange(desc(proportion_hits))

p_hits %>% ggplot(aes(state, proportion_hits)) +
    geom_bar(stat = "identity") +
    coord_flip()

p_hits <- errors %>%
    group_by(state) %>%
    filter(n() > 5) %>%
    summarize(proportion_hits = mean(hit), n = n())

# Make a barplot of the proportion of hits for each state
p_hits %>%
    #reorder(state, proportion_hits) %>%
    ggplot(aes(x = state, y = proportion_hits)) +
    geom_bar(stat = "identity") +
    coord_flip()

z <- qt(0.975, nrow(one_poll_per_pollster) - 1) #N-1 degrees of freedom
one_poll_per_polster %>%
    summarize(avg = mean(spread), moe = z * sd(spread) / sqrt(length(spread))) %>%
    mutate(start = avg - moe, end = avg_hit + moe)

1 - pt(2, df = 3) + pt(-2, df = 3)

# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- seq(3, 50, 1)
# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func <- function(df) {
    1 - pt(2, df = df) + pt(-2, df = df)
}
# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs <- sapply(df, pt_func)
# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df, probs)

res <- replicate(B, {
    X <- sample(x, size = N, replace = TRUE)
    interval <- mean(X) + c(-1, 1) * qnorm(0.975) * sd(X) / sqrt(N)
    between(mu, interval[1], interval[2])
})

res <- replicate(B, {
    X <- sample(x, N, replace = TRUE)
    interval <- mean(X) + c(-1, 1) * qnorm(0.975) * sd(X) / sqrt(N)
    between(mu, interval[1], interval[2])
})

# The 'errors' data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals <- errors %>%
    filter(grade %in% c("A-", "C-")) %>%
    group_by(grade, hit) %>%
    summarize(num = n()) %>%
    spread(grade, num)

# Print the proportion of hits for grade A- polls to the console
totals[[2, 3]] / sum(totals[[3]])

# Print the proportion of hits for grade C- polls to the console
totals[[2, 2]] / sum(totals[[2]])

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test <- totals %>%
    select(-hit) %>%
    chisq.test()

# Print the p-value of the chi-squared test to the console
chisq_test$p.value

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- (totals[[2, 2]] / sum(totals[[2]])) /
    (totals[[1, 2]] / sum(totals[[2]]))

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- (totals[[2, 3]] / sum(totals[[3]])) /
    (totals[[1, 3]] / sum(totals[[3]]))

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A / odds_C