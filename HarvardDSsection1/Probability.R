
# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p) {
    B <- 10000
    result <- replicate(B, {
        b_win <- sample(c(1, 0), 7, replace = TRUE, prob = c(1 - p, p))
        sum(b_win) >= 4
    })
    mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win. Call this object 'Pr'.
Pr <- sapply(p, prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p, Pr)


## Generate Normal Distribution 
B <- 10000
tallest <- replicate(B, {
    simulated_data <- rnorm(800, avg, sd)
    max(simulated_data)
})
mean(tallest >= 7 * 12)

# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation.
set.seed(1)

# Create an object called `highestIQ` that contains the highest IQ score from each random distribution of 10,000 people.
highestIQ <- replicate(B, {
    simulated_data <- rnorm(1000, 100, 15)
    max(simulated_data)
})

# Make a histogram of the highest IQ scores.
hist(highestIQ)

## Generate Random Variables
beads <- rep(c("red", "blue"), times = c(2, 3))
X <- ifelse(sample(beads, 1) == blue, 1, 0)


## Simulating a Roulette wheel
#Number of people playing
S <- 1000
# Roulette wheel has 18 red pockets, 18 black pockets and 2 green ones
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2))
# If red comes us the customer wins and casino loses one $, otherwise casino wins the draw and earns one $
n<-1000
X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9 / 19, 10 / 19))
S <- sum(X)

## Generate Random Variable function using Monte Carlo Simulation
B <- 10000
S <- replicate(B, {
    X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9 / 19, 10 / 19))
    S <- sum(X)
})
mean(S <= a)

## This is a Normal Distribution, therefore we can define the function just using its average and SD
mean(S) # average
sd(S) # SD
# Normal Density
s <- seq(min(S), max(S), length = 100)
normal_density <- data.frame(s = s, fix.empty.names = dnorm(s, mean(S), sd(S)))
data.frame(S = S) %>% ggplot2(aes(S, ..density..)) +
    geom_histogram(color = "black", binwidth = 10) +
    ylab("Probability") +
    geom_line(data = normal_density, mapping = aes(s, f), color = "blue")
## S+n/2 follows a binomial distribution



# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green + black + red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green = 1 - p_green

# Create a model to predict the random variable `X`, your winnings from betting on green. Sample one time.
X <- sample(c(-1, 17), 1, replace = TRUE, prob = c(p_not_green, p_green))

# Print the value of `X` to the console
X

## Standard Error
abs(17 - (-1)) * sqrt(p_green * p_not_green)

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green + black + red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 1000

# Create a vector called 'X' that contains the outcomes of 1000 samples
X <- sample(c(-1, 17), n, replace = TRUE, prob = c(p_not_green, p_green))

# Assign the sum of all 1000 outcomes to the variable 'S'
S <- sum(X)

# Print the value of 'S' to the console
S

# Compute the standard error of the sum of 1,000 outcomes
X <- sample(c(-1, 17), n, replace = TRUE, prob = c(p_not_green, p_green))
sqrt(n) * abs(17 - (-1)) * sqrt(p_green * p_not_green)


# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Define the number of bets using the variable 'n'
n <- 100

# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17 * p_green + -1 * p_not_green)

# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1) * sqrt(p_green * p_not_green)

# Using the expected value 'avg' and standard error 'se', compute the probability that you win money betting on green 100 times.
1 - pnorm(0, avg, se)



# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Define the number of bets using the variable 'n'
n <- 100

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S <- replicate(B, {
    X <- sample(c(17, -1), n, replace = TRUE, prob = c(p_green, p_not_green))
    sum(X)
})

# Compute the average value for 'S'
mean(S)

# Calculate the standard deviation of 'S'
sd(S)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Create a vector called `X` that contains the outcomes of `n` bets
X <- sample(c(17, -1), n, replace = TRUE, prob = c(p_green, p_not_green))

# Define a variable `Y` that contains the mean outcome per bet. Print this mean to the console.
Y <- mean(X)
Y

# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Compute the standard error of 'Y', the mean outcome per bet from 10,000 bets.
abs(17 - (-1)) * sqrt(p_green * p_not_green) / sqrt(10000)


## How Banks Set interest rates
n <- 1000
loss_per_foreclosure <- 200000
p <- 0.02
B <- 10000
losses <- replicate(B, {
    defaults <- sample(c(0, 1), n, prob = c(1 - p, p), replace = TRUE)
    #draws <- sample(c(x, loss_per_foreclosure), n, prob = c(1 - p, p), replace = TRUE)
    sum(defaults * loss_per_foreclosure)
})

# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
S <- sum(defaults) * loss_per_foreclosure
S

## using CLT
n*(p*loss_per_foreclosure + (1-p)*0)
sqrt(n) * abs(loss_per_foreclosure) * sqrt(p * (1 - p))
# Assign a variable `x` as the total amount necessary to have an expected outcome of $0
x <- -loss_per_foreclosure * p_default / (1 - p_default)

# Generate a variable `x` using `z`, `p_default`, `loss_per_foreclosure`, and `n`
x <- (-loss_per_foreclosure) * (n * p_default - z * (sqrt(n * p_default * (1 - p_default)))) / (n * (1 - p_default) + z * sqrt(n * p_default * (1 - p_default)))

# Convert `x` to an interest rate, given that the loan amount is $180,000. Print this value to the console.
x / 180000

