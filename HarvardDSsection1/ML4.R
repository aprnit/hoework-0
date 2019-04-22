# ML Example - School Data
set.seed(1986)
n <- round(2 ^ rnorm(1000, 8, 1))

set.seed(1)
mu <- round(80 + 2 * rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS", 1:100),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

# Top 10 schools
schools %>% top_n(10, quality) %>% arrange(desc(quality))

## Students take a Test
set.seed(1)
scores <- sapply(1:nrow(schools), function(i) {
    scores <- rnorm(schools$size[i], schools$quality[i], 30)
    scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))
sorted <- schools %>% arrange(desc(score))

## Plot average school vs school size
schools %>% ggplot(aes(size, score)) +
    geom_point(alpha = 0.5) +
    geom_point(data = filter(schools, rank <= 10), col = 2)

overall <- mean(sapply(scores, mean))
alpha <- 25
schools <- schools %>% mutate(deviation = score / ())


