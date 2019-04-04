library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder %>% filter(continent == "Africa" & year == "2012") %>%
    ggplot(aes(fertility, life_expectancy, color = region)) +
    geom_point()


tab <- gapminder %>%
    filter(year %in% 1960:2010 & country %in% c("Vietnam", "United States"))

p <- tab %>%
    ggplot(aes(year, life_expectancy, color = country)) +
    geom_line()


gapminder %>%
    filter(year %in% 1960:2010 & country == "Cambodia") %>%
    ggplot(aes(year, life_expectancy)) +
    geom_line()

daydollars <- gapminder %>%
    mutate(dollars_per_day = gdp / population / 365) %>%
    filter(year == "2010" & continent == "Africa" & !is.na(dollars_per_day))
daydollars %>%
    ggplot(aes(x = dollars_per_day)) +
    geom_density() +
    scale_x_continuous(tran = "log2")


gapminder %>%
    mutate(dollars_per_day = gdp / population / 365) %>%
    filter(continent == "Africa" & year %in% c(1970, 2010) & !is.na(dollars_per_day)) %>%
    ggplot(aes(dollars_per_day)) +
    geom_density() +
    scale_x_continuous(trans = "log2") +
    facet_grid(year ~ .)

gapminder %>%
    mutate(dollars_per_day = gdp / population / 365) %>%
    filter(continent == "Africa" & year %in% c(1970, 2010) & !is.na(dollars_per_day)) %>%
    ggplot(aes(dollars_per_day, fill = region)) +
    geom_density(bw = 0.5, position = "stack") +
    scale_x_continuous(trans = "log2") +
    facet_grid(year ~ .)


gapminder_Africa_2010 <- gapminder %>%
    mutate(dollars_per_day = gdp / population / 365) %>%
    filter(continent == "Africa" & year == "2010" & !is.na(dollars_per_day))
gapminder_Africa_2010 %>%
    ggplot(aes(dollars_per_day, infant_mortality, color = region)) +
    geom_point()

gapminder_Africa_2010 %>% # your plotting code here
    ggplot(aes(dollars_per_day, infant_mortality, color = region)) +
    geom_point() +
    scale_x_continuous(trans = "log2")

gapminder_Africa_2010 %>% # your plotting code here
    ggplot(aes(dollars_per_day, infant_mortality, color = region)) +
    geom_point() +
    scale_x_continuous(trans = "log2") +
    geom_text(aes(label = country))


library(dplyr)
library(ggplot2)
library(dslabs)
data("murders")
murders %>% mutate(rate = total / population * 100000)
rate <- murders$total / murders$population * 100000
murders %>% mutate(rate = reorder(murders, rate, FUN = median)) %>%
    ggplot(aes(region, rate)) +
    geom_boxplot() +
    geom_point()

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)

the_disease = "Smallpox"
dat <- us_contagious_diseases %>%
    filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
    mutate(rate = count / population * 10000) %>%
    mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) +
    geom_tile(color = "grey50") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    ggtitle(the_disease) +
    ylab("") +
    xlab("")

library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>% filter(state == "California") %>%
    group_by(year, disease) %>%
    filter(weeks_reporting >= 10) %>%
    summarize(rate = sum(count) / sum(population) * 10000) %>%
    ggplot(aes(year, rate, color = disease)) +
    geom_line()