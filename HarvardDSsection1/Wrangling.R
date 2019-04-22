# Chapter -6 : Data Wrangling
library(dslabs)

path <- system.file("extdata", package = "dslabs")
list.files(path)
filename <- "murders.csv"
fullpath <- file.path(path, filename)
# Copy file over to your working directory
file.copy(fullpath, getwd())
file.exists(filename) #checks if the file is in working directory

file.copy(file.path(path, "murders.csv"), getwd())

library(tidyverse)
## Wide data and tidy data
new_tiny_dataset <- wide_data %>%
    gather(year, fertility, '1960':'2015') 
#gather(year, fertility, -country), will not gather the country data


df1 <- bind_cols(x=c("a", "b"), y=c("a", "a"))
df2 <- bind_cols(x=c("a", "a"), y= c("a", "b"))
setdiff(df1, df2)

## URL's dont work for the below examples
library(rvest)
url = "https://en.wikipedia.org/wiki/Murder_in_theUnited_States_by_state"
h <- read_html(url)
tab <- h %>% html_nodes("table")
tab <- tab[[2]]
tab <- tab %>% html_table

h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".o-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-ListItemText") %>% html_text()


animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)

str_replace("feet|foot|ft", "'")


animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "moo*"
str_detect(animals, pattern)

schools <- c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "University Georgia",
             "U California", "California State University")
schools %>%
    str_replace("^Univ\\.?\\s|^U\\.?\\s", "University") %>%
    str_replace("University ", "University of ")

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

converted <- problems %>%
    str_replace("feet|foot|ft", "'") %>%
    str_replace("inches|in|''|\"", "") %>%
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
converted[!index]

yes <- c("5 feet 7inches", "5 7 ")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>%
    str_replace("\\s*(feet|foot|ft)\\s*", "'") %>%
    str_replace("\\s*(inches|in|''|\")\\s*", "") %>%
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

## Case 1
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")
# Case 2-4
str_replace(s, "^([56])'?$", "\\1'0")
# Case 3
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"
# Case 5
yes <- c("1,7", "1, 8", "2, ")
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")
## Pultting all of tha above in a function
convert_format <- function(s) {
    s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>% #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}
## Function to convert word to numbers
words_to_numbers <- function(s) {
    str_to_lower(s) %>%
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}


s <- c("5'10", "6'1\"", "5'8inches", "5'7.5")
tab <- data.frame(x = s)
extract(data = tab, col = x, into = c("feet", "inches", "decimal"),
regex = "(\\d)'(\\d{1,2})(\\.\\d+)?")

## function that cleans up strings so that all the feet and inches formats use the same x'y format when appropriate.
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>%
    mutate(original = height,
         height = words_to_numbers(height) %>% convert_format()) %>%
         extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>%
         mutate_at(c("height", "feet", "inches"), as.numeric) %>%
         mutate(guess = 12 * feet + inches) %>%
         mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height / 2.54, smallest, tallest) ~ height / 2.54, #centimeters
    !is.na(height) & between(height * 100 / 2.54, smallest, tallest) ~ height * 100 / 2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
    select(-guess)
## We can check all the entries we converted using the following code:
new_heights %>%
    filter(not_inches(original)) %>%
    select(original, height) %>%
    arrange(height) %>%
    View()

filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
x <- str_split(lines, ",", simplify = TRUE)
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame(col_names) %>%
    setNames(col_names)

#dat <- data.frame(map_chr(x, 1), map_chr(x, 2), map_chr(x, 3), map_chr(x, 4), map_chr(x, 5)) %>%
#    setNames(c("state", "abb", "region", "population", "total"))

## Extracting data from PDF
library(dslabs)
data("research_funding_rates")
research_funding_rates
# Downloading Data
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)
the_names_1 <- the_names_1 %>%
    str_trim() %>%
    str_replace_all(",\\s.", "") %>%
    str_split("\\s{2,}", simplify = TRUE)
the_names_1

raw_data_research_funding_rates <- txt[2]
tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]
the_names_1 <- tab[3]
the_names_2 <- tab[4]
the_names_1 <- the_names_1 %>%
    str_trim() %>%
    str_replace_all(",\\s.", "") %>%
    str_split("\\s{2,}", simplify = TRUE)
the_names_1
the_names_2 <- the_names_2 %>%
    str_trim() %>%
    str_split("\\s+", simplify = TRUE)
the_names_2
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
    str_to_lower() %>%
    str_replace_all("\\s", "_")
the_names
new_research_funding_rates <- tab[6:14] %>%
    str_trim %>%
    str_split("\\s{2,}", simplify = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(the_names) %>%
    mutate_at(-1, parse_number)
new_research_funding_rates %>% head()
