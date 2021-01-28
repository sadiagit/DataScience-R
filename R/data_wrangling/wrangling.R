library(dslabs)
library(tidyverse)
data(reported_heights)

head(reported_heights)
sd(reported_heights$height)
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

# demonstrate the effect of groups
str_detect(s, pattern_without_groups)

str_detect(s, pattern_with_groups)

filename <- "murders.csv"
path <- system.file("extdata", package = "dslabs")
file.location <- file.path(system.file("extdata", package = "dslabs"), "murders.csv")
file.destination <- file.path(getwd(), "data")
file.copy(file.location, file.destination)
file.destination
?read_csv
filepath = "~/RDSProject/R/data/wdbc.data"
dat <- read.csv("data/wdbc.data", header = FALSE)

d <- read_csv("data/times.csv")

head(d)

tidy_data <- d %>%
  gather( year,time, `2015`:`2017`)
tidy_data %>% spread(year, time)


library(tidyverse)
library(dslabs)


co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>%
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- co2_wide %>% gather(month, co2, -year)
