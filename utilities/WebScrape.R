setwd("C:/dev/code/Woods2019/code")
library(rvest)
library(dplyr)
webpage = read_html("http://www.met.reading.ac.uk/~brugge/May2019.html")


h <- read_html("https://en.wikipedia.org/wiki/List_of_current_members_of_the_United_States_House_of_Representatives")

reps <- h %>%
  html_node("#mw-content-text > div > table:nth-child(18)") %>%
  html_table()

reps <- reps[,c(1:2,4:9)] %>%
  as_tibble()

metdata <- webpage %>%
  html_node("pre") %>%
  html_text()

