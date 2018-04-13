
# Wikimedia data science assignment ---------------------------------------
# https://github.com/wikimedia-research/Discovery-Hiring-Analyst-2016


# Packages ----------------------------------------------------------------

library('readr')
library('tidyverse')
library('lubridate')

setwd("C:/Users/chris/OneDrive/Rstudio/Wikimedia datascience")

# Get data ----------------------------------------------------------------

events_log <- read_csv("events_log.csv", 
                       col_types = cols(timestamp = col_character()))


# Clean data ------------------------------------------------------------

# dates
events_log$timestamp <- ymd_hms(events_log$timestamp)
events_log$date <- date(events_log$timestamp)

# arrange data
events_log <- events_log %>% 
  arrange(timestamp, session_id)


# Answering the Q's -------------------------------------------------------

# Q1: What is our daily overall clickthrough rate? How does it vary between the groups? ------------------------------
## click through rate:  the proportion of search sessions where the user clicked on one of the results displayed

click.through.rate <- events_log %>% 
  group_by(date, group) %>% 
  summarise(search = sum(action == "searchResultPage"),
            visit = sum(action == "visitPage")
  ) %>% 
  mutate(rate = visit/search * 100)


## Q2: Which results do people tend to try first? How does it change day-to-day? -----------------------------------

result.choice <- events_log %>% 
  filter(action == "visitPage") %>% 
  group_by(date) %>% 
  summarise(first = sum(result_position == 1, na.rm = T),
            second = sum(result_position == 2, na.rm = T),
            third = sum(result_position == 3, na.rm = T),
            fourth = sum(result_position == 4, na.rm = T),
            fifth = sum(result_position == 5, na.rm = T),
            sixth = sum(result_position == 6, na.rm = T),
            seventh = sum(result_position == 7, na.rm = T),
            eigth = sum(result_position == 8, na.rm = T),
            ninth = sum(result_position == 9, na.rm = T),
            tenth = sum(result_position == 10, na.rm = T),
            total= sum(result_position, na.rm = T)
            )

# Q3: What is our daily overall zero results rate? How does it vary between the groups? -------------------------

## zero results rate: the proportion of searches that yielded 0 results

zero.results.rate <- events_log %>% 
  filter(action == "searchResultPage") %>% 
  group_by(date, group) %>% 
  summarise(zero = sum(n_results == 0),
            non.zero = sum(n_results > 0),
            total = n()) %>% 
  mutate(rate = zero/total * 100)


# Data exploration ---------------------------------------------------------


## Overall

# number of sessions per day
sessions.day <- events_log %>% 
  group_by(date) %>% 
  summarise(sessions = n())

# number of events per day
sessions.events.day <- events_log %>% 
  group_by(date, action) %>% 
  summarise(sessions = n())

# Looking at number of individual sessions. 
test <- events_log %>%
  filter(session_id == "8ee241d030e9dd7c") %>%
  arrange(timestamp)

test2 <- events_log %>%
  filter(session_id == "96539ff273a06ee7") %>%
  arrange(timestamp)

test3 <- events_log %>%
  filter(session_id == "107c7917c8c667a5") %>%
  arrange(timestamp)


##Q1

# Looking at how rows look where action == "searchResultPage"
test4 <- events_log %>%
  filter(action == "searchResultPage")


## Q2

test5 <- events_log %>% 
  filter(action == "visitPage",
         result_position == 1
  ) %>% 
  arrange(timestamp) %>% 
  group_by(date) %>% 
  summarise(count = n())

test6 <- events_log %>% 
  filter(action == "visitPage",
         result_position > 1
  ) %>% 
  arrange(timestamp) %>% 
  group_by(date) %>% 
  summarise(count = n())


## Q3

# counting rows for "zero" and more than zero n-results 
non.zero <- events_log %>%
  filter(action == "searchResultPage",
         n_results > 0) %>%
  group_by(date) %>%
  summarise(sessions = n())

zero <- events_log %>%
  filter(action == "searchResultPage",
         n_results == 0) %>%
  group_by(date) %>%
  summarise(sessions = n())

