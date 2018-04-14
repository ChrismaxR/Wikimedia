
# Wikimedia data science assignment ---------------------------------------
# https://github.com/wikimedia-research/Discovery-Hiring-Analyst-2016


# Packages ----------------------------------------------------------------

library('tidyr')
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

# Visualise

rate <- click.through.rate %>%
  filter(!is.na(date)) 
  
p1 <- ggplot(rate) +
         geom_bar(aes(date, rate))
  

## Q2: Which results do people tend to try first? How does it change day-to-day? -----------------------------------

# frequency table: chosen result number per day
result.choice <- events_log %>% 
  filter(action == "visitPage") %>% 
  group_by(date, result_position) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  spread(result_position, n, fill = 0) %>% 
  select(1:21) # filter columns to result_position = 20

# Visualise
# heatmap <- ggplot(result.choice1, aes(variable, date))

  

# Q3: What is our daily overall zero results rate? How does it vary between the groups? -------------------------

## zero results rate: the proportion of searches that yielded 0 results

zero.results.rate <- events_log %>% 
  filter(action == "searchResultPage") %>% 
  group_by(date, group) %>% 
  summarise(zero = sum(n_results == 0),
            non.zero = sum(n_results > 0),
            total = n()) %>% 
  mutate(rate = zero/total * 100)

# Visualise

# Q4: Let session length be approximately the time between the first event and the last event in a session. 
# Choose a variable from the dataset and describe its relationship to session length. Visualize the relationship.



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

