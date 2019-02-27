library(tidyverse)
library(readxl)
library(readr)
library(gt)

elections = read_csv("ps_4_elections-poll-nc09-3.csv")

#There were X respondents who supported the Democratic candidate.
elections %>% 
  filter(response == "Dem") %>% 
  count()

#There were X more respondents who favored the Republican candidate than who were Undecided.
elections %>% 
  filter(response %in% c("Rep", "Und")) %>% 
  group_by(response) %>% 
  count()

# There are two gender variables (gender and gender_combined). 
#There are X individuals for whom these variables have different values.
elections %>% 
  select(gender, gender_combined) %>% 
  filter(gender != gender_combined) %>% 
  count()

#The first response of Rep came X minutes (rounded to the nearest minute) before the first response of Dem.
elections %>% 
  select(response, timestamp) %>% 
  filter(response %in% c("Dem", "Rep")) %>% 
  group_by(response) %>% 
  arrange(timestamp) %>% 
  slice(1)

#The first response of Rep came X minutes (rounded to the nearest minute) before the first response of Dem.
dem_time <- elections %>% 
  filter(response == "Dem") %>% 
  pull(timestamp)

rep_time <- elections %>% 
  filter(response == "Rep") %>% 
  pull(timestamp)

dem_time[1] - rep_time[1]

table <- elections %>%
  select(response, race_eth, final_weight) %>%
  group_by(response, race_eth) %>%
  filter(race_eth != "[DO NOT READ] Don't know/Refused") %>%
  summarize(total = sum(final_weight)) %>%
  spread(key = response, value = total, fill = 0) %>%
  mutate(total = Dem + Rep + Und) %>%
  mutate(Dem = Dem / total) %>%
  mutate(Rep = Rep / total) %>%
  mutate(Und = Und / total) %>%
  select(-total)

gt(table) %>% 
  as_raw_html() %>% as.character() %>% cat()