## 2019-01-15 #TidyTuesday Project 
## Author: Michael O'Dell
## File create date: 2019-01-15
## Copyright 2019 Michael O'Dell
## License: MIT


# SET ENVIRONMENT ---------------------------------------------------------

library(tidyverse)



# LOAD DATA ---------------------------------------------------------------

agencies <- read_csv("../../../tidytuesday/data/2019/2019-01-15/agencies.csv")
launches <- read_csv("../../../tidytuesday/data/2019/2019-01-15/launches.csv")


# EXPLORE DATA ------------------------------------------------------------

dim(launches)
str(launches)
# check for missing values:
apply(apply(launches, 2, is.na), 2, sum)
# lots of missing agencies
# lots of missing variants--likely many vehicles do not have variants
# a few missing launch dates (but no missing years)
# some missing missions
apply(launches, 2, function(x) {length(unique(x))})
# tag has a unique entry for each row.
# launch and Julian dates are almost unique (several launches on some days)
# similarly missions map almost 1:1 with launches.
# the launch history spans 62 years
# there are 366 unique launch vehicles with 73 variants
# agency_type: private, startup, and state
# type: generic organization (O)
#       launch agency (LA), 
#       launch vehicle manufacturer (LV), 
#       satellite manufacturer (PL), 
#       rocket experimenter (RE), 
#       engine/motor manufacturer (E) 
#       launch site (LS)

apply(launches %>% select(-tag,
                          -JD,
                          -launch_date,
                          -mission), 2, table)

# all the missing agencies are state code SU (USSR)
launches %>% 
  filter(
    is.na(agency)
  ) %>% 
  select(
    state_code
  ) %>% table


dim(agencies)
str(agencies)
# check for missing values:
apply(apply(agencies, 2, is.na), 2, sum)
# nothing missing.
apply(agencies, 2, function(x) {length(unique(x))})
# INVESTIGATE: launches indicates 41 agencies. However, agencies list 74
# INVESTIGATE: agencies indicate 43 launches while launches indicates 5726
# agencies and launches agree on the number of state codes (17)
# USEFUL: agencies provide full names for agencies (launches$agency <> agencies$ucode)
# lots of missing short_english_names and english_names
# agency_type includes: private, startup, and state
# INVESTIGATE: do agencies and launches agency_types match?

apply(agencies, 2, table)
# lat and lon are blank so not very useful
# location looks to be the agency hq location-not the launch location.

# Why the discrepancy between files in agencies?
setdiff(launches$agency, agencies$ucode)  # US, BR, CH & others missing from agencies$ucode
agencies %>%
  filter(
    ucode %in% intersect(launches$agency, agencies$ucode)
  ) %>%
  select(
    ucode,
    name
  ) %>% print(n = Inf)
# commercial launch orgs are common to both files

agencies %>%
  select(
    ucode,
    name
  ) %>% print(n = Inf)

launches %>%
  select(
    agency,
    agency_type
  ) %>% unique %>% print(n = Inf)

## so launches seems to consolidate all agencies for a state under a single country code.
launches %>%
  filter(
    agency_type == "state"
  ) %>%
  select(
    agency,
    state_code
  ) %>% table
# state code SU has no ocurrances of an agency as noted above.


# Why the discrepancy between files in number of launches?
sum(agencies$count) - nrow(launches) # agencies is missing 20 launches
with(launches, table(category))


# Do agencies and launches agency_type match?
table(launches$agency_type)
agencies %>% 
  group_by(
    agency_type
  ) %>%
  summarize(
    count = sum(count)
  )
# nope. agencies is missing 20 launches over all categories.
# agencies does not seem to add value. Just use launces.

launches %>% 
  filter(
    state_code == "I-ESA"
  ) %>% select(type, mission, category, launch_year)


# POSE QUESTION AND PLOT FOR ANSWER ---------------------------------------

## Do different agency types have different learning curves?
## Hypothesis:  state agencies will be cautious and lead, 
##              private firms will build on state knowledge and contracts but will be
##                cautious given that they rely on state support
##              startups will be quick, move up the learning curve quickly but cut corners

# Over all states
launches %>%
  group_by(
    state_code,
    agency_type
  ) %>%
  mutate(
    country_start = min(launch_year),
    experience = launch_year - country_start,
    # persistence = n(),
    launch_success = ifelse(category == "O", 1, 0)
  ) %>%
  ggplot(
    aes(experience, launch_success, color = agency_type)
  ) +
  geom_smooth(se = FALSE) +
  labs(title = "Learning Curves by Agency Type\n(All Countries)", 
       x = "Experience (years)", y = "Annual Launch Success Rate",
       color = "Agency Type") +
  scale_y_continuous(labels = scales::percent)


# Just states with state, private, and start-ups
varied <- launches %>%
  select(
    state_code, 
    agency_type
  ) %>%
  unique %>%
  group_by(
    state_code
  ) %>%
  summarize(
    eco = n()
  ) 


launches %>%
  filter(
    state_code %in% (varied %>% filter(eco > 2) %>% select(state_code))
  ) %>%
  group_by(
    state_code,
    agency_type
  ) %>%
  mutate(
    country_start = min(launch_year),
    experience = launch_year - country_start,
    # persistence = n(),
    launch_success = ifelse(category == "O", 1, 0)
  ) %>%
  ggplot(
    aes(experience, launch_success, color = agency_type)
  ) +
  geom_smooth() +
  labs(title = paste0("Learning Curves by Agency Type\n(", 
                      (varied %>% filter(eco > 2) %>% select(state_code)), " only)"), 
       x = "Experience (years)", y = "Annual Launch Success Rate",
       color = "Agency Type") +
  scale_y_continuous(labels = scales::percent)



p <- launches %>%
  filter(
    state_code %in% (varied %>% filter(eco > 2) %>% select(state_code))
  ) %>%
  group_by(
    state_code,
    agency_type
  ) %>%
  mutate(
    country_start = min(launch_year),
    experience = launch_year - country_start,
    # persistence = n(),
    launch_success = ifelse(category == "O", 1, 0)
  ) %>%
  ggplot(
    aes(launch_year, launch_success, color = agency_type)
  ) +
  geom_smooth() +
  labs(title = paste0("Learning Curves by Agency Type\n(", 
                      (varied %>% filter(eco > 2) %>% select(state_code)), " only)"), 
       x = "Year", y = "Annual Launch Success Rate",
       color = "Agency Type") +
  scale_y_continuous(labels = scales::percent)


# learning curve by countries with state only; state + private; & state, private, + startup
launches %>%
  left_join(., varied, by = "state_code") %>%
  group_by(
    state_code
  ) %>%
  mutate(
    country_start = min(launch_year),
    experience = launch_year - country_start,
    launch_success = ifelse(category == "O", 1, 0)
  ) %>% 
  ggplot(
    aes(experience, launch_success, color = factor(eco))
  ) +
  geom_smooth() +
  labs(title = "Learning Curves by Agency Type Count\n(All Countries)", 
       x = "Experience (years)", y = "Annual Launch Success Rate",
       color = "Agency Type") +
  scale_y_continuous(labels = scales::percent)

# by year
launches %>%
  left_join(., varied, by = "state_code") %>%
  group_by(
    state_code
  ) %>%
  mutate(
    country_start = min(launch_year),
    experience = launch_year - country_start,
    launch_success = ifelse(category == "O", 1, 0)
  ) %>% 
  ggplot(
    aes(launch_year, launch_success, color = factor(eco))
  ) +
  geom_smooth() +
  labs(title = "Learning Curves by Agency Type Count\n(All Countries)", 
       x = "Year", y = "Annual Launch Success Rate",
       color = "Agency Type") +
  scale_y_continuous(labels = scales::percent)

file_name <- "tidytuesday_2019-01-15_mpo.jpeg"

jpeg(file = file_name, 
     width = 9, height = 6, units = "in", pointsize = 4, res = 300)

p

dev.off()


