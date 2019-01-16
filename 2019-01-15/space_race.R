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
# PLOT SOMETHING ----------------------------------------------------------

# success rate by year and agency_type
# year by country

global <- launches %>%
  group_by(
    state_code,
    agency_type,
    launch_year,
    category
  ) %>%
  summarize(
    count = n()
  ) %>%
  spread(
    category,
    count,
    fill = 0     # implicit missing values need to be zero for success rate computation
  ) %>%
  mutate(
    success_rate = O / (O + F)
  ) %>% 
  ungroup %>%
  group_by(
    state_code,
    agency_type
  ) %>%
  mutate(
    country_start = min(launch_year),
    experience = launch_year - country_start,
    persistence = n()
  ) 


mod <- lm(success_rate ~ experience + agency_type + state_code, data = global)
summary(mod)

global %>%
  ggplot(
    aes(experience, success_rate)
  ) + 
  geom_point(aes(color = agency_type), alpha = 0.2) + 
  geom_smooth(aes(color = agency_type), method = "loess", se = FALSE) 

launches %>%
  group_by(
    state_code,
    agency_type
  ) %>%
  mutate(
    country_start = min(launch_year),
    experience = launch_year - country_start,
    persistence = n(),
    launch_success = ifelse(category == "O", 1, 0)
  ) %>%
ggplot(
    aes(experience, launch_success, color = agency_type)
  ) +
  geom_smooth(se = FALSE)
