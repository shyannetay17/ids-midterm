library(tidyverse)
library(dplyr)

continents <- read.csv("continents-according-to-our-world-in-data.csv")
gdp_per_capita <- read.csv("gdp-per-capita-worldbank.csv")
youth_neet <- read.csv("youth-not-in-education-employment-training.csv")

neet <- left_join(youth_neet, continents,
                  by = c("Entity", "Code", "Year"))

neet <- neet %>% 
  rename(
    Country   = Entity,
    neet_share = `Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.`
  )

continent_lookup <- continents %>%
  filter(Year == 2015) %>%        # year where continent is filled
  select(Code, Continent) %>%     # only need these 2 columns
  distinct()                      # one row per Code

neet <- neet %>%
  select(-Continent) %>%                  # remove old NA-heavy Continent column
  left_join(continent_lookup, by = "Code")

neet_clean <- neet %>%
  filter(!is.na(Continent))

neet_clean <- neet_clean %>%           
  group_by(Country) %>%  
  arrange(Year, .by_group = TRUE) %>%  
  mutate(
    neet_growth = (neet_share - lag(neet_share)) / lag(neet_share) * 100
  ) %>%
  ungroup()

view(neet_clean)