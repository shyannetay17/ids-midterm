continents <- read.csv("continents-according-to-our-world-in-data.csv")
gdp_per_capita <- read.csv("gdp-per-capita-worldbank.csv")
youth_neet <- read.csv("youth-not-in-education-employment-training.csv")

gdp <- left_join(gdp_per_capita, continents, 
                 by = c("Entity", "Code", "Year"))
neet <- left_join(youth_neet, continents,
                  by = c("Entity", "Code", "Year"))
view(gdp)
view(neet)

#rename columns
gdp <- gdp %>%
  rename(
    Country = Entity,
    GDP_pc  = `GDP.per.capita..PPP..constant.2017.international...`
  )

#continent-map
library(dplyr)

continents_map <- continents %>%
  group_by(Entity, Code) %>%
  summarise(
    Continent = first(na.omit(Continent)),
    .groups = "drop"
  )

#joining gdp per capita with continent data set
gdp <- gdp_per_capita %>%
  left_join(continents_map, by = c("Entity", "Code"))
view(gdp)

#remove rows with code - NA
gdp <- gdp %>% 
  filter(Code != "", !is.na(Code))
view(gdp)

#remove the rows with country = world
gdp <- gdp %>% 
  filter(Country != "World", Code != "OWID_WRL")


#calculate gdp per capita growth per country
library(dplyr)
gdp <- gdp %>%           
  group_by(Country) %>%  
  arrange(Year, .by_group = TRUE) %>%  
  mutate(
    gdp_growth = (GDP_pc - lag(GDP_pc)) / lag(GDP_pc) * 100
  ) %>%
  ungroup()

#calculate gdp per capita growth per continent
gdp_continent <- gdp %>%
  group_by(Continent, Year) %>%
  summarise(
    mean_growth = mean(gdp_growth, na.rm = TRUE),
    .groups = "drop"
  )
view(gdp_continent)