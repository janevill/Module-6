########################################################################################
# Summary: Land application of nitrogen and phosphorus
# Date: September 25, 2019
# Data analyzed in this session are from the US Geological Survey and include total 
# nitrogen (N) and phosphorus (P) mass (kg) applied to the land surface via fertilizer
# manure, and atmospheric deposition.    
# Measurements are at the county-scale across the US. The total land areas (sq. km) over which 
# nutrients were applied are also reported per county. The data can be accessed from USGS 
# Scientific Investigations Report  2006-5012, "County-Level Estimates of Nutrient Inputs 
# to the Land Surface of the Conterminous United States, 1982-2001" 
# (https://pubs.usgs.gov/sir/2006/5012/) by Barbara C. Ruddy, David L. Lorenz, and David K. 
# Mueller.
########################################################################################

# Motivating question ----
### Which NC counties, on average over the period of record, have the highest rates of N and P input relative to different application types?

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# Read & inspect the dataset ----
nut <- read_csv("data/nutrient-inputs-reformatted.csv")
str(nut)
view(nut)
summary(nut)
## Tidy data ----
# 1. Gather and separate columns----

tidy_nut <- nut %>%
  gather(-State,-County,-FIPS,-Area,
         key = "year_nut_application",
         value="kg") %>%
  separate(year_nut_application, 
           into= c("year", "nut","application"),
           sep= "_") ->tidy_nut
  


# 2. Convert year to numeric----

tidy_nut$year <- as.numeric(tidy_nut$year)

# 3. Handle NAs-----
tidy_nut %>%
  drop_na()
tidy_nut %>%
  drop_na(kg)
tidy_nut %>%
  drop_na(Area)

##fill() - fills NAs with most recent value
tidy_nut %>%
  fill(kg, .direction = "down")
tidy_nut %>%
  fill(kg, .direction = "up")

##replace_na- specify value to fill in for nAs
tidy_nut %>%
  replace_na(list(kg=0))

# 4. Create a NC subset-----
tidy_nut %>%
  filter(State == "NC") ->NC_nut
head(NC_nut)

# Visualize ----
NC_nut %>%
  filter(County == "WAKE") %>%
  ggplot(mapping=aes(x=year,y=kg,color = application)) +
  geom_point()+
  geom_line()+
  theme_minimal()+
  facet_wrap(~ nut,ncol=1, 
             scales= "free_y")


# Summarize ----
### Which NC counties, on average over the period of record, have the highest rates of N and P input relative to different application types?
NC_nut %>%
  group_by(County,nut,application) %>%
  summarize(mean_kg = mean(kg)) %>%
  ungroup()%>%
  group_by(nut, application) %>%
  mutate(rank=min_rank(desc(mean_kg))) %>%
  arrange(rank)

