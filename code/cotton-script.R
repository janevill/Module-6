########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----

cotton <- read_csv("data/cotton-usda-nass.csv")
str(cotton)
head(cotton)
tail(cotton)
view(cotton)
dim(cotton)
summary(cotton)

# 3.1. Create a NC data subset ----

cotton %>%
  filter(state== "NORTH CAROLINA")%>%
  select(year,state,ag_district,county,data_item,value) -> nc_cotton
  

# 3.2. Divide the data_item column ----
nc_cotton %>%
  separate(data_item, into = c("cotton_type", "measurement"), sep = " - ") %>%
  filter(value != "(D)") -> nc_cotton

# 3.3. Convert the value column to numeric type ----

nc_cotton$value <- as.numeric(nc_cotton$value)

# 4. Visualizing trends ----
##visualize cotton yield and area harvested trends across NC ag districts
##How have yield and area harvested of cotton changed across all of the NC agricultural districts over time? 
ggplot(nc_cotton, mapping = aes(x=year, y=value))+
  geom_point()+
  facet_grid(measurement ~ ag_district, scales = "free_y")+
  theme_minimal()+
  labs(title="Cotton Production in NC", caption = "Source: USDA NASS", x = "Year", y= " ")+
  theme(axis.text.x = element_text(angle=90))
ggsave("outputs/cotton_yield_area_harvested.pdf")

# 5. Summarize data from 2018 ----
## What were the top 3 cotton producing counties in NC in terms of total lbs of cotton for 2018?
nc_cotton %>%
  filter(year == "2018") %>%
  spread(key =measurement, value = value)-> nc_cotton2018
nc_cotton2018 %>%
  mutate(total_lbs=nc_cotton2018$`YIELD, MEASURED IN LB / ACRE`*nc_cotton2018$`ACRES HARVESTED`) %>%
  mutate(rank=min_rank(desc(total_lbs))) %>%
  arrange(rank) -> nc_cotton2018
top_3_cotton_counties_2018 <- nc_cotton2018 %>%
  filter(rank <=3)
  
