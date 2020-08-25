###########################
## Abigail Porter
## 08-23-2020
## Lab 3
#########################

library(tidyverse)
library(sf)
library(units)
library(ggrepel)


region = data.frame(region = state.region, state_name = state.name)

***THIS part "South" will change in lab
conus = USAboundaries::us_states() %>% left_join(region) %>%
  filter(region == "South")

cities = readr::read_csv("data/uscities.csv")




***THIS part is how you make a plot
plot(conus)
