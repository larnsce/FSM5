# Header ---------------------

# R for analysis of JMP data published on washdata.org
# Created by Lars Schoebitz
# MIT Licence
# 2019-02-12

# Comments -------------------

# Load libraries --------------

library(RCurl)
library(tidyverse)
library(ggalt)

# Load data -------------

# x <- getURL("https://raw.githubusercontent.com/larnsce/sanitation_data/master/data/jmp/2019-02-12_jmp_2017_database_tidy_additional_variables.csv")

jmp_data <- read_csv(file = here::here("data/jmp_2017_database_tidy.csv"))

# explore data  ------

## get all countries that are in the database

jmp_countries <- jmp_data %>% 
  group_by(country) %>% 
  count() %>% 
  select(-n)
  
## calculate summaries for those with "emptied_and_treated" sub-indicator

subindicator_fs <- jmp_data %>% 
  filter(variable == "emptied_and_treated") %>% 
  filter(year == 2015) %>%
  filter(residence == "national") %>% 
  mutate(percent = factor(round(percent, 0))) %>% 
  select(country, iso3, percent)

subindicator_fs2 <- subindicator_fs %>% 
    bind_rows(
      filter(jmp_countries, !country %in% subindicator_fs$country)
    ) 

  
## --------------- Wastewater

subindicator_ww <- jmp_data %>% 
  filter(variable == "wastewater_treated") %>% 
  filter(year == 2015) %>% 
  filter(residence == "national") %>% 
  mutate(
    percent = cut(percent, breaks = c(0, 25, 50, 75, 100), labels = c("0% - 25%", ">25% - 50%", ">50% - 75%", ">75% - 100%"))
  ) 


subindicator_ww2 <- subindicator_ww %>% 
  bind_rows(
    filter(jmp_countries, !country %in% subindicator_ww$country)
  ) %>% 
  select(country, iso3, percent)


## prepare world maps

## good help: https://stackoverflow.com/questions/9558040/ggplot-map-with-l?noredirect=1&lq=1
  
world <- ggplot2::map_data("world")

world <- world %>% 
  filter(region != "Antarctica") %>% 
  as_tibble()

# add iso3 code -----------------------------

world_iso3 <- world %>% 
  
  ## add iso3
  
  mutate(
    iso3 = iso.alpha(world$region, n = 3)
  ) 

## prepare data for maps

  
## faecal sludge
world_subindicator_fs <- world_iso3 %>% 
  left_join(subindicator_fs2) 

world_subindicator_ww <- world_iso3 %>% 
  left_join(subindicator_ww2) 

# plot the map for faecal sludge

map_fs <- ggplot() +
  geom_cartogram(data = world, map = world, aes(x = long, y = lat, map_id = region))  +
  geom_cartogram(data = world_subindicator_fs, map = world, colour = "grey40", size = 0.2, aes(fill = percent, map_id = region)) +
  labs(x = NULL, y = NULL) +
  coord_proj("+proj=wintri") +
  scale_fill_viridis_d(name = "emptied and treated [%]", na.value = "grey80") +
  labs(title = "SDG 6.2.1 national sub-indicator estimates 2015", x = NULL, y = NULL) +
  theme_minimal(base_size = 16) +
  theme(axis.text = element_blank()) +
  #theme(legend.position = "bottom") +
  theme(plot.margin = unit(rep(4, 4), "pt")) +
  theme(legend.key = element_rect(size = 3, color = "white")) +
  theme(legend.key.size = unit(2, 'lines'))
  

map_ww <- ggplot() +
  labs(x = NULL, y = NULL) +
  geom_cartogram(data = world, map = world, aes(x = long, y = lat, map_id = region))  +
  geom_cartogram(data = world_subindicator_ww, map = world, colour = "grey40", size = 0.2, aes(fill = percent, map_id = region)) +
  coord_proj("+proj=wintri") +
  scale_fill_viridis_d(name = "emptied and treated [%]", na.value = "grey80") +
  labs(title = "SDG 6.2.1 national sub-indicator estimates 2015", x = NULL, y = NULL) +
  theme_minimal(base_size = 16) +
  theme(axis.text = element_blank()) +
  #theme(legend.position = "bottom") +
  theme(plot.margin = unit(rep(4, 4), "pt")) +
  theme(legend.key = element_rect(size = 3, color = "white")) +
  theme(legend.key.size = unit(2, 'lines'))



  