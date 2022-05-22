library(ggplot2)
library(dplyr)
library(stringr)
library(usmap)
county_shape <- map_data("county")
map_data_jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true", stringsAsFactors = FALSE)

map_data_jail_recent_race <- map_data_jail %>%
                             filter(year == max(year)) %>%
                             select(state,county_name,total_pop,black_jail_pop,latinx_jail_pop)
                                  
map_jail <- map_data_jail_recent_race %>%
                             mutate(county = tolower(str_sub(map_data_jail_recent_race$county_name,1,nchar(map_data_jail_recent_race$county_name)-7)))

map_data_race <- full_join(map_data_jail_recent_race,county_shape,by = c("county" = "subregion"))

map_shape <- left_join(county_shape, map_jail, by = c("subregion" = "county"))
ggplot(map_shape)+
  geom_polygon(mapping = aes(x = long,
                             y = lat,
                             group = group,
                             fill = black_jail_pop)) +
  coord_map()

