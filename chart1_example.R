library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)
pris_pop <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true",stringsAsFactors = FALSE)

plot_seven_years <- pris_pop %>%
  filter(year<2017) %>%
  group_by(year) %>%
  summarize(all_pop_pris = sum(total_prison_pop,na.rm = TRUE),
            all_black_prison = sum(black_prison_pop,na.rm = TRUE),
            all_white_prison = sum(white_prison_pop,na.rm = TRUE)) %>%
  mutate(blackPrisPopRace = all_black_prison/all_pop_pris,
         whitekPrisPopRace = all_white_prison/all_pop_pris)

plot_seven_years <- plot_seven_years %>%
  select(year, blackPrisPopRace, whitekPrisPopRace)

df_long <- melt(plot_seven_years, id.var = "year")

df_long <- df_long %>% rename(race = variable,pop_ratio = value)


x_labels <- c(
  "1970","1977","1984", "1991", "1998", "2005", "2012"
  )

ggplot(df_long,) +
  geom_line(aes(x = year,y = pop_ratio, color = race)) +
  scale_x_continuous(breaks = seq(1970, 2016, by = 7), labels = x_labels) +
  labs(
    title = "Prison Population Ratio by Race",
    x = "Year",
    y = "Ratio"
  )
