library(ggplot2)
library(dplyr)

pris_pop <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true",stringsAsFactors = FALSE)

plot_seven_years <- pris_pop %>%
  filter(year<2017) %>%
  group_by(year) %>%
  summarize(all_pop_pris = sum(total_prison_pop,na.rm = TRUE),
            all_black_prison = sum(black_prison_pop,na.rm = TRUE),
            all_white_prison = sum(white_prison_pop,na.rm = TRUE)) %>%
  mutate(ratio_black_total = all_black_prison/all_pop_pris,
         rati0_white_total = all_white_prison/all_pop_pris)
  

x_labels <- c(
  "1970","1977","1984", "1991", "1998", "2005", "2012"
  )

ggplot(
  plot_seven_years,
  aes(x = year, y = ratio_black_total/rati0_white_total)
) +
  geom_line(color = "red") +
  scale_x_continuous(breaks = seq(1970, 2016, by = 7), labels = x_labels) +
  labs(
    title = "Ratio of blacks to whites as a percentage of total prison population",
    x = "Rate",
    y = "Time(year)"
  )
