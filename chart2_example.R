library(ggplot2)
library(tidyverse)
library(reshape2)

jail_pop <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true", stringsAsFactors = FALSE)

plot_seven_years_gender <- jail_pop %>%
  mutate(seven_year_interval = ifelse((year-3)%%7==0,year,year-(year-3)%%7)) %>%
  group_by(seven_year_interval) %>%
  summarize(all_male_pop_15to64 = sum(male_pop_15to64,na.rm = TRUE),
            all_female_pop_15to64 = sum(female_pop_15to64,na.rm = TRUE))

df_long <- melt(plot_seven_years_gender, id.var = "seven_year_interval")

df_long <- df_long %>% rename(gender = variable,population = value)

x_labels <- c(
  "1970-1976","1977-1983","1984-1990", "1991-1997", "1998-2004", "2005-2011", "2012-2018"
)

ggplot(data=df_long, aes(fill=gender, y=population, x=seven_year_interval)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_x_continuous(breaks = seq(1970, 2018, by = 7), labels = x_labels) +
  labs(
    title = "Number of People in Prison by Gender",
    x = "Time (seven_year_interval)",
    y = "Population in Jail"
  )
w