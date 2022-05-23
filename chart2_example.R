library(ggplot2)
library(tidyverse)
library(reshape2)

jail_pop <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true", stringsAsFactors = FALSE)

plot_four_years_gender <- jail_pop %>%
  filter(year>1977) %>%
  filter(year<2018) %>%
  mutate(four_year_interval = ifelse((year-2)%%4==0,year,year-(year-2)%%4)) %>%
  group_by(four_year_interval) %>%
  summarize(maleJailPop_total = sum(male_jail_pop,na.rm = TRUE),
            femaleJailPop_total = sum(female_jail_pop,na.rm = TRUE))

df_long <- melt(plot_four_years_gender, id.var = "four_year_interval")

df_long <- df_long %>% rename(gender = variable,population = value)

x_labels <- c(
  "1978-1981","1982-1985","1986-1989", "1990-1993", "1994-1997", "1998-2001", "2002-2005", "2006-2009", "2010-2013", "2014-2017"
)

ggplot(data=df_long, aes(fill=gender, y=population, x=four_year_interval)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_x_continuous(breaks = seq(1978, 2017, by = 4), labels = x_labels) +
  labs(
    title = "Number of People in Jail by Gender",
    x = "Time(four_year_interval)",
    y = "Population in Jail"
  )