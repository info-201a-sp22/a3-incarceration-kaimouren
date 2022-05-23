library(ggplot2)
library(dplyr)
library(stringr)
pris_pop_s <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true", stringsAsFactors = FALSE)
jail_pop_s <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true", stringsAsFactors = FALSE)

options(scipen=100, digits=4)

summary <- list()

summary$total_pop <- pris_pop_s %>%
  filter(year == 2013) %>%
  summarize(total_pris_pop = sum(total_prison_pop,na.rm=TRUE)) %>%
  pull(total_pris_pop)

summary$total_pris_black <- pris_pop_s %>%
  filter(year == 2013) %>%
  summarize(total_black_pop = sum(black_prison_pop,na.rm=TRUE)) %>%
  pull(total_black_pop)

###question3
pris_county <- pris_pop_s %>% 
                  filter(year == 2013) %>%
                  filter(black_prison_pop == max(black_prison_pop,na.rm=TRUE)) %>%
                  pull(county_name)

summary$pris_county_numb <- pris_pop_s %>% 
  filter(year == 2013) %>%
  filter(black_prison_pop == max(black_prison_pop,na.rm=TRUE)) %>%
  pull(total_prison_pop)

###largest prison population by state
pris_state <- pris_pop_s %>% 
                 filter(year == 2013) %>%
                 group_by(state) %>%
                 summarize(black_prison_pop_state = sum(black_prison_pop,na.rm = TRUE)) %>%
                 filter(black_prison_pop_state == max(black_prison_pop_state,na.rm=TRUE)) %>%
                 pull(state)

summary$pris_state_number <- pris_pop_s %>% 
                          filter(year == 2013) %>%
                          group_by(state) %>%
                          summarize(black_prison_pop_state = sum(black_prison_pop,na.rm = TRUE)) %>%
                          filter(black_prison_pop_state == max(black_prison_pop_state,na.rm=TRUE)) %>%
                          pull(black_prison_pop_state)

###question 1
plot_seven_years <- pris_pop_s %>%
  filter(year<2017) %>%
  group_by(year) %>%
  summarize(all_pop_pris = sum(total_prison_pop,na.rm = TRUE),
            all_black_prison = sum(black_prison_pop,na.rm = TRUE),
            all_white_prison = sum(white_prison_pop,na.rm = TRUE)) %>%
  mutate(ratio_black_total = all_black_prison/all_pop_pris,
         rati0_white_total = all_white_prison/all_pop_pris)

summary$avg_ratio_black <- plot_seven_years %>%
  summarize(mean_ratio_black = mean(ratio_black_total),
            mean_ratio_white = mean(rati0_white_total)) %>%
  summarize(rate_mean = mean_ratio_black/mean_ratio_white) %>%
  pull(rate_mean)

summary$blackJailPop <- jail_pop_s %>%
  filter(year < 2017) %>%
  summarize(blackPop_jail = mean(black_jail_pop,na.rm=TRUE)) %>%
  pull(blackPop_jail)

summary$whiteJailPop <- jail_pop_s %>%
    filter(year < 2017) %>%
  summarize(whitePop_jail = mean(white_jail_pop,na.rm=TRUE)) %>%
    pull(whitePop_jail)

###gender
summary$maleJailPop <- jail_pop_s %>%
  summarize(malePop_jail = sum(male_jail_pop,na.rm=TRUE)) %>%
  pull(malePop_jail)

summary$femaleJailPop <- jail_pop_s %>%
  summarize(femalePop_jail = sum(female_jail_pop,na.rm=TRUE)) %>%
  pull(femalePop_jail)

