library(tidyverse)
library(dplyr)
library(ggplot2)
library(stats)

data <- read.csv('/Users/emmabluman/Desktop/dew_point_pitching/data.csv')
data <- data %>% filter(PITCH_TYPE_TRACKED_KEY != 'UN')

data %>% filter(PITCH_TYPE_TRACKED_KEY == 'CB') %>% 
ggplot(aes(x = SPIN_RATE_ABSOLUTE, color = THROW_SIDE_KEY)) + 
  geom_density() 

data %>% filter(PITCH_TYPE_TRACKED_KEY == 'FB') %>% 
  ggplot(aes(x = SPIN_RATE_ABSOLUTE, color = THROW_SIDE_KEY)) + 
  geom_density() 

data %>% filter(PITCH_TYPE_TRACKED_KEY == 'FB') %>% 
  ggplot(aes(x = SPIN_RATE_ABSOLUTE, color = THROW_SIDE_KEY)) + 
  geom_density() 

pitcher_data <- data %>% group_by(PITCHER_KEY, THROW_SIDE_KEY, PITCH_TYPE_TRACKED_KEY) %>% 
  summarize(n = n(), pitcher_spin = mean(SPIN_RATE_ABSOLUTE), pitcher_sd = sd(SPIN_RATE_ABSOLUTE)) 


pitcher_handidness_data <-
  left_join(pitcher_data, handidness_data, 
            by = c("PITCH_TYPE_TRACKED_KEY", "THROW_SIDE_KEY")) %>% 
  mutate(spin_dev = pitcher_spin - handidness_spin)


spin_by_pitch <- 
  left_join(data, pitcher_data, by = c("PITCH_TYPE_TRACKED_KEY", "THROW_SIDE_KEY", "PITCHER_KEY")) %>% 
  select(PID, PITCHER_KEY, THROW_SIDE_KEY, PITCH_TYPE_TRACKED_KEY, pitcher_sd, pitcher_spin, SPIN_RATE_ABSOLUTE) %>% 
  mutate(spin_dev = pitcher_spin - SPIN_RATE_ABSOLUTE)

model <- lm(PotentialDewPoint ~ SPIN_RATE_ABSOLUTE + HORIZONTAL_BREAK +  INDUCED_VERTICAL_BREAK + PITCHER_KEY, data = data)
