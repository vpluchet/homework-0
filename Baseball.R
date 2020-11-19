library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()


head(Teams)

# Scatterplot of the relationship between HRs and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Scatterplot of the relationship between stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Scatterplot of the relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Scatterplot of the relationship between at bat per game and runs per game
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  summarize(r = cor(AB_per_game, R_per_game)) %>% pull(r)

# Function to generate correlation coefficient for the charts
corr_eqn <- function(x,y, digits = 2) {
  corr_coef <- round(cor(x, y), digits = digits)
  corr_coef <- expression(paste(italic(r)," = ", corr_coef))
  return(corr_coef)
}

# scatterplot of win rate (number of wins per game) versus number of fielding errors (E) per game
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(Wins_per_game = W/G, Fielding_Errors_per_game = E/G) %>%
  ggplot(aes(Fielding_Errors_per_game, Wins_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(Wins_per_game = W/G, Fielding_Errors_per_game = E/G) %>%
  summarize(r = cor(Wins_per_game, Fielding_Errors_per_game)) %>% pull(r)


# scatterplot of triples (X3B) per game versus doubles (X2B) per game
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(Triples = X3B/G, Doubles = X2B/G) %>%
  ggplot(aes(Doubles,Triples)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(Triples = X3B/G, Doubles = X2B/G) %>%
  summarize(r = cor(Doubles, Triples)) %>% pull(r)








