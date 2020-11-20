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

# Question 3 Section 2.2 Run linear model predicting
# number of runs per game based on both the number of bases on balls per game
# and the number of home runs per game.
Dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, HR_per_game = HR / G, R_per_game = R / G)

Fit <- Dat %>% lm(R_per_game ~ BB_per_game + HR_per_game, data = .)
summary(Fit)


# Question 9 in 2.2
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_9901 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarise(mean_singles = mean(singles), mean_bb = mean(bb))
  
sum(bat_9901$mean_singles > 0.2)
sum(bat_9901$mean_bb > 0.2)

global <- inner_join(bat_02, bat_9901, by = "playerID")

cor(global$singles, global$mean_singles)
cor(global$bb, global$mean_bb)

# Scatterplots
global %>% ggplot(aes(singles, mean_singles)) + geom_point()
global %>% ggplot(aes(bb, mean_bb)) + geom_point()

# Fitting a linear model to predict 2002 singles given 1999-2001 mean_singles
fit <- global %>% lm(singles ~ mean_singles, data = .)
summary(fit)
fit$coef[2]

fit2 <- global %>% lm(bb ~ mean_bb, data = .)
summary(fit2)
fit2$coef[2]






