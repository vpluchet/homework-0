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

# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

# Graph
dat %>% ggplot(aes(BB, R)) + 
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    ggtitle("R vs BB for different HR values") +
    facet_wrap(~ HR)

# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# inspect a grouped tibble
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

# inspect data frame and tibble
Teams
as_tibble(Teams)
# Note that the function was formerly called as.tibble()

# subsetting a data frame sometimes generates vectors
class(Teams[,20])

# subsetting a tibble always generates tibbles
class(as_tibble(Teams[,20]))

# pulling a vector out of a tibble
class(as_tibble(Teams)$HR)

# access a non-existing column in a data frame or a tibble
Teams$hr
as_tibble(Teams)$HR

# create a tibble with complex objects
tibble(id = c(1, 2, 3), func = c(mean, median, sd))

# use do to fit a regression line to each HR stratum
dat %>%  
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))

# using do without a column name gives an error
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .))

# define a function to extract slope from lm
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

# return the desired data frame
dat %>%  
  group_by(HR) %>%
  do(get_slope(.))

# not the desired output: a column containing data frames
dat %>%  
  group_by(HR) %>%
  do(slope = get_slope(.))

# data frames with multiple rows will be concatenated appropriately
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             estimate = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}

dat %>%  
  group_by(HR) %>%
  do(get_lse(.))

testlm <- dat %>% lm(R~BB, data = .)
testlm
testlm$coefficients
testlm$coefficients[1]
testlm$coefficients[2]
class(testlm$coefficients)
summary(testlm)
class(summary(testlm))

# use tidy to return lm estimates and related information as a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
fit
tidy(fit)

# add confidence intervals with tidy
tidy(fit, conf.int = TRUE)

# pipeline with lm, do, tidy
# Without filtering
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE))

# With filtering
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

# make ggplots
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

# inspect with glance
glance(fit)
fit

# Using do to get three columns
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

dat %>% 
  group_by(HR) %>% 
  do(get_slope(.))

# You want to know whether the relationship between home runs and runs per game
# varies by baseball league.

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)

dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 

dat %>% 
  group_by(lgID) %>% 
  do(glance(lm(R ~ HR, data = .)))







