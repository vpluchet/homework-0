library(tidyverse)
library(broom)
library(Lahman)
library(dslabs)

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, HR_per_game = HR / G, R_per_game = R / G)

fitR <- Teams_small %>% lm(avg_attendance ~ R_per_game, data = .)
tidy(fitR)

fitHR <- Teams_small %>% lm(avg_attendance ~ HR_per_game, data = .)
tidy(fitHR)

fitW <- Teams_small %>% lm(avg_attendance ~ W, data = .)
tidy(fitW)

# Intercept also given by the following code
Teams_small %>% 
  lm(avg_attendance ~ W, data = .) %>% 
  .$coef %>%
  .[1]

fitY <- Teams_small %>% lm(avg_attendance ~ yearID, data = .)
tidy(fitY)

Teams_small %>% ggplot(aes(yearID, avg_attendance)) + geom_point() +
  geom_smooth(method = "lm")

cor(Teams_small$W, Teams_small$R_per_game)
cor(Teams_small$W, Teams_small$HR_per_game)

dat <- Teams_small %>%
    mutate(W_strata = round(W/10, 0)) %>%
    group_by(W_strata) %>%
    filter(W_strata %in% 5:10, n() >= 20) %>%
    ungroup()

sum(dat$W_strata == 8)

fit_strata <- dat %>% group_by(W_strata) %>%
  do(tidy(lm(avg_attendance ~ R_per_game, data = .))) %>%
  filter(term == "R_per_game")

fit_strata

# calculate slope of regression line after stratifying by R per game
dat %>%  
  group_by(W_strata) %>%
  summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(R/G))

fit_strata2 <- dat %>% group_by(W_strata) %>%
  do(tidy(lm(avg_attendance ~ HR_per_game, data = .))) %>%
  filter(term == "HR_per_game")

fit_strata2

# calculate slope of regression line after stratifying by HR per game
dat %>%  
  group_by(W_strata) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))

# Fit a multivariate regression determining the effects of runs per game, home runs per game,
# wins, and year on average attendance

fit3 <- Teams_small %>% lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)
tidy(fit3)

# Value predictions
team <- c(5, 1.2, 80, 2002)
est <- tidy(fit3)$estimate[-1]
tidy(fit3)$estimate[1] + sum(team * est)

team2 <- c(5, 1.2, 80, 1960)
tidy(fit3)$estimate[1] + sum(team2 * est)

# better codes
predict(fit3, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002))
predict(fit3, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960))

# Predicting for all teams in 2002
Teams_2002 <- Teams %>% filter(yearID %in% 2002) %>%
                mutate(avg_attendance = attendance/G, HR_per_game = HR / G, R_per_game = R / G) %>%
                mutate(attendance_hat = predict(fit3, newdata = .))

mean(Teams_2002$attendance_hat)
cor(Teams_2002$avg_attendance, Teams_2002$attendance_hat)


