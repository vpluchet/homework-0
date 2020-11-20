library(tidyverse)
library(dslabs)

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

female_heights %>% summarise(mean(mother), sd(mother), mean(daughter), sd(daughter), cor(mother, daughter))

# Fitting model predicting mothers heights
Fit <- female_heights %>% lm(mother ~ daughter, data = .)
summary(Fit)
Fit$coefficients[1]
Fit$coefficients[2]

# Predicting mothers heights and extracting the projection for the first mother vs Actual
t <- predict(Fit)
t[1]

# Prediction for first mothr
predict(Fit)[1]
# Actual for first mother
female_heights$mother[1]



r <- cor(female_heights$mother, female_heights$daughter)
muy <- mean(female_heights$daughter)
mux <- mean(female_heights$mother)
sy <- sd(female_heights$daughter)
sx <- sd(female_heights$mother)
m <- r * sy / sx
b <- muy - m * mux

# Correlation between mother height and daughter height
r
# Slope of regression line
m
# Intercept of regression line
b
# Number of inches increase for 1 inch increase in mother height
m

# Percentage of variability of daughter's height explained by mother's height
100 * r^2

# Plotting daughter height versus mother height
female_heights %>% ggplot(aes(mother, daughter)) +
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = b, slope = m) +
  ggtitle("Daughter height vs Mother height") +
  geom_text(x=70, y=66.8, label= paste("corr = ", round(r, 2)), col = "blue")

# Conditional expected value
mother_height <- 60
expected_daughter <- b + m * mother_height
expected_daughter

# Plotting sons heights vs mothers

sons_heights <- GaltonFamilies%>%     
  filter(gender == "male") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(son = childHeight)

# Correlation Mother son 
r <- cor(sons_heights$mother, sons_heights$son)
print(r)

sons_heights %>% ggplot(aes(mother, son)) +
  geom_point(alpha = 0.5) + 
  ggtitle("Son height vs Mother height") +
  geom_text(x=70, y=66.8, label= paste("corr = ", round(r, 2)), col = "blue")



  


