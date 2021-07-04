##################Baseball##################

library(tidyverse)
library(dslabs)
library(Lahman)
head(Teams)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(R_per_game , AB_per_game)) + 
  geom_point(alpha = 0.5)  

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(win_per_game = W / G, E_per_game = E / G) %>%
  ggplot(aes(win_per_game , E_per_game)) + 
  geom_point(alpha = 0.5)  

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G) %>%
  ggplot(aes(X3B_per_game , X2B_per_game)) + 
  geom_point(alpha = 0.5)  

##################Correlation##################
# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

#cal correlation
rho <- mean(scale(x)*scale(y))
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r) 

#########QUIZ#########

#cal correlation
Teams %>% filter(yearID %in% 1961:2001) %>%
  summarize(r = cor(R/G, AB/G)) %>% pull(r) 


Teams %>% filter(yearID %in% 1961:2001) %>%
  summarize(r = cor(W/G, E/G)) %>% pull(r) 

Teams %>% filter(yearID %in% 1961:2001) %>%
  summarize(r = cor(X3B/G, X2B/G)) %>% pull(r) 

##################Anscombe's Quartet/Stratification##################
# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)
