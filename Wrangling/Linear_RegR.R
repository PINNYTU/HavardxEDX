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
  geom_point()+
  geom_abline(intercept = b, slope = m) #plot regression line


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
  geom_abline(intercept = 0, slope = m)

galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)

# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y
b_2
#########QUIZ#########
set.seed(1989) #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

r <- cor(female_heights$mother, female_heights$daughter)
s_y <- sd(female_heights$daughter)
s_x <- sd(female_heights$mother)
r * s_y/s_x
corre<-cor(female_heights$mother,female_heights$daughter)
#slope m #intercept b= mux - m*muy
mu_y <- mean(female_heights$daughter)
mu_x <- mean(female_heights$mother)
mu_y - (r * s_y/s_x)*mu_x

#Change in daughter's height in inches given a 1 inch increase in the mother's height r * s_y/s_x
(r * s_y/s_x)
  
#quiz10
corre^2*100

#quiz11 𝐸(𝑌|𝑋)=𝑏+𝑋𝑏+𝑒
m = r * s_y/s_x
b = mu_y - (r * s_y/s_x)*mu_x
x = 60
m*x+b

######################Linear Regression######################
# find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 


#################Linear model#################
#observe father's heights X1,...,Xn
#>>Predicts son (Yi) = B0+B1*Xi+ei, i=1,..,N
#>>>Due to inherited genes, the son's height prediction grow by B1 for each inch we increase the father's height x 
#>>>Predicts son (Yi) = B0+B1*(Xi-Xbar)+ei, i=1,..,N ; B0 = predicted height for the son of the average father if X1=Xbar

#quiz
galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))

galton_heights
lm(son ~ father_centered, data = galton_heights)

#################Least Squares Estimates (LSE)#################

# compute RSS for any pair of beta0 and beta1 in Galton's data
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
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))
  #min beta1 ~0.65, B0 is fixed 25


# fit regression line to predict son's height from father's height
fit <- lm(son ~ father, data = galton_heights)
fit

# summary statistics
summary(fit)

#LSE derived from data Y1,...Yn
# Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
cor(lse[1,], lse[2,]) 
#for large enough N, the LSE will be ~normal with expected values B0 and B1
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

# Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# summary statistics
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm");

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line();

################Quiz################
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)
mean(beta1)

#Q3
library(Lahman)
library(tidyr)

coeff2<- Teams %>% filter( yearID %in% 1961:2001) %>%
  mutate(bb_per_game=BB/G, hr_per_gamr=HR/G,R_per_game=R/G) %>%
  lm( R_per_game ~ bb_per_game+hr_per_gamr, data = .) 
coeff2

#Q4
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})
head(lse)
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

#Q5
p1<-lse %>%  ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black");
p2<-lse %>%  ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "red") 
grid.arrange(p1, p2, ncol = 2)

#Q6
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

#Q7
set.seed(1989) #if you are using R 3.5 or earlier
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

female_heights[1,]
Predict_mother <- female_heights %>%
            lm(mother ~ daughter, data=.) %>% 
            summary %>%
            .$coef
Predict_mother           
Predict_mother2 <- female_heights%>%
  lm(mother ~ daughter, data=.) 

#predicted height of the first mother in
predict(Predict_mother2)[1]

#quiz 9
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_02_mean <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarise(mean_singles=mean(singles), mean_bb=mean(bb)) %>%
  select (playerID, mean_singles, mean_bb)


bat_01 <- Batting %>% filter(yearID %in% 1999:2001 ) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% 
  group_by(playerID) %>%
  summarise(mean_singles=mean(singles), mean_bb=mean(bb))%>%
  select(playerID, mean_singles, mean_bb) 

mean_per_player<-bat_01 %>%
  group_by(playerID) %>%
  summarise(mean_singles=mean(singles), mean_bb=mean(bb))

mean_per_player %>%
  filter(mean_singles >0.2) %>%
  summarise(n=n())
  
mean_per_player %>%
  filter(mean_bb >0.2) %>%
  summarise(n=n())

#quiz 10
Join_bat<-inner_join(bat_01,bat_02, by = "playerID")
c(cor(Join_bat$singles,Join_bat$mean_singles),cor(Join_bat$bb,Join_bat$mean_bb))
Join_bat
dat <- inner_join(bat_02, bat_99_01)
cor(Join_bat$singles, Join_bat$mean_singles)

#quiz11
a<-Batting %>% filter(yearID %in% 1999:2001 ) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% 
  group_by(playerID) %>%
  ggplot(aes(mean(singles) , singles)) + 
  geom_point(alpha = 0.5)

b<-Batting %>% filter(yearID %in% 1999:2001 ) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% 
  group_by(playerID) %>%
  ggplot(aes(mean(bb) , bb)) + 
  geom_point(alpha = 0.5) 

library(ggpubr)
ggarrange(a,b)
Batting
#quiz12 predict
Join_bat<-inner_join(bat_02,bat_01, by = "playerID")
Join_bat1<-Join_bat %>% select (mean_singles,singles)
fit_single <- Join_bat1%>%lm(singles ~ mean_singles,data=.)
fit_single
fit_bb <- Join_bat%>%lm(bb ~ mean_bb,data=.)
fit_bb

#################TIBBLE#######################
# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

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

####USING DO TO COMPUTE LM####

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
# use tidy to return lm estimates and related information as a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)

# add confidence intervals with tidy
tidy(fit, conf.int = TRUE)

# pipeline with lm, do, tidy
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

#Quiz 8
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

pair_family<-galton %>% 
  group_by(pair) %>%
  summarise(n=n())

pair1 <- galton %>% 
  group_by(pair) %>%
  summarise(r=cor(childHeight,parentHeight))

#quiz10
x<-galton %>% 
  group_by(pair) %>%
  do(tidy(lm(childHeight~ parentHeight , data = .), conf.int = TRUE)) 
x
#All of the parent-child heights are correlated with a p-value of <0.05.
#The std.error values are lower for daughters than sons, resulting in smaller confidence intervals.
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_son") %>%
  pull(estimate)

################################################
library(reshape2)
library(lpSolve)
library(Lahman)
# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs
# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

####Assessment 1
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(avg_attendance = attendance/G, R_per_G= R/G) %>%
  lm(avg_attendance ~  R_per_G, data=.) %>%
  .$coef 
Teams_small
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(avg_attendance = attendance/G, hr_g= HR/G) %>%
  lm(avg_attendance ~  hr_g, data=.) %>%
  .$coef 
#Q1b #predict win
Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G) %>%
  lm(avg_attendance ~ W, data=.) %>%
  .$coef %>%
  .[1] # x=0 , ans is intercept
#Q1c #predict year
Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G) %>%
  lm(avg_attendance ~  yearID , data=.) %>%
  .$coef 
#Q2
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(avg_attendance = attendance/G, R_per_G= R/G,W =W,hr_g= HR/G) %>%
  summarize(r = cor(W, hr_g))
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(avg_attendance = attendance/G, R_per_G= R/G,W =W,hr_g= HR/G) %>%
  summarize(r = cor(W, R_per_G))
#Q3a
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(avg_attendance = attendance/G, W_strata=round(W/10))%>%
  filter(W_strata >= 5 & W_strata <=10 ) %>%
  group_by(W_strata) %>%
  summarise(n()) 
#Q3b
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(avg_attendance = attendance/G, W_strata=round(W/10), R_per_game=R/G,BB_per_game=BB/G)%>%
  filter(W_strata >= 5 & W_strata <=10 ) %>%
  group_by(W_strata) %>%
  summarize(slope = cor(avg_attendance, R_per_game)*sd(avg_attendance)/sd(R_per_game))
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(avg_attendance = attendance/G, W_strata=round(W/10), R_per_game=R/G,BB_per_game=BB/G,HR_G=HR/G)%>%
  filter(W_strata >= 5 & W_strata <=10 ) %>%
  group_by(W_strata) %>%
  summarize(slope = cor(avg_attendance, HR_G)*sd(avg_attendance)/sd(HR_G))
#Q3c
dat<-Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(avg_attendance = attendance/G, W_strata=round(W/10), R_per_game=R/G,BB_per_game=BB/G,HR_G=HR/G)%>%
  filter(W_strata >= 5 & W_strata <=10 ) 
dat %>% ggplot(aes(HR_G, avg_attendance)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ W_strata)
dat %>% ggplot(aes(R_per_game, avg_attendance)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ W_strata)
# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

#Q4
fit<-Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(avg_attendance = attendance/G, R_per_G= R/G,W =W,hr_g= HR/G)%>%
  lm(avg_attendance  ~  R_per_G+hr_g+W+yearID , data=.)
#Q5
5*322+1.2*1798+80*117 -456671
Teams %>% 
  filter(yearID %in% 2002 ) %>% 
  mutate(avg_attendance = attendance/G, R_per_G= 5,W =80,hr_g= 1.2)%>%
  mutate(avg_attendance_1 = predict(fit, newdata = .)) %>%
  summarise(mean(avg_attendance_1))
Teams %>% 
  filter(yearID %in% 1960 ) %>% 
  mutate(avg_attendance = attendance/G, R_per_G= 5,W =80,hr_g= 1.2)%>%
  mutate(avg_attendance_2 = predict(fit, newdata = .))  %>%
  summarise(mean(avg_attendance_2))
#Q6
cor1<-Teams %>% 
  filter(yearID %in% 2002 ) %>% 
  mutate(avg_attendance = attendance/G, R_per_G= 5,W =80,hr_g= 1.2)%>%
  mutate(avg_attendance_1 = predict(fit, newdata = .)) 
cor2<-Teams %>% 
  filter(yearID %in% 2002 ) %>% 
  mutate(avg_attendance = attendance/G,R_per_G= R/G,W =W,hr_g= HR/G) %>%
  mutate(avg_attendance_predict = predict(fit, newdata = .)) %>%
  summarise(r= cor(avg_attendance,avg_attendance_predict)) %>%
  pull(r)
########sophomore slump" ############ 
library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

#The code to create a table with only the ROY award winners and add their batting statistics:
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")
#The code to keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons:
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)
#The code to use the spread function to have one column for the rookie and sophomore years batting averages:
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY
#The code to calculate the proportion of players who have a lower batting average their sophomore year:
mean(ROY$sophomore - ROY$rookie <= 0)
max(Batting$yearID)
two_years <- Batting %>%
  filter(yearID %in% 2012:2013) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2012`) & !is.na(`2013`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2012`, `2013`)
two_years

arrange(two_years, `2013`)

qplot(`2012`, `2013`, data = two_years)

summarize(two_years, cor(`2012`,`2013`))

######falling object
library(dslabs)
falling_object <- rfalling_object()
falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")

fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)

tidy(fit)

augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")

tidy(fit, conf.int = TRUE)

#quiz 9a
# linear regression with two variables
fit <- Teams%>% 
  filter(yearID=='1971') %>%
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

#quiz10
res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 
res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(estimate)

res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(p.value)

############Confounding############
# UC-Berkeley admission data
library(dslabs)
data(admissions)
admissions

# percent men and women accepted
admissions %>% group_by(gender) %>% 
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

# test whether gender and admission are independent
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))

# percent admissions by major
admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)

# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# plot number of applicants admitted and not
admissions %>%
  mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
  select(-applicants, -admitted) %>%
  gather(admission, number_of_students, -c("major", "gender")) %>%
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major)

admissions %>% 
  mutate(percent_admitted = admitted * applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

# condition on major and then look at differences
admissions %>% ggplot(aes(major, admitted, col = gender, size = applicants)) + geom_point()

# average difference by major
admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))

######Assestment Quiz2######
library(dslabs)
data("research_funding_rates")
research_funding_rates

