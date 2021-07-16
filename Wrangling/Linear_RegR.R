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
