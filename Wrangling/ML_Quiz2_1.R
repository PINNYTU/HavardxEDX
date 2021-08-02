library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)
head(dat)
y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

N_class1<-dat %>% 
  filter(sex=='Female' & type=='inclass') %>%
  summarise(n())

N_class1/length(dat$type[dat$type=='inclass'])

N_class2<-dat %>% 
  filter(sex=='Female' & type=='online') %>%
  summarise(n())

N_class2/length(dat$type[dat$type=='online'])

#ans
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

#q2
y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type
y_hat1 <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y)) 
y_hat2 <- ifelse(x == "in class", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat1 == y) #0.633
mean(y_hat2 == y) #0.453

#q3
table(predicted = y_hat, actual = y)

#q4 q5
sensitivity(y_hat,reference = y)
specificity(y_hat,reference = y)

#q6
confusionMatrix(data = y_hat, reference = y)
mean(y == "Female")

