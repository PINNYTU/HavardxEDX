#################Tidy Data#################
#Reshaping Data
library(tidyverse)
library(dslabs)
data(gapminder)

#clean data
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select (country,year,fertility)
head(tidy_data)

# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, "1960":"1967")


#################Reshaping Data#################
# gather wide data to make new tidy data
new_tidy_data <- wide_data %>% 
  gather(year, fertility, '1960':'2015')

head(new_tidy_data)

# gather all columns except country
new_tidy_data2 <- wide_data %>% 
  gather(year, fertility, -country);
head(new_tidy_data2)
class(tidy_data$year)
class(new_tidy_data2$year) #by using gather it change to char

# convert gathered column names to numeric
new_tidy_data3 <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data3$year) #change to int again

# ggplot works on new tidy data
new_tidy_data3 %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point();

# spread tidy data to generate wide data
new_wide_data <- new_tidy_data3 %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)


#################Separate and Unite#################
filename <- file.path(path,  "life-expectancy-and-fertility-two-countries-example.csv")
raw_data <- read_csv(filename)
select(raw_data,1:5)
head(raw_data)

# gather all columns except country
dat <- raw_data %>% gather(key, value, -country)
head(dat)
dat$key[1:5] #must separate column

#separate into 2 columns
#clean if have 3 cols
dat1 <-  dat %>% 
        separate(key ,c("year","first_variable_name","second_variable_name"),fill="right" ) %>%
        unite(variable_name,first_variable_name, second_variable_name, sep="_" ) %>%
        spread(variable_name,value) %>%
        rename(fertility = fertility_NA)
dat1
#clean 2cols
dat2 <- dat %>% separate(key ,c("year","variable_name"),
                 sep="_", extra ="merge") %>%
        spread(variable_name,value) #pivot table
dat2

#################Quiz#################
data(co2)
#set to wide
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
head(co2_wide)
#clean
co2_tidy <- gather(co2_wide,month,co2,-year)
head(co2_tidy)

#plot
co2_tidy %>% ggplot(aes(as.numeric(month),co2, color=year)) +geom_line()

#quiz 2
dat <- admissions %>% select(-applicants)
dat_tidy <-
  spread(dat,gender, admitted)
dat_tidy

#quiz 13
tmp <- gather(admissions, key, value, admitted:applicants)
head(tmp)
tmp_tidy <- unite(tmp, column_name, c(key, gender)) %>%
  spread(tmp, column_name, key, gender)
  
  
#################Combining tables#################
library(tidyverse)
library(dslabs)
library(ggrepel)
#check identical 
identical(results_us_election_2016$state, murders$state)

#--Join--#
tab<- left_join(murders,results_us_election_2016, by='state')
head(tab)

# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)
# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2
dim(tab1)
dim(tab2)
# experiment with different joins
dim(left_join(tab1, tab2, by='state'))
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1, tab2)
semi_join(tab1, tab2)
anti_join(tab1, tab2)

#--Binding--#
#bind_cols() : binds columns and creates a tibble
#bind_rows() 
#cbind()
#rbind()
bind_cols(a = 1:3, b = 4:6)

tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)

#--Operators--#
# intersect vectors or data frames
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

# perform a union of vectors or data frames
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

# set difference of vectors or data frames
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)

# setequal determines whether sets have the same elements, regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)

#############Quiz#################
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()

top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

Salaries

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>% 
  select(nameFirst, nameLast, teamID, HR, salary)
head(top_salary)

library(Lahman) 
library(tidyverse) 
top <- Batting %>% filter(yearID == 2016) %>% 
      arrange(desc(HR)) %>% # arrange by descending HR count 
      slice(1:10) %>% 
      inner_join(AwardsPlayers) %>% 
      group_by(playerID) %>% 
      tally() %>% 
      select(playerID) 

top %>% as_tibble() 
top
awards <- AwardsPlayers %>% 
          filter(yearID == 2016) %>% 
          group_by(playerID) %>% 
          tally() %>% 
          select(playerID) awards %>% 
          as_tibble() 

setdiff(awards, top) 


