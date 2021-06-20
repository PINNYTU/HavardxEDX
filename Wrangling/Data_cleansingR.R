#################Tidy Data#################
#Reshaping Data
library("tidyverse")
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
select(wide_data, country, `1960`:`1967`)


#################Reshaping Data#################
# gather wide data to make new tidy data
new_tidy_data <- wide_data %>% 
  gather(year, fertility, '1960':'2015')

head(new_tidy_data)

# gather all columns except country
new_tidy_data2 <- wide_data %>% 
  gather(year, fertility, -country)
head(new_tidy_data2)
