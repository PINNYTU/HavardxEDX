getwd()

######################In terminal######################

#git clone https://github.com/rafalab/dslabs.git

#######################Copy file##############################
library(dslabs)
path<-system.file("extdata",package="dslabs")
list.files(path)

#copy file to working directory
filename <- "murders.csv"
fullpath<-file.path(path,filename)
fullpath
file.copy(from=fullpath,to="Desktop/HavardxEDX/Havard-EDX/Wrangling")
file.copy(file.path(path, "murders.csv"), file.path(getwd(), "data"))
file.copy(file.path(path, "murders.csv"), getwd())

#######################Read file##############################
#update.packages(ask=F)
library(dslabs)
devtools::install_github("RcppCore/Rcpp")
devtools::install_version('namespace', '0.12.18')
packageVersion('Rcpp')
install.packages("scales")
install.packages("tidyverse")

library(tidyverse)    # includes readr
library(readxl)
read_lines("murders.csv", n=3)

# inspect the first 3 lines
read_lines("murders.csv", n_max = 3)

# read file in CSV format
dat <- read_csv(filename)

#read using full path
dat <- read_csv(fullpath)
head(dat)

#Ex：
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files
filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat=read.csv(file.path(path, filename))
dat1=read.csv(file.path(path, filename1))
dat2=read.csv(file.path(path, filename2))

# filename is defined in the previous video
# read.csv converts strings to factors
dat2 <- read.csv(filename)
class(dat2$abb)
class(dat2$region)

#######################import file from url##############################
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
#create unique temporary file
tempfile() 
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename) #remove in the end


#######################Quiz 14##############################
url_q <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"

dat <- read_csv(url_q,col_names=FALSE)
dat
