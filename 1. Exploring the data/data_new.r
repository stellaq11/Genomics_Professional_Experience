# Install required packages
install.packages(c("arrow", "tidyverse", "skimr", "DataExplorer"))
install.packages("dplyr")
install.packages("ggplot2")
# Load required libraries
library(arrow)
library(tidyverse) #it says this package was built under R version 4.3.3
library(skimr)
library(DataExplorer) #it says this package is not available
library(dplyr) #need this for n_distinct function
library(ggplot2) #need this for data visualisation, but it says there is no package called 'ggplot2'

# Read in the parquet file
data_new <- read_parquet("PCV_Stacked_new.parquet")

#Explore the data
head(data_new) #first 6 rows
skim(data_new) #summary statistics, missing values, distributions
dim(data_new) #get the number of rows and columns
#rows and columns 1394776 and 10
glimpse(data_new) #quick overview of the data structure, couldn't find this function
names(data_new) #column names
str(data_new) #structure of the data frame
summary(data_new) #summary statistics for each column (e.g. length, class, mode)
colSums(is.na(data_new)) #count of missing values for each column
colMeans(is.na(data_new)) #proportion of missing values for each column
colMeans(is.na(data_new)) * 100 #percentage of missing values for each column
sapply(data_new, class) #get the class/variable type of each column
sapply(data_new, n_distinct) #number of unique values for each column

#Analysing batch and parameter
#Batch
n_distinct(data_new$batch_number) #13662

data_new %>% count(batch_number, sort = TRUE) #counts occurrences of each batch number

data_new %>% select(batch_number, mbc_batch_number) %>% distinct() #lists unique combinations of batch and mbc batch numbers

data_new %>%
  group_by(batch_number) %>%
  summarise(
    n_mbc_batches = n_distinct(mbc_batch_number)
  ) #summarises the number of unique MBC batch numbers for each batch

#Parameter
unique(data_new$parameter) #contains total antigenicity for each serotype (e.g. SO1)
n_distinct(data_new$parameter) #139
data_new %>% count(parameter, sort = TRUE) #counts occurrences of each parameter

#Both
data_new %>%
  count(batch_number, parameter, sort = TRUE) #counts occurrences of each combination of batch number and parameter

data_new %>%
  group_by(batch_number) %>%
  summarise(
    n_parameters = n_distinct(parameter),
    n_observations = n()
  ) #summarises the number of unique parameters and total observations for each batch

all(
  data_new %>%
    count(batch_number, parameter) %>%
    pull(n) == 1
) 
#this is TRUE, meaning that each batch has only one observation for each parameter


data_new %>%
  group_by(batch_number) %>%
  summarise(
    n_parameters = n_distinct(parameter)
  ) %>%
  count(n_parameters, sort = TRUE)

data_new %>%
  count(batch_number, parameter) %>%
  count(n)
# 1     1 1394776

#Examining which parameters are most and least common
parameter_counts_new <- data_new %>%
  count(parameter, sort = TRUE)

parameter_counts_new

head(parameter_counts_new, 10)

parameter_counts_new %>%
  arrange(n) %>%
  slice_head(n = 10)

min(parameter_counts_new$n) #480
max(parameter_counts_new$n) #11739
summary(parameter_counts_new$n)

#Investigate why batches have different numbers of parameters
batch_summary_new <- data_new %>%
  group_by(batch_number) %>%
  summarise(
    n_parameters = n_distinct(parameter),
    .groups = "drop"
  )
head(batch_summary_new)


#Examining weighted average measurement
summary(data_new$weighted_avg_value) 
hist(data_new$weighted_avg_value) #saved in folder under "weighted_avg_value_histogram_new.pdf"


#number of unique batch and mbc batch numbers for Puurs S01 original data
#this is checking to see the number of rows in the original file so it can be compared to the altered file
#want to see if the number of rows is the same in both files

data_new %>%
  filter(
    manufacture_site == "Puurs",
    variant == "S01"
  ) %>%
  distinct(batch_number, mbc_batch_number) %>%
  nrow()

data_new %>%
  filter(
    manufacture_site == "Puurs",
    variant == "S03"
  ) %>%
  distinct(batch_number, mbc_batch_number) %>%
  nrow()

data_new %>%
  filter(
    manufacture_site == "Puurs",
    variant == "S04"
  ) %>%
  distinct(batch_number, mbc_batch_number) %>%
  nrow()

data_new %>%
  filter(
    manufacture_site == "Puurs",
    variant == "S09V"
  ) %>%
  distinct(batch_number, mbc_batch_number) %>%
  nrow()

data_new %>%
  filter(
    manufacture_site == "Puurs",
    variant == "S18C"
  ) %>%
  distinct(batch_number, mbc_batch_number) %>%
  nrow()

data_new %>%
  filter(
    manufacture_site == "Puurs",
    variant == "S19A"
  ) %>%
  distinct(batch_number, mbc_batch_number) %>%
  nrow()

data_new %>%
  filter(
    manufacture_site == "Grange Castle",
    variant == "S03"
  ) %>%
  distinct(batch_number, mbc_batch_number) %>%
  nrow()

data_new %>%
  filter(
    manufacture_site == "Grange Castle",
    variant == "S04"
  ) %>%
  distinct(batch_number, mbc_batch_number) %>%
  nrow()

data_new %>%
  filter(
    manufacture_site == "Grange Castle",
    variant == "S18C"
  ) %>%
  distinct(batch_number, mbc_batch_number) %>%
  nrow()

data_new %>%
  filter(
    manufacture_site == "Grange Castle",
    variant == "S19A"
  ) %>%
  distinct(batch_number, mbc_batch_number) %>%
  nrow()
