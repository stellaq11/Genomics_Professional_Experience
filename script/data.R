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
library(ggplot2) #need this for histogram

# Read in the parquet file
data <- read_parquet("PCV_Stacked.parquet")

#Explore the data
head(data) #first 6 rows
skim(data) #summary statistics, missing values, distributions
dim(data) #get the number of rows and columns
glimpse(data) #quick overview of the data structure, couldn't find this function
names(data) #column names
str(data) #structure of the data frame
summary(data) #summary statistics for each column (e.g. length, class, mode)
colSums(is.na(data)) #count of missing values for each column
colMeans(is.na(data)) #proportion of missing values for each column
colMeans(is.na(data)) * 100 #percentage of missing values for each column
sapply(data, class) #get the class/variable type of each column
sapply(data, n_distinct) #number of unique values for each column

#Analysing batch and parameter
#Batch
n_distinct(data$batch_number)
data %>% count(batch_number, sort = TRUE)
data %>% select(batch_number, mbc_batch_number) %>% distinct()
data %>%
  group_by(batch_number) %>%
  summarise(
    n_mbc_batches = n_distinct(mbc_batch_number)
  )

#Parameter
unique(data$parameter)
n_distinct(data$parameter)
data %>% count(parameter, sort = TRUE)

#Both
data %>%
  count(batch_number, parameter, sort = TRUE)

data %>%
  group_by(batch_number) %>%
  summarise(
    n_parameters = n_distinct(parameter),
    n_observations = n()
  )

all(
  data %>%
    count(batch_number, parameter) %>%
    pull(n) == 1
) #this is TRUE, meaning that each batch has only one observation for each parameter


data %>%
  group_by(batch_number) %>%
  summarise(
    n_parameters = n_distinct(parameter)
  ) %>%
  count(n_parameters, sort = TRUE)

data %>%
  count(batch_number, parameter) %>%
  count(n)

#Examining which parameters are most and least common
parameter_counts <- data %>%
  count(parameter, sort = TRUE)

parameter_counts

head(parameter_counts, 10)

parameter_counts %>%
  arrange(n) %>%
  slice_head(n = 10)

min(parameter_counts$n)
max(parameter_counts$n)
summary(parameter_counts$n)

#Investigate why batches have different numbers of parameters
batch_summary <- data %>%
  group_by(batch_number) %>%
  summarise(
    n_parameters = n_distinct(parameter),
    .groups = "drop"
  )
head(batch_summary)


#Examining weighted average measurement
summary(data$weighted_avg_value)
hist(data$weighted_avg_value)