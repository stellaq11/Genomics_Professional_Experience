# Install required packages
install.packages(c("arrow", "tidyverse", "skimr", "DataExplorer"))
# Load required libraries
library(arrow)
library(tidyverse)
library(skimr)
library(DataExplorer)
# Read in the parquet file
data <- read_parquet("data/PCV_Stacked.parquet")

#Explore the data
head(data) #first 6 rows
skim(data) #summary statistics, missing values, distributions
dim(data) #get the number of rows and columns
glimpse(data) #quick overview of the data structure, error with this one
names(data) #column names
str(data) #structure of the data frame
summary(data) #summary statistics for each column (e.g. length, class, mode)
colSums(is.na(data)) #count of missing values for each column
colMeans(is.na(data)) #proportion of missing values for each column
colMeans(is.na(data)) * 100 #percentage of missing values for each column
