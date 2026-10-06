#need to change the python code to R code, and then pivot the data
#install packages
install.packages(c("dplyr", "tidyr", "arrow"))
#load packages
library(dplyr)
library(tidyr) #pivot data
library(arrow) #read parquet files

#read in the parquet file
pcv_stacked_new <- read_parquet("PCV_Stacked_new.parquet")
head(pcv_stacked_new) #first 6 rows
names(pcv_stacked_new) #column names

#define site/variant combinations
var_mat_new <- list(
  "Puurs" = c("S01", "S03", "S04", "S09V", "S18C", "S19A"),
  "Grange Castle" = c("S03", "S04", "S18C", "S19A")
)

#define the pivot index
pivot_index_new <- c(
    "batch_number",
    "material_number",
    "product_name",
    "mbc_batch_number",
    "date_of_manufacture"
)

#create a for loop to iterate through each site and variant combination
for (site in names(var_mat_new)) {
  
  for (serotype in var_mat_new[[site]]) {
    
#print which dataset is currently being processed
    print(paste(
      "Processing site:", site,
      "| variant:", serotype
    ))

#filter the data for the current site and variant
    df_new <- pcv_stacked_new %>%
      filter(
        manufacture_site == site,
        variant == serotype
      )

#pivot the data from stacked to wide format
    df_wide <- df_new %>%
      pivot_wider(
        id_cols = all_of(pivot_index_new),
        names_from = parameter,
        values_from = weighted_avg_value,
        values_fn = mean
    )

#view the wide data frame
    print(head(df_wide)) #print the first 6 rows

#create the output file name based on site and serotype
    file_name_new <- paste0(
      gsub(" ", "_", site),
      "_",
      serotype,
      "_new.csv"
    ) 
    
#save the wide data frame to a CSV file
    write.csv(df_wide, file_name_new, row.names = FALSE)
  }
}

#analyse one of the CSV files to check the output
install.packages("readr")
library(readr)

Puurs_S01_new <- read_csv("Puurs_S01_new.csv")
head(Puurs_S01_new) #first 6 rows
names(Puurs_S01_new) #column names
dim(Puurs_S01_new) #get the number of rows and columns
summary(Puurs_S01_new$'S01 Total Antigenicity') #summary statistics for the total antigenicity column
sum(!is.na(Puurs_S01_new$'S01 Total Antigenicity')) #count of non-missing values for the total antigenicity column
sum(is.na(Puurs_S01_new$'S01 Total Antigenicity')) #count of missing values for the total antigenicity column
colSums(is.na(Puurs_S01_new)) #count of missing values for each column

#checking for duplicates
Puurs_S01_new %>%
  count(batch_number) %>%
  filter(n > 1)

Puurs_S03_new <- read_csv("Puurs_S03_new.csv")
head(Puurs_S03_new) #first 6 rows
names(Puurs_S03_new) #column names
dim(Puurs_S03_new) #get the number of rows and columns
summary(Puurs_S03_new$'S03 Total Antigenicity') #summary statistics for the total antigenicity column
sum(!is.na(Puurs_S03_new$'S03 Total Antigenicity')) #count of non-missing values for the total antigenicity column
sum(is.na(Puurs_S03_new$'S03 Total Antigenicity')) #count of missing values for the total antigenicity column
colSums(is.na(Puurs_S03_new)) #count of missing values for each column

#checking for duplicates
Puurs_S03_new %>%
  count(batch_number) %>%
  filter(n > 1)

Puurs_S04_new <- read_csv("Puurs_S04_new.csv")
head(Puurs_S04_new) #first 6 rows
names(Puurs_S04_new) #column names
dim(Puurs_S04_new) #get the number of rows and columns
summary(Puurs_S04_new$'S04 Total Antigenicity') #summary statistics for the total antigenicity column
sum(!is.na(Puurs_S04_new$'S04 Total Antigenicity')) #count of non-missing values for the total antigenicity column
sum(is.na(Puurs_S04_new$'S04 Total Antigenicity')) #count of missing values for the total antigenicity column
colSums(is.na(Puurs_S04_new)) #count of missing values for each column

#checking for duplicates
Puurs_S04_new %>%
  count(batch_number) %>%
  filter(n > 1)

Puurs_S09V_new <- read_csv("Puurs_S09V_new.csv")
head(Puurs_S09V_new) #first 6 rows
names(Puurs_S09V_new) #column names
dim(Puurs_S09V_new) #get the number of rows and columns
summary(Puurs_S09V_new$'S09V Total Antigenicity') #summary statistics for the total antigenicity column
sum(!is.na(Puurs_S09V_new$'S09V Total Antigenicity')) #count of non-missing values for the total antigenicity column
sum(is.na(Puurs_S09V_new$'S09V Total Antigenicity')) #count of missing values for the total antigenicity column
colSums(is.na(Puurs_S09V_new)) #count of missing values for each column

#checking for duplicates
Puurs_S09V_new %>%
  count(batch_number) %>%
  filter(n > 1)

Puurs_S18C_new <- read_csv("Puurs_S18C_new.csv")
head(Puurs_S18C_new) #first 6 rows
names(Puurs_S18C_new) #column names
dim(Puurs_S18C_new) #get the number of rows and columns
summary(Puurs_S18C_new$'S18C Total Antigenicity') #summary statistics for the total antigenicity column
sum(!is.na(Puurs_S18C_new$'S18C Total Antigenicity')) #count of non-missing values for the total antigenicity column
sum(is.na(Puurs_S18C_new$'S18C Total Antigenicity')) #count of missing values for the total antigenicity column
colSums(is.na(Puurs_S18C_new)) #count of missing values for each column

#checking for duplicates
Puurs_S18C_new %>%
  count(batch_number) %>%
  filter(n > 1)

Puurs_S19A_new <- read_csv("Puurs_S19A_new.csv")
head(Puurs_S19A_new) #first 6 rows
names(Puurs_S19A_new) #column names
dim(Puurs_S19A_new) #get the number of rows and columns
summary(Puurs_S19A_new$'S19A Total Antigenicity') #summary statistics for the total antigenicity column
sum(!is.na(Puurs_S19A_new$'S19A Total Antigenicity')) #count of non-missing values for the total antigenicity column
sum(is.na(Puurs_S19A_new$'S19A Total Antigenicity')) #count of missing values for the total antigenicity column
colSums(is.na(Puurs_S19A_new)) #count of missing values for each column

#checking for duplicates
Puurs_S19A_new %>%
  count(batch_number) %>%
  filter(n > 1)

Grange_Castle_S03_new <- read_csv("Grange_Castle_S03_new.csv")
head(Grange_Castle_S03_new) #first 6 rows
names(Grange_Castle_S03_new) #column names
dim(Grange_Castle_S03_new) #get the number of rows and columns
summary(Grange_Castle_S03_new$'S03 Total Antigenicity') #summary statistics for the total antigenicity column
sum(!is.na(Grange_Castle_S03_new$'S03 Total Antigenicity')) #count of non-missing values for the total antigenicity column
sum(is.na(Grange_Castle_S03_new$'S03 Total Antigenicity')) #count of missing values for the total antigenicity column
colSums(is.na(Grange_Castle_S03_new)) #count of missing values for each column

#checking for duplicates
Grange_Castle_S03_new %>%
  count(batch_number) %>%
  filter(n > 1)

Grange_Castle_S04_new <- read_csv("Grange_Castle_S04_new.csv")
head(Grange_Castle_S04_new) #first 6 rows
names(Grange_Castle_S04_new) #column names
dim(Grange_Castle_S04_new) #get the number of rows and columns
summary(Grange_Castle_S04_new$'S04 Total Antigenicity') #summary statistics for the total antigenicity column
sum(!is.na(Grange_Castle_S04_new$'S04 Total Antigenicity')) #count of non-missing values for the total antigenicity column
sum(is.na(Grange_Castle_S04_new$'S04 Total Antigenicity')) #count of missing values for the total antigenicity column
colSums(is.na(Grange_Castle_S04_new)) #count of missing values for each column

#checking for duplicates
Grange_Castle_S04_new %>%
  count(batch_number) %>%
  filter(n > 1)

Grange_Castle_S18C_new <- read_csv("Grange_Castle_S18C_new.csv")
head(Grange_Castle_S18C_new) #first 6 rows
names(Grange_Castle_S18C_new) #column names
dim(Grange_Castle_S18C_new) #get the number of rows and columns
summary(Grange_Castle_S18C_new$'S18C Total Antigenicity') #summary statistics for the total antigenicity column
sum(!is.na(Grange_Castle_S18C_new$'S18C Total Antigenicity')) #count of non-missing values for the total antigenicity column
sum(is.na(Grange_Castle_S18C_new$'S18C Total Antigenicity')) #count of missing values for the total antigenicity column
colSums(is.na(Grange_Castle_S18C_new)) #count of missing values for each column

#checking for duplicates
Grange_Castle_S18C_new %>%
  count(batch_number) %>%
  filter(n > 1)

Grange_Castle_S19A_new <- read_csv("Grange_Castle_S19A_new.csv")
head(Grange_Castle_S19A_new) #first 6 rows
names(Grange_Castle_S19A_new) #column names
dim(Grange_Castle_S19A_new) #get the number of rows and columns
summary(Grange_Castle_S19A_new$'S19A Total Antigenicity') #summary statistics for the total antigenicity column
sum(!is.na(Grange_Castle_S19A_new$'S19A Total Antigenicity')) #count of non-missing values for the total antigenicity column
sum(is.na(Grange_Castle_S19A_new$'S19A Total Antigenicity')) #count of missing values for the total antigenicity column
colSums(is.na(Grange_Castle_S19A_new)) #count of missing values for each column

#checking for duplicates
Grange_Castle_S19A_new %>%
  count(batch_number) %>%
  filter(n > 1)


#for all of the Puurs serotypes, there was a higher amount of missing values for total antigenicity than non-missing values
#for all of the Grange Castle serotypes, there was a higher amount of non-missing values for total antigenicity than missing values
#also in the Puurs serotypes, the highest amount of missing values was in the total antigenicity column
#in the Grange Castle serotypes, total antigenicity didn't have the highest amount of missing values 
