#need to change the python code to R code, and then pivot the data
#install packages
install.packages(c("dplyr", "tidyr", "arrow"))
#load packages
library(dplyr)
library(tidyr) #pivot data
library(arrow) #read parquet files

#read in the parquet file
pcv_stacked <- read_parquet("PCV_Stacked.parquet")
head(pcv_stacked) #first 6 rows
names(pcv_stacked) #column names

#define site/variant combinations
var_mat <- list(
  "Puurs" = c("S01", "S03", "S04", "S09V", "S18C", "S19A"),
  "Grange Castle" = c("S03", "S04", "S18C", "S19A")
)

#define the pivot index
pivot_index <- c(
    "batch_number",
    "material_number",
    "product_name",
    "mbc_batch_number",
    "date_of_manufacture"
)

#create a for loop to iterate through each site and variant combination
for (site in names(var_mat)) {
  
  for (serotype in var_mat[[site]]) {
    
#print which dataset is currently being processed
    print(paste(
      "Processing site:", site,
      "| variant:", serotype
    ))

#filter the data for the current site and variant
    df <- pcv_stacked %>%
      filter(
        manufacture_site == site,
        variant == serotype
      )

#pivot the data from stacked to wide format
    df_wide <- df %>%
      pivot_wider(
        id_cols = all_of(pivot_index),
        names_from = parameter,
        values_from = weighted_avg_value,
        values_fn = mean
    )

#view the wide data frame
    print(head(df_wide)) #print the first 6 rows

#create the output file name based on site and serotype
    file_name <- paste0(
      gsub(" ", "_", site),
      "_",
      serotype,
      ".csv"
    ) 
    
#save the wide data frame to a CSV file
    write.csv(df_wide, file_name, row.names = FALSE)
  }
}

#analyse one of the CSV files to check the output
install.packages("readr")
library(readr)

Puurs_S01 <- read_csv("Puurs_S01.csv")
head(Puurs_S01) #first 6 rows
names(Puurs_S01) #column names
dim(Puurs_S01) #get the number of rows and columns
summary(Puurs_S01$'S01 Total Antigenicity') #summary statistics for the total antigenicity column
sum(!is.na(Puurs_S01$'S01 Total Antigenicity')) #count of non-missing values for the total antigenicity column
sum(is.na(Puurs_S01$'S01 Total Antigenicity')) #count of missing values for the total antigenicity column
colSums(is.na(Puurs_S01)) #count of missing values for each column

Puurs_S01 %>%
  count(batch_number) %>%
  filter(n > 1)

  