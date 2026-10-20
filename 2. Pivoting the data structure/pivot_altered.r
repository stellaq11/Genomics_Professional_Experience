# install packages
install.packages(c("dplyr", "tidyr", "arrow"))

# load packages
library(dplyr)
library(tidyr)
library(arrow)

# read in the parquet file
pcv_stacked_new <- read_parquet("PCV_Stacked_new.parquet")

head(pcv_stacked_new)
names(pcv_stacked_new)

# define site/variant combinations
var_mat_new <- list(
  "Puurs" = c("S01", "S03", "S04", "S09V", "S18C", "S19A"),
  "Grange Castle" = c("S03", "S04", "S18C", "S19A")
)

# define the pivot index
pivot_index_new <- c(
  "batch_number",
  "material_number",
  "mbc_batch_number",
  "date_of_manufacture"
)

# create a for loop to iterate through each site and variant combination
for (site in names(var_mat_new)) {
  
  for (serotype in var_mat_new[[site]]) {
    
    # print which dataset is currently being processed
    print(paste(
      "Processing site:", site,
      "| variant:", serotype
    ))
    
    # filter the data for the current site and variant
    df_new <- pcv_stacked_new %>%
      filter(
        manufacture_site == site,
        variant == serotype
      )
    
    # Align date and material number within each batch
    # using batch_number + mbc_batch_number as the batch key
    # take earliest date of manufacture and first material number
    df_new <- df_new %>%
      group_by(batch_number, mbc_batch_number) %>%
      mutate(
        date_of_manufacture = if (all(is.na(date_of_manufacture))) {
      as.Date(NA) #put NA if all values are NA
    } else {
      min(date_of_manufacture, na.rm = TRUE)
    },
        material_number = names(which.max(table(material_number)))
      ) %>%
      ungroup()
    
    # pivot the data from stacked to wide format
    df_wide <- df_new %>%
      pivot_wider(
        id_cols = all_of(pivot_index_new),
        names_from = parameter,
        values_from = weighted_avg_value,
        values_fn = mean
      )
    
    # view the wide data frame
    print(head(df_wide))
    
    # create the output file name
    file_name_new <- paste0(
      gsub(" ", "_", site),
      "_",
      serotype,
      "_altered.csv"
    )
    
    # save the wide data frame to a CSV file
    write.csv(df_wide, file_name_new, row.names = FALSE)
  }
}

class(pcv_stacked_new$date_of_manufacture)
str(pcv_stacked_new$date_of_manufacture)

#analyse one of the CSV files to check the output
install.packages("readr")
library(readr)#

Puurs_S01_altered <- read_csv("Puurs_S01_altered.csv")
head(Puurs_S01_altered) #first 6 rows
names(Puurs_S01_altered)
dim(Puurs_S01_altered)
summary(Puurs_S01_altered$'S01 Total Antigenicity')
sum(!is.na(Puurs_S01_altered$'S01 Total Antigenicity')) #count of non-missing values for the total antigenicity column
sum(is.na(Puurs_S01_altered$'S01 Total Antigenicity')) #count of missing values for the total antigenicity column
colSums(is.na(Puurs_S01_altered))


Puurs_S01_altered
ncol(Puurs_S01_altered) #number of columns
print(head(Puurs_S01_altered), width = Inf) #viewing the entire data frame in the console
nrow(Puurs_S01_altered) #number of rows

#checking for duplicates
Puurs_S01_altered %>%
  count(batch_number) %>%
  filter(n > 1)

Puurs_S03_altered <- read_csv("Puurs_S03_altered.csv")
head(Puurs_S03_altered) #first 6 rows
names(Puurs_S03_altered)
dim(Puurs_S03_altered)
ncol(Puurs_S03_altered) #number of columns
nrow(Puurs_S03_altered) #number of rows
colSums(is.na(Puurs_S03_altered)) #count of missing values for each column

#checking for duplicates
Puurs_S03_altered %>%
  count(batch_number) %>%
  filter(n > 1)

Puurs_S04_altered <- read_csv("Puurs_S04_altered.csv")
dim(Puurs_S04_altered)

Puurs_S04_altered %>%
  count(batch_number) %>%
  filter(n > 1)

Puurs_S09V_altered <- read_csv("Puurs_S09V_altered.csv")
dim(Puurs_S09V_altered)

Puurs_S09V_altered %>%
  count(batch_number) %>%
  filter(n > 1)

Puurs_S18C_altered <- read_csv("Puurs_S18C_altered.csv")
dim(Puurs_S18C_altered)

Puurs_S18C_altered %>%
  count(batch_number) %>%
  filter(n > 1)

Puurs_S19A_altered <- read_csv("Puurs_S19A_altered.csv")
dim(Puurs_S19A_altered)

Puurs_S19A_altered %>%
  count(batch_number) %>%
  filter(n > 1)

Grange_Castle_S03_altered <- read_csv("Grange_Castle_S03_altered.csv")
dim(Grange_Castle_S03_altered)

Grange_Castle_S03_altered %>%
  count(batch_number) %>%
  filter(n > 1)

Grange_Castle_S04_altered <- read_csv("Grange_Castle_S04_altered.csv")
dim(Grange_Castle_S04_altered)

Grange_Castle_S04_altered %>%
  count(batch_number) %>%
  filter(n > 1)

Grange_Castle_S18C_altered <- read_csv("Grange_Castle_S18C_altered.csv")
dim(Grange_Castle_S18C_altered)

Grange_Castle_S18C_altered %>%
  count(batch_number) %>%
  filter(n > 1)

Grange_Castle_S19A_altered <- read_csv("Grange_Castle_S19A_altered.csv")
dim(Grange_Castle_S19A_altered)

Grange_Castle_S19A_altered %>%
  count(batch_number) %>%
  filter(n > 1)