#missingness assessment
install.packages(c("mice", "dplyr"))
library(mice)
library(dplyr)

#examine missing data in 1 serotype
#now want to create a function for all serotypes
feature_data <- Puurs_S01_altered %>%
  select(
    -batch_number,
    -material_number,
    -mbc_batch_number,
    -date_of_manufacture
  )

missing_summary <- data.frame(
  variable = names(feature_data),
  missing_percent = sapply(feature_data, function(x) {
    mean(is.na(x)) * 100
  })
)

missing_summary

#drop variables with more than 20% missing values
variables_to_drop <- missing_summary %>%
  filter(missing_percent > 20) %>%
  pull(variable)

variables_to_drop









#check for missingness using a function

check_missingness <- function(data) {

  # Remove identifier columns
  feature_data <- data %>%
    select(
      -any_of(c(
        "batch_number",
        "material_number",
        "mbc_batch_number",
        "date_of_manufacture"
      ))
    )

  # Calculate missingness for each feature
  missingness <- data.frame(
    variable = names(feature_data),
    missing_count = sapply(feature_data, function(x) sum(is.na(x))),
    missing_percent = sapply(feature_data, function(x) mean(is.na(x)) * 100)
)

  # Sort from highest to lowest missingness
  missingness <- missingness %>%
    arrange(desc(missing_percent))

  return(missingness)
}

library(readr)

Puurs_S01_altered <- read_csv("Puurs_S01_altered.csv")
check_missingness(Puurs_S01_altered)

Puurs_S03_altered <- read_csv("Puurs_S03_altered.csv")
check_missingness(Puurs_S03_altered)

Puurs_S04_altered <- read_csv("Puurs_S04_altered.csv")
check_missingness(Puurs_S04_altered)

Puurs_S09V_altered <- read_csv("Puurs_S09V_altered.csv")
check_missingness(Puurs_S09V_altered)

Puurs_S18C_altered <- read_csv("Puurs_S18C_altered.csv")
check_missingness(Puurs_S18C_altered)

Puurs_S19A_altered <- read_csv("Puurs_S19A_altered.csv")
check_missingness(Puurs_S19A_altered)

Grange_Castle_S03_altered <- read_csv("Grange_Castle_S03_altered.csv")
check_missingness(Grange_Castle_S03_altered)

Grange_Castle_S04_altered <- read_csv("Grange_Castle_S04_altered.csv")
check_missingness(Grange_Castle_S04_altered)

Grange_Castle_S18C_altered <- read_csv("Grange_Castle_S18C_altered.csv")
check_missingness(Grange_Castle_S18C_altered)

Grange_Castle_S19A_altered <- read_csv("Grange_Castle_S19A_altered.csv")
check_missingness(Grange_Castle_S19A_altered)



#run the function for all serotypes and store the results in a list
serotype_data <- list(
  Puurs_S01 = Puurs_S01_altered,
  Puurs_S03 = Puurs_S03_altered,
  Puurs_S04 = Puurs_S04_altered,
  Puurs_S09V = Puurs_S09V_altered,
  Puurs_S18C = Puurs_S18C_altered,
  Puurs_S19A = Puurs_S19A_altered,
  Grange_Castle_S03 = Grange_Castle_S03_altered,
  Grange_Castle_S04 = Grange_Castle_S04_altered,
  Grange_Castle_S18C = Grange_Castle_S18C_altered,
  Grange_Castle_S19A = Grange_Castle_S19A_altered
)

missingness_results <- lapply(
  serotype_data,
  check_missingness
)

#combine everything into a single data frame
missingness_all <- bind_rows(
  lapply(names(missingness_results), function(x) {
    missingness_results[[x]] %>%
      mutate(dataset = x, .before = 1)
  })
)

head(missingness_all)
names(missingness_all)

#apply 20% missingness threshold to all serotypes
missingness_all <- missingness_all %>%
  mutate(
    decision = ifelse(
      missing_percent > 20,
      "Review: >20% missing",
      "Retain: <=20% missing"
    )
  )

#use capital V here to view the entire data frame in R
View(missingness_all)


#identify variables to drop for each serotype based on the 20% missingness threshold
variables_to_drop <- lapply(
  missingness_results,
  function(missing_summary) {
    missing_summary %>%
      filter(missing_percent > 20) %>%
      pull(variable)
  }
)

variables_to_drop

#exclude variables above the 20% missingness threshold from each serotype dataset
#this does not change the original datasets, it creates new datasets with the variables removed
feature_data <- lapply(names(serotype_data), function(dataset_name) {

  serotype_data[[dataset_name]] %>%
    select(
      -any_of(c(
        "batch_number",
        "material_number",
        "mbc_batch_number",
        "date_of_manufacture"
      )),
      -any_of(variables_to_drop[[dataset_name]])
    )

})

names(feature_data) <- names(serotype_data)

#examing serotype S01 after removing variables with >20% missingness
feature_data$Puurs_S01
View(feature_data$Puurs_S01)
dim(feature_data$Puurs_S01)
names(feature_data$Puurs_S01)

feature_data$Puurs_S03
View(feature_data$Puurs_S03)
dim(feature_data$Puurs_S03)
names(feature_data$Puurs_S03)

feature_data$Puurs_S04
View(feature_data$Puurs_S04)
dim(feature_data$Puurs_S04)
names(feature_data$Puurs_S04)

feature_data$Puurs_S09V
View(feature_data$Puurs_S09V)
dim(feature_data$Puurs_S09V)
names(feature_data$Puurs_S09V)

feature_data$Puurs_S18C
View(feature_data$Puurs_S18C)
dim(feature_data$Puurs_S18C)
names(feature_data$Puurs_S18C)

feature_data$Puurs_S19A
View(feature_data$Puurs_S19A)
dim(feature_data$Puurs_S19A)
names(feature_data$Puurs_S19A)

feature_data$Grange_Castle_S03
View(feature_data$Grange_Castle_S03)
dim(feature_data$Grange_Castle_S03)
names(feature_data$Grange_Castle_S03)

feature_data$Grange_Castle_S04
View(feature_data$Grange_Castle_S04)
dim(feature_data$Grange_Castle_S04)
names(feature_data$Grange_Castle_S04)

feature_data$Grange_Castle_S18C
View(feature_data$Grange_Castle_S18C)
dim(feature_data$Grange_Castle_S18C)
names(feature_data$Grange_Castle_S18C)

feature_data$Grange_Castle_S19A
View(feature_data$Grange_Castle_S19A)
dim(feature_data$Grange_Castle_S19A)
names(feature_data$Grange_Castle_S19A)



#check the number of columns in each serotype dataset after removing variables with >20% missingness
sapply(feature_data, ncol)



#using the mice package to impute missing values for each serotype dataset
library(mice)
library(dplyr)

# Select the Puurs S01 dataset
data_Puurs_S01 <- feature_data$Puurs_S01

# Check dimensions
dim(data_Puurs_S01)

# Count missing values in each variable
sort(colSums(is.na(data_Puurs_S01)), decreasing = TRUE)

#this is not working
install.packages("reformulas")
library(reformulas)
# Summarise missingness
md.pattern(data_Puurs_S01)

#summarise missingness in a table instead
library(dplyr)

missing_summary <- data.frame(
  variable = names(data_Puurs_S01),
  missing_count = sapply(data_Puurs_S01, function(x) sum(is.na(x))),
  missing_percent = sapply(data_Puurs_S01, function(x) {
    mean(is.na(x)) * 100
  })
) %>%
  arrange(desc(missing_percent))

View(missing_summary)



#run MICE on the Puurs S01 dataset
#set a seed for reproducibility

set.seed(123)

imputed_S01 <- mice(
  data_Puurs_S01,
  m = 5,
  maxit = 5,
  method = "pmm",
  seed = 123
)

#I think I need to update my r. MICE is not working.
