#need to remove columns above threshold of missingness
#look at rate of missingness for each serotype
#check whether to use 10, 15 or 20% threshold for missingness
#check missingness for each serotype individually
#incomplete_data is used analyse the misingness in each column 


install.packages(c("mice", "dplyr", "arrow", "readr", "tidyr"))
library(mice)
library(dplyr)
library(arrow)
library(readr)
library(tidyr)


#check level of missingness excluding the total antigenicity
#check for missingness using a function

check_missingness <- function(data) {

#Identify the Total Antigenicity column for this serotype
  total_antigenicity <- grep(
    "Total Antigenicity",
    names(data),
    value = TRUE
  )

  # Remove identifier columns
  feature_data <- data %>%
    select(
      -any_of(c(
        "batch_number",
        "material_number",
        "mbc_batch_number",
        "date_of_manufacture",
        total_antigenicity
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


#check if the function works for each of the serotypes
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


#need to decide on a threshold for each serotype
#need to identify those above a certain threshold
#need to create a function to remove those above a certain threshold

missing_Puurs_S01 <- check_missingness(Puurs_S01_altered)

View(missing_Puurs_S01)

#this doesnt work
variables_over_20 <- missing_Puurs_S01 %>%
  filter(missing_percent > 20)

# View the results
View(variables_over_20)


head(variables_over_20)



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

missing_above_20_removed <- lapply(names(serotype_data), function(dataset_name) {

  # Get the current serotype dataset
  dataset <- serotype_data[[dataset_name]]

  # Identify the Total Antigenicity column for this serotype
  total_antigenicity <- grep(
    "Total Antigenicity",
    names(data),
    value = TRUE
  )

  dataset %>%
    select(
      -any_of(c(
        "batch_number",
        "material_number",
        "mbc_batch_number",
        "date_of_manufacture",
        total_antigenicity
      )),
      -any_of(variables_to_drop[[dataset_name]])
    )

})

names(missing_above_20_removed) <- names(serotype_data)

#examing serotype S01 after removing variables with >20% missingness
missing_above_20_removed$Puurs_S01
View(missing_above_20_removed$Puurs_S01)
dim(missing_above_20_removed$Puurs_S01)
names(missing_above_20_removed$Puurs_S01)

names(missing_above_20_removed[["Puurs_S01"]])

#see how many feature variables remain in each serotype
sapply(missing_above_20_removed, ncol)



#need to change the following to missing_above_20_removed

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


#search the help page of mice to learn more about how to use it
??mice


#have not used this section yet. not sure where it fits in. before or after removing missingness?
#think it is used after removing missingness as it creates the train/test splits
#need to do this before imputation (i think)
#need to impute using the training data 

#convert the python code into r code for the split
#need to put the missing_above_20_removed in here somewhere

#check the names of the columns
#it is a list containing the 10 cleaned serotypes
names(missing_above_20_removed)

#make sure total antigenicity is in each serotype as this is what we will be predicting
names(missing_above_20_removed[["Puurs_S01"]])




split_total_antigenicity <- function(data, outcome_variable, 
                                     test_size = 0.2, seed = 123) {
  
  # Set a fixed seed so the split is reproducible
  set.seed(seed)
  
  # 1. Rows with measured Total Antigenicity
  measured_ta <- data[
    !is.na(data[[outcome_variable]]),
    ,
    drop = FALSE
  ]
  
  # 2. Rows with missing Total Antigenicity
  # Remove the outcome column because it is entirely missing here
  prediction_data <- data[
    is.na(data[[outcome_variable]]),
    ,
    drop = FALSE
  ]
  
  prediction_data[[outcome_variable]] <- NULL
  
  # 3. Identify the feature columns
  feature_cols <- setdiff(names(data), outcome_variable)
  
  # 4. Randomly split measured rows into training and testing sets
  n_test <- floor(nrow(measured_ta) * test_size)
  
  test_idx <- sample(
    seq_len(nrow(measured_ta)),
    size = n_test
  )
  
  train_idx <- setdiff(
    seq_len(nrow(measured_ta)),
    test_idx
  )
  
  # 5. Separate features (X) and outcome (y)
  X_train <- measured_ta[train_idx, feature_cols, drop = FALSE]
  y_train <- measured_ta[[outcome_variable]][train_idx]
  
  X_test <- measured_ta[test_idx, feature_cols, drop = FALSE]
  y_test <- measured_ta[[outcome_variable]][test_idx]
  
  # 6. Return the five objects
  return(list(
    X_train = X_train,
    y_train = y_train,
    X_test = X_test,
    y_test = y_test,
    prediction_data = prediction_data
  ))
}









#this was created before. have not used it yet. think the one above is a better fit.
Puurs_S01_altered <- read_csv("Puurs_S01_altered.csv")
names(Puurs_S01_altered)

outcome_variable <- "S01 Total Antigenicity"

# Rows where Total Antigenicity has been measured
measured_ta <- Puurs_S01_altered %>%
  filter(!is.na(.data[[outcome_variable]]))

# Rows where Total Antigenicity is missing
# Remove the outcome column because it is entirely missing here
prediction_data <- Puurs_S01_altered %>%
  filter(is.na(.data[[outcome_variable]])) %>%
  select(-all_of(outcome_variable))

# Check the number of rows
nrow(measured_ta)
nrow(prediction_data)
dim(measured_ta)
dim(prediction_data)