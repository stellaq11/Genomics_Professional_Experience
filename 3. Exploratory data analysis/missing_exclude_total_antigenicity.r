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

missing_above_20_removed$Puurs_S03
View(missing_above_20_removed$Puurs_S03)
dim(missing_above_20_removed$Puurs_S03)
names(missing_above_20_removed$Puurs_S03)

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


#run the function across all 10 serotypes

# Create training, testing and prediction datasets
splits_all_serotypes <- lapply(
  names(missing_above_20_removed),
  function(dataset_name) {
    
    # Get the cleaned dataset
    data <- missing_above_20_removed[[dataset_name]]
    
    # Extract the serotype identifier
    serotype <- sub(
      "^(Puurs|Grange_Castle)_",
      "",
      dataset_name
    )
    
    # Define the Total Antigenicity outcome column
    outcome_variable <- paste(
      serotype,
      "Total Antigenicity"
    )
    
    # Split the dataset
    split_total_antigenicity(
      data = data,
      outcome_variable = outcome_variable,
      test_size = 0.2,
      seed = 123
    )
  }
)

# Give each list element its dataset name
names(splits_all_serotypes) <- names(missing_above_20_removed)


#assessing the results for a particular serotype

X_train_Puurs_S01 <- splits_all_serotypes$Puurs_S01$X_train
X_train_Puurs_S01
y_train_Puurs_S01 <- splits_all_serotypes$Puurs_S01$y_train

X_test_Puurs_S01 <- splits_all_serotypes$Puurs_S01$X_test
y_test_Puurs_S01 <- splits_all_serotypes$Puurs_S01$y_test

prediction_data_Puurs_S01 <- splits_all_serotypes$Puurs_S01$prediction_data



#want to save the files for input into python
install.packages("jsonlite")
library(jsonlite)

write_json(
  X_train_Puurs_S01,
  path = "X_train_Puurs_S01.json",
  dataframe = "rows",
  pretty = TRUE,
  na = "null",
  auto_unbox = TRUE
)

y_train_Puurs_S01 <- splits_all_serotypes$Puurs_S01$y_train
y_train_Puurs_S01

write_json(
  y_train_Puurs_S01,
  path = "y_train_Puurs_S01.json",
  dataframe = "rows",
  pretty = TRUE,
  na = "null",
  auto_unbox = TRUE
)

#function to create json files for all 10 serotypes
# Create a folder to store all JSON files
output_folder <- "JSON_files"

dir.create(
  output_folder,
  showWarnings = FALSE,
  recursive = TRUE
)

# Loop through all 10 serotypes
for (serotype in names(splits_all_serotypes)) {

  split <- splits_all_serotypes[[serotype]]

  # Save each of the five datasets
  for (dataset_name in c(
    "X_train",
    "y_train",
    "X_test",
    "y_test",
    "prediction_data"
  )) {

    dataset <- split[[dataset_name]]

    # Create a filename
    file_name <- paste0(
      serotype, "_",
      dataset_name,
      ".json"
    )

    # Save as JSON
    jsonlite::write_json(
      dataset,
      path = file.path(output_folder, file_name),
      dataframe = "rows",
      pretty = TRUE,
      na = "null",
      auto_unbox = TRUE
    )
  }
}

#check that 50 files were created
json_files <- list.files(
  "JSON_files",
  pattern = "\\.json$",
  full.names = TRUE
)

length(json_files)


#check if the serotypes have the correct training and test dimensions
lapply(splits_all_serotypes, function(split) {
  list(
    X_train = dim(split$X_train),
    y_train = dim(as.data.frame(split$y_train)),
    X_test = dim(split$X_test),
    y_test = dim(as.data.frame(split$y_test)),
    prediction_data = dim(split$prediction_data)
  )
})

#save 50 files as parquet as well
# Create a folder for the Parquet files
output_folder <- "Parquet_files"

dir.create(
  output_folder,
  showWarnings = FALSE,
  recursive = TRUE
)

# Dataset names to export for every serotype
dataset_names <- c(
  "X_train",
  "y_train",
  "X_test",
  "y_test",
  "prediction_data"
)

# Loop through all 10 serotypes
for (serotype in names(splits_all_serotypes)) {

  split <- splits_all_serotypes[[serotype]]

  for (dataset_name in dataset_names) {

    dataset <- split[[dataset_name]]

    # Convert the dataset to a data frame
    dataset <- as.data.frame(dataset)

    # Create the output filename
    file_name <- paste0(
      serotype, "_",
      dataset_name,
      ".parquet"
    )

    # Write the Parquet file
    arrow::write_parquet(
      dataset,
      sink = file.path(output_folder, file_name)
    )
  }
}

#check 50 files were created
parquet_files <- list.files(
  "Parquet_files",
  pattern = "\\.parquet$",
  full.names = TRUE
)

length(parquet_files)



#check the split sizes for all serotypes

split_summary <- do.call(
  rbind,
  lapply(names(splits_all_serotypes), function(dataset_name) {
    
    result <- splits_all_serotypes[[dataset_name]]
    
    data.frame(
      dataset = dataset_name,
      training_rows = nrow(result$X_train),
      testing_rows = nrow(result$X_test),
      prediction_rows = nrow(result$prediction_data)
    )
  })
)

print(split_summary)

#observations from split_summary
#The Puurs datasets have substantially more rows reserved for prediction than the Grange Castle datasets. This reflects the amount of missing Total Antigenicity in the cleaned data, rather than a problem with your splitting function.
#Grange Castle S04 has the smallest measured dataset, with 762 observations available for training and testing combined. This is worth considering when you compare model performance across serotypes.

# verify that the outcome values are never missing in the training and testing sets
#this should be equal to 0. it is 

sapply(splits_all_serotypes, function(x) {
  c(
    missing_y_train = sum(is.na(x$y_train)),
    missing_y_test = sum(is.na(x$y_test))
  )
})

#check for missing values in the predictor variables
#counts of missing cells, not counts of affected rows

sapply(splits_all_serotypes, function(x) {
  sum(is.na(x$X_train))
})

#calculate the percentage of missing predictor values


missing_predictor_summary <- do.call(
  rbind,
  lapply(names(splits_all_serotypes), function(dataset_name) {
    
    X_train <- splits_all_serotypes[[dataset_name]]$X_train
    
    data.frame(
      dataset = dataset_name,
      training_rows = nrow(X_train),
      predictor_columns = ncol(X_train),
      missing_cells = sum(is.na(X_train)),
      total_cells = length(as.matrix(X_train)),
      missing_percent = round(
        100 * sum(is.na(X_train)) / length(as.matrix(X_train)),
        2
      )
    )
  })
)

print(missing_predictor_summary)


#MICE imputation next

#look at the data types in cleaned training data 

# Inspect predictor data types for Puurs S01
str(splits_all_serotypes$Puurs_S01$X_train)

# Count missing values by predictor
sort(
  colSums(is.na(splits_all_serotypes$Puurs_S01$X_train)),
  decreasing = TRUE
)






#code for MICE imputation

library(mice)

# Extract the three datasets
s01 <- splits_all_serotypes$Puurs_S01

X_train <- as.data.frame(s01$X_train)
X_test <- as.data.frame(s01$X_test)
prediction_data <- as.data.frame(s01$prediction_data)

# Combine predictors in the same column structure
all_predictors <- rbind(
  X_train,
  X_test,
  prediction_data
)

# TRUE means the row is excluded when fitting
# the imputation models, but can still be imputed
ignore_rows <- c(
  rep(FALSE, nrow(X_train)),
  rep(TRUE, nrow(X_test) + nrow(prediction_data))
)

# Run MICE using predictive mean matching
set.seed(123)

mice_s01 <- mice(
  data = all_predictors,
  m = 5,
  maxit = 10,
  method = "pmm",
  ignore = ignore_rows,
  seed = 123,
  printFlag = TRUE
)







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





#want to save splits_all_serotypes from vscode so i can upload it to posit cloud
# Check that your object exists
exists("splits_all_serotypes")

# Check its structure
str(splits_all_serotypes, max.level = 2)

#saving the data in RDS format rather than CSV. RDS preserves your complete R object, including the nested list structure, data frames, column names, and data types.
#this is due to the large file size and the difficulty running mice in vscode
# Save the complete object
saveRDS(
  splits_all_serotypes,
  file = "splits_all_serotypes.rds"
)

# Confirm the file location
normalizePath("splits_all_serotypes.rds")

#check if the file exists
file.exists("splits_all_serotypes.rds")

#check the file size before uploading
#this gives the file size in MB
#max upload is 500MB on posit cloud
file.info("splits_all_serotypes.rds")$size / 1024^2
#this returns 2.465128

#uploading to positcloud
# Install the required packages
install.packages("mice")

# Load MICE
library(mice)

# Load your saved dataset
splits_all_serotypes <- readRDS(
  "splits_all_serotypes.rds"
)

# Check that it loaded correctly
str(splits_all_serotypes, max.level = 2)

# List the datasets
names(splits_all_serotypes)
#this returns "Puurs_S01", "Puurs_S03", "Puurs_S04", "Puurs_S09V", "Puurs_S18C", "Puurs_S19A", "Grange_Castle_S03", "Grange_Castle_S04", "Grange_Castle_S18C", "Grange_Castle_S19A"

#verify the training, test, and prediction splits
# Inspect the structure of one dataset
str(splits_all_serotypes$Puurs_S01, max.level = 1)

# Check the training predictors
dim(splits_all_serotypes$Puurs_S01$X_train)

# Check the testing predictors
dim(splits_all_serotypes$Puurs_S01$X_test)

# Check the prediction predictors
dim(splits_all_serotypes$Puurs_S01$prediction_data)

packageVersion("mice") # ‘3.19.0’

#check missing values before imputing
# Missing values in the training predictors
colSums(is.na(
  splits_all_serotypes$Puurs_S01$X_train
))

# Missing values in the testing predictors
colSums(is.na(
  splits_all_serotypes$Puurs_S01$X_test
))

#check missingness across all 10 training datasets
#see which predictor columns contain missing data in the training dataset

missing_summary_before_imputation <- lapply(
  names(splits_all_serotypes),
  function(nm) {

    x <- splits_all_serotypes[[nm]]$X_train

    data.frame(
      dataset = nm,
      n_rows = nrow(x),
      n_predictors = ncol(x),
      missing_cells = sum(is.na(x)),
      columns_with_missing = sum(colSums(is.na(x)) > 0),
      entirely_missing_columns = sum(
        colSums(!is.na(x)) == 0
      )
    )
  }
)

missing_summary_before_imputation <- do.call(rbind, missing_summary_before_imputation)

print(missing_summary_before_imputation)


#check which method is most suitable
# Inspect predictor types
sapply(
  splits_all_serotypes$Puurs_S01$X_train,
  class
)

# Inspect missingness for each predictor
sort(
  colSums(is.na(
    splits_all_serotypes$Puurs_S01$X_train
  )),
  decreasing = TRUE
)


#run a small imputation test on Puurs_S01

# Select the training predictors for Puurs_S01
X_train_S01 <- splits_all_serotypes$Puurs_S01$X_train

# Confirm all predictors are numeric
stopifnot(all(vapply(X_train_S01, is.numeric, logical(1))))

# Run a small MICE test
set.seed(123)

mice_test_S01 <- mice(
  data = as.data.frame(X_train_S01),
  m = 2,
  maxit = 5,
  method = "pmm",
  seed = 123,
  printFlag = FALSE
)

# Inspect the result
print(mice_test_S01)

# Check the first completed training dataset
X_train_S01_imputed <- complete(mice_test_S01, action = 1)

# Confirm there are no missing predictor values
sum(is.na(X_train_S01_imputed))

#check imputation diagnostics
# Review convergence plots
plot(mice_test_S01)

# Check for warnings or logged events
mice_test_S01$loggedEvents


X <- as.data.frame(
  splits_all_serotypes$Puurs_S01$X_train
)

# 1. Identify constant or nearly constant predictors
unique_counts <- vapply(
  X,
  function(x) length(unique(x[!is.na(x)])),
  integer(1)
)

print(unique_counts[unique_counts <= 1])

# 2. Calculate pairwise correlations
cor_matrix <- cor(
  X,
  use = "pairwise.complete.obs"
)

# 3. List predictor pairs with absolute correlation >= 0.95
high_cor <- which(
  upper.tri(cor_matrix) &
    abs(cor_matrix) >= 0.95,
  arr.ind = TRUE
)

if (nrow(high_cor) > 0) {
  high_cor_results <- data.frame(
    predictor_1 = colnames(cor_matrix)[high_cor[, 1]],
    predictor_2 = colnames(cor_matrix)[high_cor[, 2]],
    correlation = cor_matrix[high_cor]
  )
  
  print(high_cor_results)
} else {
  print("No predictor pairs have absolute correlation >= 0.95")
}

# 4. Check for exactly duplicated columns
duplicated_columns <- names(X)[
  duplicated(as.list(X))
]

print(duplicated_columns)

# Make a copy with safe column names for diagnostics
X_plot <- as.data.frame(
  splits_all_serotypes$Puurs_S01$X_train
)

names(X_plot) <- paste0(
  "V",
  seq_along(X_plot)
)

set.seed(123)

mice_plot_test <- mice(
  X_plot,
  m = 2,
  maxit = 5,
  method = "pmm",
  seed = 123,
  printFlag = FALSE
)

plot(mice_plot_test)

# Inspect MICE's logged events
mice_plot_test$loggedEvents

missing_counts <- colSums(is.na(X))

missing_counts[missing_counts > 0]

# Variables and methods used by MICE
data.frame(
  variable = names(mice_plot_test$method),
  method = mice_plot_test$method
) |>
  subset(method != "")

#which variables are being imputed
data.frame(
  temporary_name = names(X_plot),
  original_name = names(X)
)

X <- as.data.frame(
  splits_all_serotypes$Puurs_S01$X_train
)

# Give columns safe temporary names
names(X) <- paste0("V", seq_along(X))

# Initialise MICE to obtain default settings
ini <- mice(
  X,
  maxit = 0,
  printFlag = FALSE
)

meth <- ini$method
pred <- ini$predictorMatrix

# Only impute variables that have missing values
meth[colSums(is.na(X)) == 0] <- ""

# Exclude V1 and V4 as predictors in the
# imputation models, while retaining the columns
pred[, c("V1", "V4")] <- 0

# Do not use a variable to predict itself
diag(pred) <- 0

set.seed(123)

mice_S01_revised <- mice(
  data = X,
  m = 5,
  maxit = 10,
  method = meth,
  predictorMatrix = pred,
  seed = 123,
  printFlag = FALSE
)

# Inspect warnings and logged events
mice_S01_revised$loggedEvents

# Check remaining missing values in one completed dataset
X_train_S01_imputed <- complete(
  mice_S01_revised,
  action = 1
)

sum(is.na(X_train_S01_imputed))

# Inspect the trace plots
plot(mice_S01_revised)

# Extract the first completed dataset
X_train_S01_imputed <- complete(
  mice_S01_revised,
  action = 1
)

# Restore original predictor names
names(X_train_S01_imputed) <-
  names(splits_all_serotypes$Puurs_S01$X_train)

# Verify
sum(is.na(X_train_S01_imputed))

# Check dimensions are unchanged
dim(X_train_S01_imputed)
dim(splits_all_serotypes$Puurs_S01$X_train)