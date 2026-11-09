import os
import numpy as np
import pandas as pd
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer

RANDOM_STATE=42
imputer = IterativeImputer(random_state=RANDOM_STATE)

#  looks like imputation not peformed yet 

site_serotypes = {'Grange_Castle': ['S03', 'S04', 'S18C', 'S19A'],
                  'Puurs': ['S01', 'S03', 'S04', 'S09V', 'S18C', 'S19A']}


# We can loop over this, and store it in some format, dealers choice.. 

for site, serotypes in site_serotypes.items():
    print(f"Processing site: {site}")
    
    for serotype in serotypes:
        
        print(f"Processing serotype: {serotype}")
        
        # Read in the parquet file for the current site and serotype
        X_train = pd.read_parquet(os.path.join(f'../Parquet_files/{site}_{serotype}_X_train.parquet'))
        X_test = pd.read_parquet(os.path.join(f'../Parquet_files/{site}_{serotype}_X_test.parquet'))
        prediction_data = pd.read_parquet(os.path.join(f'../Parquet_files/{site}_{serotype}_prediction_data.parquet'))
        
        # Fit to training data and transform in one go.
        X_train_imputed = imputer.fit_transform(X_train)
        
        # Now predict missing ('transform') for test and prediction data
        X_test_imputed = imputer.transform(X_test)
        prediction_data_imputed = imputer.transform(prediction_data)
        
        # Sanity check here to check for missing vals 
        print(pd.DataFrame(X_train_imputed, columns=X_train.columns).isnull().sum())
        print(pd.DataFrame(X_test_imputed, columns=X_test.columns).isnull().sum())
        print(pd.DataFrame(prediction_data_imputed, columns=prediction_data.columns).isnull().sum())
        
        # If you want to save the imputed data back to parquet files, you can do so here.
        # remove this break so the loop continues for all serotypes
        break
    
    
    # remove this break so the loop continues for all sites
    break



'''
Now you have X_train, y_train, X_test, y_test data with no missing values
Can use X_train and y_train to fit a model and attempt to perform feature selection with it.
Do not use test data for feature selection - data leakage. 
The prediction data should only be used for making predictions after the model has been trained and validated in the test set, 
so you wont need it for a while until the model is ready to make predictions.
'''
