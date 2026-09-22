# MA5234

## Data Location

Install `aws` command line tool and run:

```console
aws s3 cp s3://digbyb/MA5234/PCV_Stacked.parquet . --no-sign-request
```

Pilib/Leonel should be able to assist with this. 

## Data Structure

`PCV_Stacked.parquet` contains P02, Q01, S0X and Total Antigenicity measurements for pneumococcal serotype variants packaged in Grange Castle and Puurs, respectively:

| Site | Variants |
|---|---|
| Puurs | S01, S03, S04, S09V, S18C, S19A |
| Grange Castle | S03, S04, S18C, S19A |

Given each serotype undergoes its own chemical process and is filled at two different plants, **each serotype/site combination must be modelled independently**:

```python
import polars as pl

var_mat = {
    'Puurs': ['S01', 'S03', 'S04', 'S09V', 'S18C', 'S19A'],
    'Grange Castle': ['S03', 'S04', 'S18C', 'S19A']
}

pcv_stacked = pl.read_parquet("PCV_Stacked.parquet")

for site, variant_list in var_mat.items():

    for variant in variant_list:

        print(f"Processing site: {site}, variant: {variant}")

        df = pcv_stacked.filter(
            (pl.col('manufacture_site') == site) &
            (pl.col('variant') == variant)
        )

        pivot_index = ['batch_number', 'material_number', 'product_name', 'mbc_batch_number', 'date_of_manufacture']

        # very small number of duplicated rows - aggregate their value
        df_wide = df.pivot(
            on='parameter', 
            values='weighted_avg_value', 
            index=pivot_index,
            aggregate_function='mean'
        )

        file_handle = f"{site.replace(' ', '_')}_{variant}"
        df_wide.write_csv(f"{file_handle}.csv")

```

Each site/variant combination is saved to a wide table with `batch_number`, `material_number`, `product_name`, `mbc_batch_number`, `date_of_manufacture` metadata, followed by the feature variables and the outcome variable total antigenicity (e.g "S18C Total Antigenicity").

Rows with values for Total Antigenicity are to be used to build, train and test the model. Once the model has been developed, use it to predict Total Antigenicity in rows for which there is no Total Antigenicity measurement. 

Recall that each site/serotype is treated independently, thus the "Grange Castle S03" model should only be used to predict values for that site/serotype combination. 

## Aggregating Predictions

Each row in the dataset represents a unique Batch Number linked to an MBC Batch Number. Because a single MBC Batch can feed into multiple downstream Batch Numbers, Total Antigenicity predictions should first be generated at the Batch Number level, then aggregated up to the MBC Batch Number level.

## Incomplete Data

The feature variables contain missing values. Devise a strategy for imputing these values to minimise loss of power (e.g if a variable has more than X% missing data, decide to drop it or impute mising values).

Imputation using median/mean values is too reductive, investigate the use of a multivariate imputation methods for this task.

## Data Limits

Below are the acceptable limits for Total Antigenicity measurements:

| serotype | high risk | low risk |
|---|---|---|
| GC S04 | 5.6 | 5.4 |
| GC S19A | 3.55 | 4.0 |
| GC S03 | 3.9 | 4.1 |
| GC S18C | 5.35 | 4.65 |
| PU S01 | 5.4 | 4.7 |
| PU S03 | 4.0 | 4.1 |
| PU S04 | 5.5 | 4.7 |
| PU S09V | 5.5 | 4.5 |
| PU S18C | 5.35 | 4.65 |
| PU S19A | 3.7 | 4.0 |

_GC: Grange Castle, PU: Puurs_

Each serotype has different limits for what is considered 'out of spec'. Please note that depending on the serotype, this can be high or low levels of Total Antigenicity. 

For example, if the Grange Castle S03 model returns a point prediction of 4.0, this falls within the limits of 3.9 & 4.1 and is considered 'medium risk'. A point prediction of 4.2 is considered 'low-risk' and a point prediction of 3.8 is considered 'high-risk'. 

**It is crucial to generate prediction intervals for point predictions to model the uncertainty around each point prediction. By using the point prediction and its prediction interval, you can assign low/medium/high risk values with greater accuracy.**

## Variable Description

Serotype saccharides are measured at three steps:

- `P02 | (P02/Q01)`: Saccharide Concentration is measured after activation and conjugation.
- `Q01`: Saccharide Concentration / Yield, Protein Concentration is measured after adding a buffer for dilution.
- `S0X`: Saccharide concentration / Yield, Protein Concentration is measured after the product is filtered and added to a bag.

The measurements taken at this stage are used to determine the quantity added to MBC Bags (S0X) for formulation. Once Drug Product formulation has taken place (serotypes mixed) the Total Antigenicity is measured. This is used as a proxy for testing the saccharide concentration in the final product. 

The models generated in this project will be capable of predicting 'low/medium/high' risk MBC bags where 'risk' categorises the level of saccharide present. The model can thus be used to advise the matching of MBC bags for formulation and identify procedural issues.

## Conclusion

At its core, this project requires predicting a continuous outcome (Total Antigenicity) for each serotype/site combination in the dataset. 

Please feel free to contact me for further clarification or discussions surrounding the source data `Barry.Digby@pfizer.com`