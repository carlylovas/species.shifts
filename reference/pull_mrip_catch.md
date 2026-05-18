# Pull federal MRIP catch estimates

Function to pull and clean recreational catch estimates. Data can be
acquired from the ACCSP Data Warehouse. Please download as
'Catch_Estimates.csv'

## Usage

``` r
pull_mrip_catch(proj_path)
```

## Arguments

- proj_path:

  Local path to data file

## Value

Data frame of catch estimates. Please refer to \[MRIP Data
Dictionary\](https://www.fisheries.noaa.gov/s3//2025-03/MRIP-Data-User-Handbook_March_2025_update.pdf.pdf)
for more information regarding estimates.

## Examples

``` r
# catch <- pull_mrip_catch(proj_path = proj_path)
```
