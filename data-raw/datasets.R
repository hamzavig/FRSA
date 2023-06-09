## code to prepare datasets goes here

library(readr)
library(readxl)

yieldCurves <- read_csv('data-raw/rf_yieldCurves.csv')
defaultCurves <- read_csv('data-raw/rf_defaultCurves.csv')
annuities <- read_csv('data-raw/ann_ptf.csv')
principalAtMaturities <- read_csv('data-raw/pam_ptf.csv')
operations <- read_csv('data-raw/operations.csv')

usethis::use_data(yieldCurves, defaultCurves, annuities, principalAtMaturities, operations, overwrite = TRUE)
