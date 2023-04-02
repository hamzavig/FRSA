## code to prepare datasets goes here

library(readr)
library(readxl)

yieldCurves <- read_csv('data-raw/rf_yieldCurves.csv')
annuities <- read_csv('data-raw/ptf_annuities.csv')

usethis::use_data(yieldCurves, annuities, overwrite = TRUE)
