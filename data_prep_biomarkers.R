library(tidyverse)



# Functions ---------------------------------------------------------------
na_first <- function(x) first(na.omit(x))
na_last <- function(x) last(na.omit(x))
na_mean <- function(x) mean(x, na.rm = TRUE)

# Load data ---------------------------------------------------------------
tbl_bio <- read_csv(
  "https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/time_series_375_prerpocess_en.csv"
)


# Clean -------------------------------------------------------------------

# fill patient ID
tbl_bio <- tbl_bio %>% fill(PATIENT_ID)

# save mean
tbl_bio %>%
  group_by(PATIENT_ID, `Admission time`, `Discharge time`) %>%
  summarise_each(funs(na_first)) %>%
  write_csv("data/clean_biomarker_mean.csv")

# save first
tbl_bio %>%
  group_by(PATIENT_ID, `Admission time`, `Discharge time`) %>%
  summarise_each(funs(na_first)) %>%
  write_csv("data/clean_biomarker_first.csv")

# save last
tbl_bio %>%
  group_by(PATIENT_ID, `Admission time`, `Discharge time`) %>%
  summarise_each(funs(na_last)) %>%
  write_csv("data/clean_biomarker_last.csv")
