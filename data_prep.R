library(tidyverse)


# Questions ---------------------------------------------------------------
# are all deaths due to covid? 


# Todo --------------------------------------------------------------------
# symptoms need to be fixed before creating dummy data



# Load data ---------------------------------------------------------------
url_survival <- "https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/latestdata.csv"
url_merge <- "https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/bruno_798_patients_merged_survival_data.csv"
tbl_ncov19 <- read_csv(url_survival)
tbl_merge <- read_csv(url_merge)
glimpse(tbl_ncov19)
glimpse(tbl_merge)

# Clean -------------------------------------------------------------------

# remove samples without date of symptom onset
is_date_onset <- !is.na(tbl_ncov19$date_onset_symptoms)
table(is_date_onset)

tbl_ncov19 <- tbl_ncov19[is_date_onset, ]
dim(tbl_ncov19)

# merge datasets
tbl_ncov19 %>% select(starts_with("date"))
tbl_ncov19 <- rename(tbl_ncov19, id = ID)
tbl_merge$date_confirmation <- NA # create new column with NA values before merging
cols_common <- c("id", "sex", "age", "country", "symptoms", "outcome", 
                 "date_onset_symptoms", "date_admission_hospital", 
                 "date_death_or_discharge", "date_confirmation")

tbl_master <- rbind(tbl_merge[cols_common], tbl_ncov19[cols_common])



# clean dates
## Some dates are ranged, here I take the first date by using substr to 
## get the first 10 characters.
tbl_master$date_onset_symptoms <-  as.Date(gsub("[.]", "/", substr(tbl_master$date_onset_symptoms, 1, 10)), format = '%d/%m/%Y')
tbl_master$date_admission_hospital <- as.Date(gsub("[.]", "/", substr(tbl_master$date_admission_hospital, 1, 10)), format = '%d/%m/%Y')
tbl_master$date_death_or_discharge <- as.Date(gsub("[.]", "/", substr(tbl_master$date_death_or_discharge, 1, 10)), format = '%d/%m/%Y')
tbl_master$date_confirmation <- as.Date(gsub("[.]", "/", substr(tbl_master$date_confirmation, 1, 10)), format = '%d/%m/%Y')

# replace ; with , in symptoms column
tbl_master$symptoms <- str_replace_all(tbl_master$symptoms, ";", ", ")
tbl_master$symptoms <- str_replace_all(tbl_master$symptoms, ":", ", ")


# separate data
## keep only symptoms
is_symptom <- !is.na(tbl_master$symptoms)
table(is_symptom)


# Dummify symptoms
# find unique symptoms
vec_symptoms <- c()
for (i in pull(tbl_master[is_symptom, "symptoms"])) {
  list_symptoms <- strsplit(i, ",")[[1]]
  for (symptom in list_symptoms) {
    symptom <- trimws(symptom)
    if (symptom == "") next
    vec_symptoms <- c(vec_symptoms, symptom)
  }
}
df_symptom_counts <- as.data.frame(table(vec_symptoms)) %>% arrange(desc(Freq))
count_threshold <- 25
df_symptom_counts %>% 
  filter(Freq > count_threshold) %>%
  ggplot() +
  geom_bar(aes(x = reorder(vec_symptoms, -Freq), y = Freq), stat = "identity") +
  ggtitle(paste("Count of symptoms with count >", count_threshold)) +
  theme_classic() +
  xlab("Symptoms")

vec_symptoms <- trimws(unique(vec_symptoms))

tbl_master %>% write_csv("data/survival_master_duplicates_not_checked.csv")


#  ---------------------------------------------------------------------------
# Here we need to fix symptom names before creating dummies
#  ---------------------------------------------------------------------------

# create dummy
for (i in 1:nrow(tbl_master[is_symptom, ])) {
  symptom <- pull(tbl_master[is_symptom, "symptoms"])[i]
  if (is.na(symptom)) next
  for (symptom_i in strsplit(symptom, ",")[[1]]) {
    if (symptom_i %in% colnames(tbl_master)) {
      if (symptom_i == "") next
      #if (symptom_i == ",") next
      
      tbl_master[is_symptom, symptom_i][i, ] <- 1
    } else {
      tbl_master[symptom_i] <- 0
    }
  }
}



####
# CODE BELOW IS  INCOMPLETE
####


# create time-to-events
table(is.na(tbl_master$date_onset_symptoms))
table(is.na(tbl_master$date_admission_hospital))
table(is.na(tbl_master$date_confirmation))
table(is.na(tbl_master$date_death_or_discharge))

date_start <- tbl_master$date_onset_symptoms
date_end <- tbl_master$date_death_or_discharge
date_end[is.na(date_end)] <- tbl_master$date_confirmation[is.na(date_end)]
date_end[is.na(date_end)] <- tbl_master$date_admission_hospital[is.na(date_end)]
idx_neg <- (date_end - date_start) < 0
idx_neg[is.na(idx_neg)] <- FALSE

(date_end - date_start)[idx_neg]
print("Some dates dont make sense")
tbl_master[idx_neg, ] %>% select("ID", starts_with("date"), "country")

# create a "days_to_event" column
tbl_master$days_to_event <-
  
# create death and hospitalized columns
tbl_master$death
tbl_master$hospitalized


times <-  tbl_km$date_end - tbl_km$date_onset_symptoms
status <- tbl_km$dead
surv_group <- tbl_km$country
idx_keep <- !(is.na(tbl_km$date_end) | times < 0)
tbl_km_plot <- tbl_km
