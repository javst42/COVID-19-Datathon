library(countrycode)
library(tidyverse)


# Questions ---------------------------------------------------------------
# are all deaths due to covid? 


# Todo --------------------------------------------------------------------
# symptoms need to be fixed before creating dummy data
# time-to-event column needs to be created
# what do  we do about data where end dates are not set?
# TRUE/FALSE death_outcome and hospitalized_outcome columns need to be 
## created where 1 means the event occurred and 0 means it was right-censored



# Load data ---------------------------------------------------------------
url_survival <- "https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/latestdata.csv"
url_survival <- "/Users/brunods/MIT/COVID_datathon/data/latestdata.csv"
url_merge <- "https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/bruno_798_patients_merged_survival_data.csv"
tbl_ncov19 <- read_csv(url_survival)
tbl_merge <- read_csv(url_merge)
glimpse(tbl_ncov19)
glimpse(tbl_merge)

tbl_symptom_dict <- read_csv("data/symptoms_dictionary.csv")

tbl_symptom_dict[duplicated(tbl_symptom_dict$original), ]

table(tbl_symptom_dict$`symptom class`)
table(tbl_symptom_dict$`standard symptom`)
length(unique(tbl_symptom_dict$original))
length(unique(tbl_symptom_dict$`symptom class`))
length(unique(tbl_symptom_dict$`standard symptom`))


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


# convert age to numeric
tbl_master$age %>% table()
tbl_master$age[tbl_master$age == "elderly"] <- "65"  # elderly is defined as 65yo
tbl_master$age[tbl_master$age == "<10"] <- "10"
tbl_master$age<- gsub("s", "", tbl_master$age)
tbl_master$age <- sapply(
  tbl_master$age, function(x) strsplit(x, "-")[[1]][1]
) # take the first number in a number range
tbl_master$age <- as.numeric(tbl_master$age)  # convert to numeric


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


# check for and remove duplicated samples
is_duplicated1 <- tbl_master %>% select(-id, -symptoms) %>% duplicated()
is_duplicated2 <- tbl_master %>% select(-id, -symptoms) %>% duplicated(fromLast=TRUE)
is_duplicated <- is_duplicated1 | is_duplicated2
tbl_master %>% 
  filter(is_duplicated) %>%
  arrange(country, outcome, age, sex, date_onset_symptoms, date_admission_hospital) %>%
  filter(!is.na(symptoms)) %>%
  tail()

table(is_duplicated)

tbl_master <- tbl_master[!is_duplicated1, ]
dim(tbl_master)

# add continent
tbl_master$continent <- countrycode(sourcevar = tbl_master$country,
                                    origin = "country.name",
                                    destination = "continent")
tbl_master$continent[tbl_master$country == "HK SAR"] <- "Asia"
table(tbl_master$continent)
tbl_master[is.na(tbl_master$continent), ]




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
count_threshold <- 15
df_symptom_counts %>% 
  filter(Freq > count_threshold) %>%
  ggplot() +
  geom_bar(aes(x = reorder(vec_symptoms, -Freq), y = Freq), stat = "identity") +
  ggtitle(paste("Count of symptoms with count >", count_threshold)) +
  theme_classic() +
  xlab("Symptoms")

vec_symptoms <- trimws(unique(vec_symptoms))
length(vec_symptoms)
length(intersect(tbl_symptom_dict$original, vec_symptoms))

# tbl_master %>% write_csv("data/survival_master.csv")


#  ---------------------------------------------------------------------------
# Here we need to fix symptom names before creating dummies
#  ---------------------------------------------------------------------------

# use dictionary to change symptom names
# create dummy
for (i in 1:nrow(tbl_master[is_symptom, ])) {
  symptom <- pull(tbl_master[is_symptom, "symptoms"])[i]
  if (is.na(symptom)) next
  list_symptoms <- strsplit(symptom, ",")[[1]]
  for (symptom_i in list_symptoms) {
    symptom_i <- trimws(symptom_i)
    is_in_dict <- tbl_symptom_dict$original == symptom_i
    if (sum(is_in_dict) > 1) {
      print("more than one key")
      print(i)
      break
    }
    symptom_new <- pull(tbl_symptom_dict[is_in_dict, 2])
    if (length(symptom_new) == 0) next
    if (is.na(symptom_new)) next
    if (symptom_new %in% colnames(tbl_master)) {
      tbl_master[is_symptom, symptom_new][i, ] <- 1
    } else {
      tbl_master[symptom_new] <- 0
    }
  }
}



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


# Some of these dates dont make sense... they must be typos
#
#
#
(date_end - date_start)[idx_neg]
print("Some dates dont make sense")
tbl_master[idx_neg, ] %>% select("id", starts_with("date"), "country")

# create a "days_to_event" column
tbl_master$days_to_event <- date_end - date_start
table(is.na(tbl_master$days_to_event))

## Not sure about this line:
tbl_master$days_to_event[is.na(tbl_master$days_to_event)] <- max(tbl_master$days_to_event[!is.na(tbl_master$days_to_event)])
  
# create death and hospitalized columns

outcome <- tolower(tbl_master$outcome)
table(outcome)
dead <- outcome
dead[is.na(dead)] <- 0
dead <- ifelse(
  dead == "dead" | dead == "death" | dead == "deceased" |
    dead == "died",
  1,
  0)
tbl_master$dead <- dead
hospitalized <- outcome
hospitalized[is.na(hospitalized)] <- 0
hospitalized <- ifelse(!is.na(tbl_master$date_admission_hospital), 1, 0)
tbl_master$hospitalized <- hospitalized


# tbl_master %>% write_csv("data/survival_master_dummy.csv")
table(apply(tbl_master %>% select(-days_to_event, -continent), 1, function(x) all(is.na(x))))



# EDA ---------------------------------------------------------------------
tbl_master %>% 
  drop_na(continent) %>%
  ggplot(aes(x = age, color = continent)) +
  geom_density() +
  theme_bw()

# distribution of age grouped by hospitalization
tbl_master %>% 
  ggplot(aes(x = age, color = as.factor(hospitalized))) +
  geom_density() +
  theme_bw()

# distribution of age grouped by death
tbl_master %>% 
  filter(continent == "Asia") %>%
  ggplot(aes(x = age, color = as.factor(dead))) +
  geom_density() +
  theme_bw()


# deaths by continents
tbl_master %>%
  drop_na(continent) %>%
  ggplot(aes(x = as.factor(continent), y = age, 
             fill = as.factor(dead),
             color = as.factor(dead))) +
  geom_violin(alpha = 0.2) + 
  geom_jitter(alpha = 0.4) +
  ggtitle("Deaths")


tbl_master %>%
  drop_na(continent) %>%
  ggplot(aes(x = as.factor(continent), y = age, 
             fill = as.factor(hospitalized),
             color = as.factor(hospitalized))) +
  geom_violin(alpha = 0.2) + 
  geom_jitter(alpha = 0.4) +
  ggtitle("Hospitalization")

