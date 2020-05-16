library(BallMapper)
library(Boruta)
library(caret)
library(impute)
library(survival)
library(survminer)
library(tidyverse)



# Load data ---------------------------------------------------------------
url_survival <- 'https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/survival_master_dummy.csv'

tbl_master <- read_csv(url_survival)
tbl_master

# clean dates
Sys.setenv(TZ = "America/New_York")
tbl_master$date_onset_symptoms <-  as.Date(tbl_master$date_onset_symptoms)
tbl_master$date_admission_hospital <- as.Date(tbl_master$date_admission_hospital, 1, 10)
tbl_master$date_death_or_discharge <- as.Date(tbl_master$date_death_or_discharge, 1, 10)
tbl_master$date_confirmation <- as.Date(tbl_master$date_confirmation, 1, 10)

table(is.na(tbl_master$date_onset_symptoms))
table(is.na(tbl_master$date_admission_hospital))
table(is.na(tbl_master$date_death_or_discharge))
table(is.na(tbl_master$date_confirmation))

# clean outcomes
table(tbl_master$outcome)
tbl_master$outcome <- tolower(tbl_master$outcome)
outcome_dict <- list(
  "censored" = c(
    "censored",
    NA, "critical condition, intubated as of 14.02.2020", "not hospitalized",
    "severe", "stable", "symptoms only improved with cough. currently hospitalized for follow-up.",
    "treated in an intensive care unit (14.02.2020)", "alive", "stable condition",
    "under treatment", "critical condition", "receiving treatment", "severe illness", "unstable"),
  "recovered" = c(
    "discharge", "discharged", "discharged from hospital", "recovered",
    "recovering at home 03.03.2020", "released from quarantine"
  ),
  "dead" = c(
    "death", "dead", "died", "deceased"
  )
)
outcome_dict <- Biobase::reverseSplit(outcome_dict)

tbl_master$outcome_surv <- tbl_master$outcome
tbl_master$outcome_surv[is.na(tbl_master$outcome_surv)] <- "censored"
tbl_master$outcome_surv <- unname(unlist(outcome_dict[tbl_master$outcome_surv]))
table(tbl_master$outcome_surv)

tbl_master$outcome_hospitalized <- ifelse(!is.na(tbl_master$date_admission_hospital), 1, 0)
table(tbl_master$outcome_hospitalized)


# Create time to event columns --------------------------------------------

# time to hospitalization
tbl_master$date_start <- tbl_master$date_onset_symptoms
tbl_master$date_start[is.na(tbl_master$date_start)] <- tbl_master$date_confirmation[is.na(tbl_master$date_start)]
# add censored samples: those who did not go to hospital and have not recovered or died
is_censored_hospitalized <- (tbl_master$outcome_surv == "censored") & (tbl_master$outcome_hospitalized == 0)
table(is_censored_hospitalized)
date_end <- tbl_master$date_admission_hospital
date_end[is_censored_hospitalized] <- as.Date("2020-05-11") # setting censored date to when dataset was last updated
# calculate time to event
tbl_master$time_to_hospital <- as.numeric(date_end - tbl_master$date_start)
tbl_master$time_to_hospital[tbl_master$time_to_hospital <= 0] <- NA # the problem is some confirmations occur after hospitalization
table(is.na(tbl_master$time_to_hospital))
plot(density(na.omit(tbl_master$time_to_hospital)))


# Feature selection with Boruta -------------------------------------------
# time to hospitalization
df_surv <- data.frame(time = tbl_master$time_to_hospital, status = tbl_master$outcome_hospitalized)
df_surv <- cbind(df_surv, tbl_master[, c(15:50)])
table(df_surv$status)
# remove some censored samples... too many for boruta
idx_hosp <- c(1:nrow(tbl_master))[!is.na(tbl_master$date_admission_hospital)]
idx_keep <- sample(c(1:nrow(tbl_master))[-idx_hosp], 500) 
idx_keep <- c(idx_hosp, idx_keep) 
df_surv <- df_surv[idx_keep, ]
df_surv <- df_surv[!is.na(df_surv$time), ]
any(is.na(df_surv))
#df_surv <- impute.knn(df_surv, k = 10)
apply(df_surv, 2, function(x) any(is.na(x)))
bdf <- Boruta(Surv(time, status) ~ ., data = df_surv, doTrace = 2)
print(bdf)
cols_important <- names(bdf$finalDecision)[bdf$finalDecision == "Confirmed"]
cols_important <- gsub(pattern = "`", replacement = "", x = cols_important)
plot(bdf)
df_imp <- data.frame(Biomarker = names(sort(colMeans(bdf$ImpHistory), decreasing = TRUE)),
                     Importance = unname(sort(colMeans(bdf$ImpHistory), decreasing = TRUE)))
colnames(df_imp)
df_imp %>% write_csv("results/boruta_symptoms_hospitalization_importance.csv")
cols_important <- names(sort(colMeans(bdf$ImpHistory), decreasing = TRUE)[1:5])
cols_important <- gsub(pattern = "`", replacement = "", x = cols_important)




# BallMapper --------------------------------------------------------------
normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
idx_hosp <- c(1:nrow(tbl_master))[!is.na(tbl_master$date_admission_hospital)]
idx_keep <- sample(c(1:nrow(tbl_master))[-idx_hosp], 15000) 
idx_keep <- c(idx_hosp, idx_keep) 
X <- tbl_master[idx_keep, cols_important]
y <- tbl_master$outcome_hospitalized[idx_keep]
idx <- complete.cases(X)
table(idx)
values <- as.data.frame(y)
points <- normalize(X)
bm <- BallMapper(points = points, 
                 values = values, 
                 epsilon = 0.3)
ColorIgraphPlot(bm, 
                minimal_ball_radius = 7,
                showVertexLabels = TRUE,
                showLegend = TRUE)

make_ballmapper_summary <- function(data, bm, vec_node = 1) {
  pts <- points_covered_by_landmarks(bm, vec_node)
  subset <- data[pts,]
  av <- numeric(ncol(subset)) 
  stdev <- numeric(ncol(subset)) 
  num_samples <- nrow(subset)
  for ( i in 1:ncol(subset) ) {
    av[i] <- mean(subset[,i])
    stdev[i] <- sd(subset[,i]) 
  }
  data.frame(feature = colnames(data),
             average = av,
             stdev = stdev,
             num_samples = num_samples)
}
group1 <- make_ballmapper_summary(cbind(X, time, y), bm, 1)
group2 <- make_ballmapper_summary(cbind(X, time, y), bm, 2)
group3 <- make_ballmapper_summary(cbind(X, time, y), bm, 3)
group4 <- make_ballmapper_summary(cbind(X, time, y), bm, 4)
group5 <- make_ballmapper_summary(cbind(X, time, y), bm, 5)

list_groups <- list(
  group1, group2, group3, group4, group5
)

vec_age <- c()
vec_lymph <-c()
vec_crp <-c()
vec_ldh <- c()
vec_time <- c()
vec_y <- c()
vec_num_samples <- c()
for (group in list_groups) {
  vec_age <- c(vec_age, group$average[1])
  vec_lymph <- c(vec_lymph, group$average[2])
  vec_crp <- c(vec_crp, group$average[3])
  vec_ldh <- c(vec_ldh, group$average[4])
  vec_time <- c(vec_time, group$average[5])
  vec_y <- c(vec_y, group$average[6])
  vec_num_samples <- c(vec_num_samples, group$num_samples[1])
}
plot(vec_age)
plot(vec_lymph)
plot(vec_crp)
plot(vec_ldh)
plot(vec_time)
plot(vec_y)

df <- data.frame(group = 1:5, age = vec_age,
                 lymphocytes = vec_lymph,
                 hcCRP = vec_crp,
                 LDH = vec_ldh,
                 time = vec_time,
                 death = vec_y,
                 num_samples = vec_num_samples
)
df %>% ggplot(aes(x = group, y = 100 * death, size = num_samples)) +
  geom_point() +
  theme_classic() +
  ylab("Death %") +
  xlab("BallMapper Group") +
  ggtitle("Deaths across groups") +
  theme(text = element_text(size=28)) 
df %>% ggplot(aes(x = group, y = time, size = num_samples)) +
  geom_point() +
  theme_classic()
df %>% ggplot(aes(x = group, y = LDH, size = num_samples)) +
  geom_point() +
  theme_classic()
















# Train test split validate -----------------------------------------------
# 80% 10% 10% train test val
set.seed(42)
train_index <- as.vector(createDataPartition(tbl_master$dead, p = 0.8, list = FALSE))

test_index <- setdiff(1:nrow(tbl_master), train_index)



# Boruta ------------------------------------------------------------------
# hospitalized
set.seed(42)
colnames(tbl_master)


tbl_boruta <- tbl_master[train_index, c(2, 3, 15:52, 53, 54)] # include days_to_event, dead column

table(tbl_boruta$dead)
table(tbl_boruta$sex)

# remove samples to make algorithm run faster

tbl_censored <- tbl_boruta[tbl_boruta$dead == 0, ]
tbl_boruta <- rbind(
  tbl_boruta[tbl_boruta$dead == 1, ], 
  tbl_censored[sample(1:nrow(tbl_censored), nrow(tbl_censored)*0.2), ]
)

dim(tbl_boruta)
table(tbl_boruta$dead)
tbl_boruta$sex <- ifelse(tbl_boruta$sex == "male", 1, 0)
table(complete.cases(tbl_boruta))
tbl_boruta <-  tbl_boruta[complete.cases(tbl_boruta), ]
dim(tbl_boruta)
colnames(tbl_boruta)

boruta_deaths <- Boruta(Surv(days_to_event, dead) ~ ., data = tbl_boruta, doTrace = 2)
print(boruta_deaths)
cols_important <- names(boruta_deaths$finalDecision)[boruta_deaths$finalDecision != "Rejected"]
plot(boruta_deaths)
save.image("COVID_boruta.RData")

tbl_boruta$dead <- factor(tbl_boruta$dead)

tbl_boruta <- tbl_boruta %>% mutate_if(is.numeric, as.factor)
tbl_master[, colnames(tbl_boruta)] <- tbl_master[, colnames(tbl_boruta)] %>% mutate_if(is.numeric, as.factor)

tbl_boruta$age <- as.numeric(tbl_boruta$age)
tbl_master$age <- as.numeric(tbl_master$age)
tbl_master$days_to_event <- as.numeric(tbl_master$days_to_event)

colnames(tbl_boruta)[colnames(tbl_boruta) == "organ failure"] <- "organ_failure"
colnames(tbl_master)[colnames(tbl_master) == "organ failure"] <- "organ_failure"

table(tbl_master$outcome)
tbl_master$outcome <- tolower(tbl_master$outcome)
outcome_dict <- list(
  "censored" = c(
    "censored",
    NA, "critical condition, intubated as of 14.02.2020", "not hospitalized",
    "severe", "stable", "symptoms only improved with cough. currently hospitalized for follow-up.",
    "treated in an intensive care unit (14.02.2020)", "alive", "stable condition",
    "under treatment", "critical condition", "receiving treatment", "severe illness", "unstable"),
  "recovered" = c(
    "discharge", "discharged", "discharged from hospital", "recovered",
    "recovering at home 03.03.2020", "released from quarantine"
  ),
  "dead" = c(
    "death", "dead", "died", "deceased"
  )
)
outcome_dict <- Biobase::reverseSplit(outcome_dict)

tbl_master$outcome_surv <- tbl_master$outcome
tbl_master$outcome_surv[is.na(tbl_master$outcome_surv)] <- "censored"
tbl_master$outcome_surv <- unname(unlist(outcome_dict[tbl_master$outcome_surv]))


tbl_master$fever


# EDA ---------------------------------------------------------------------
for (i in colnames(tbl_boruta)[!(colnames(tbl_boruta) %in% c("age", "days_to_event"))]) {
  print(i)
  plot_title <- paste0("Comparing life/death for symptom: ", i)
  plt <- tbl_master %>% 
    ggplot(aes_string(fill = i, x = "outcome_surv")) + 
    geom_bar(aes(y = log(..count..)),
             position = "dodge") +
    ggtitle(plot_title) +
    xlab("Patient outcome") +
    ylab("log(count)") +
    theme_classic()
  #print(plt)
  #cont <- readline("Continue?")
  #if (cont == "n") break
  fname <- ifelse(
    i %in% cols_important,
    paste0("results/barplots/", i, "_barplot.png"),
    paste0("results/barplots/", i, "_barplot_important.png")
  )
  ggsave(fname, plt)
}


# KM ----------------------------------------------------------------------
table(tbl_master$outcome)

table(tbl_master$dead)
fit_dead <- survfit(Surv(days_to_event, dead) ~ continent, data = tbl_master)
fit_dead
ggsurvplot(fit_dead, data = tbl_master, conf.int = TRUE)

table(tbl_master$hospitalized)
fit_hospital <- survfit(Surv(days_to_event, hospitalized) ~ continent, data = tbl_master)
fit_hospital
ggsurvplot(fit_hospital, data = tbl_master, conf.int = TRUE)



