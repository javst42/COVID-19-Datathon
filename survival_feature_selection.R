library(Boruta)
library(survival)
library(survminer)
library(tidyverse)




# Load data ---------------------------------------------------------------
tbl_master <- read_csv("https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/survival_master_dummy.csv")

idx_keep <- !(tbl_master$days_to_event < 0)
fit_dead <- survfit(Surv(days_to_event, dead) ~ continent, data = tbl_master)
fit_dead
ggsurvplot(fit_dead, data = tbl_master, conf.int = TRUE)


fit_hospital <- survfit(Surv(days_to_event, hospitalized) ~ continent, data = tbl_master)
fit_hospital
ggsurvplot(fit_hospital, data = tbl_master, conf.int = TRUE)



# Boruta ------------------------------------------------------------------
set.seed(42)
tbl_boruta <- tbl_master[, c(2, 3, 11:35)] %>% select(-dead)
tbl_boruta$age <- as.numeric(substr(tbl_boruta$age, 1, 2))
tbl_boruta <-  tbl_boruta[complete.cases(tbl_boruta), ]
boruta_hospitalization <- Boruta(Surv(days_to_event, hospitalized) ~ ., data = tbl_boruta, doTrace = 2)
print(boruta_hospitalization)
plot(boruta_hospitalization)
