library(BallMapper)
library(Boruta)
library(caret)
library(impute)
library(naniar)
library(survival)
library(survminer)
library(tidyverse)
library(umap)

normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# Load data ---------------------------------------------------------------
tbl_bio <- read_csv(
  "https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/clean_biomarker_first.csv"
)
tbl_bio %>% glimpse()

table(tbl_bio$outcome)


# Clean -------------------------------------------------------------------
time <- as.numeric(tbl_bio$`Discharge time` - tbl_bio$`Admission time`)

# EDA ---------------------------------------------------------------------
tbl_bio$`2019-nCoV nucleic acid detection`
summary(tbl_bio)
str(tbl_bio)
vis_miss(tbl_bio)
gg_miss_upset(tbl_bio)
n_var_miss(tbl_bio)

# biomarker ranges
## LDH
tbl_bio %>%
  ggplot(aes(x = outcome, y = `Lactate dehydrogenase`)) +
  geom_violin(alpha = 0.5, aes(fill = as.factor(outcome))) +
  geom_jitter(alpha = 0.2, aes(color = as.factor(outcome))) +
  geom_abline(slope = 0, intercept = 280, color = "red") +
  geom_abline(slope = 0, intercept = 140, color = "red") +
  theme_classic()

tbl_bio %>%
  ggplot(aes(x = outcome, y = `Hypersensitive c-reactive protein`)) +
  geom_violin(alpha = 0.5, aes(fill = as.factor(outcome))) +
  geom_jitter(alpha = 0.2, aes(color = as.factor(outcome))) +
  geom_abline(slope = 0, intercept = 280, color = "red") +
  geom_abline(slope = 0, intercept = 140, color = "red") +
  theme_classic()



# Train test split --------------------------------------------------------
m <- nrow(tbl_bio)
n <- ncol(tbl_bio)

cols_X <- colnames(tbl_bio)[c(5:6, 8:n)]
#cols_X <- c("age", "(%)lymphocyte", "Hypersensitive c-reactive protein", "Lactate dehydrogenase")
X <- as.data.frame(tbl_bio[cols_X], 
                   row.names = tbl_bio$PATIENT_ID)

# handle missing values
table(is.na(X))
X <- impute.knn(as.matrix(X), k = 5)$data
table(is.na(X))
y <- tbl_bio$outcome
table(is.na(y))

set.seed(42)
idx_train <- sample(x = 1:m, size = 0.8 * m)
X_train <- X[idx_train, ]
y_train <- y[idx_train]
X_test <- X[-idx_train, ]
y_test <- y[-idx_train]
time_train <- time[idx_train]
time_test <- time[-idx_train]

idx_val <- sample(x = 1:length(y_test), size = 0.5 * length(y_test))
y_val <- y_test[idx_val]
y_test <- y_test[-idx_val]
X_val <- X_test[idx_val, ]
X_test <- X_test[-idx_val, ]
time_val <- time_test[idx_val]
time_test <- time_test[-idx_val]

table(y_train)
table(y_test)
table(y_val)

plot(density(time_train))
plot(density(time_test))
plot(density(time_val))

dim(X_train)
dim(X_test)
dim(X_val)


# preprocess 
X_train <- normalize(X_train)
X_test <- normalize(X_test)
X_val <- normalize(X_val)



# Unsupervised learning on train set --------------------------------------
config <- umap.defaults
config$n_neighbors <- 80
config$metric <- "manhattan"
config$n_components <- 2
mapper <- umap(d = X_train, config = config)
embeddings <- mapper$layout
tbl_embeddings <- as_tibble(as.data.frame(embeddings))
tbl_embeddings$y <- y_train
tbl_embeddings$time <- time_train
tbl_embeddings$age <- tbl_bio$age[idx_train]

y_rand <- runif(nrow(tbl_embeddings))

tbl_embeddings %>% 
  ggplot(aes(x = V1, y = V2, color = as.factor(y), size = age)) +
  geom_point() +
  theme_classic()


tbl_embeddings %>% 
  ggplot(aes(x = as.factor(y), y = V1, color = as.factor(y), size = age)) +
  geom_violin() +
  theme_classic()


# Feature selection -------------------------------------------------------
df_surv <- data.frame(time_train, y_train)
df_surv <- cbind(df_surv, X_train)
bdf <- Boruta(Surv(time_train, y_train) ~ ., data = df_surv, doTrace = 2)
print(bdf)
cols_important <- names(bdf$finalDecision)[bdf$finalDecision == "Confirmed"]
cols_important <- gsub(pattern = "`", replacement = "", x = cols_important)
plot(bdf)
df_imp <- data.frame(Biomarker = names(sort(colMeans(bdf$ImpHistory), decreasing = TRUE)),
                     Importance = unname(sort(colMeans(bdf$ImpHistory), decreasing = TRUE)))
colnames(df_imp)
df_imp %>% write_csv("results/boruta_biomarker_importance.csv")
cols_important <- names(sort(colMeans(bdf$ImpHistory), decreasing = TRUE)[1:5])
cols_important <- gsub(pattern = "`", replacement = "", x = cols_important)

#X_save <- X
#X  <- X_save
X <- X[, cols_important]
X_train <- X_train[, cols_important]
X_test <- X_test[, cols_important]
X_val <- X_val[, cols_important]



# BallMapper --------------------------------------------------------------
idx <- complete.cases(X)
values <- as.data.frame(y)
points <- normalize(X)
bm <- BallMapper(points = points, 
                 values = values, 
                 epsilon = 0.35)
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
  

simpleDynamicNetwork(bm)
coloredDynamicNetwork(bm)
colorByAllVariables(bm, values)


# find number of points covered by node number
print(
  points_covered_by_landmarks(bm, c(1, 2))
)

find_dominant_difference_using_averages()

# Validation set ----------------------------------------------------------



