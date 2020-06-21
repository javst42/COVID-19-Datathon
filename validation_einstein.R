library(BallMapper)
library(ComplexHeatmap)
library(impute)
library(randomForestSRC)
library(tidyverse)
library(umap)


# Load data ---------------------------------------------------------------
tbl_einstein <- read_csv(
  "https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/covid_einstein.csv"
)
tbl_bio <- read_csv(
  "https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/clean_biomarker_last.csv"
)
tbl_bio_test <- read_csv(
  "https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/clean_biomarker_test_last.csv"
)

colnames(tbl_einstein)
cols_keep_einstein <- c(colnames(tbl_einstein)[1:6], "Lymphocytes", "Proteina C reativa mg/dL", "Lactic Dehydrogenase")
colnames(tbl_bio)
cols_keep_bio <- c("PATIENT_ID", "outcome", "(%)lymphocyte", "Hypersensitive c-reactive protein", "Lactate dehydrogenase")
colnames(tbl_bio_test)
# Train and target
X_bio <- tbl_bio[, cols_keep_bio] %>% drop_na()
y_bio <- X_bio$outcome
X_bio <- X_bio[, c(-1, -2)]
X_bio <- as.data.frame(scale(X_bio))

X_bio_test <- tbl_bio_test[, cols_keep_bio] %>% drop_na()
y_bio_test <- X_bio_test$outcome
X_bio_test <- X_bio_test[, c(-1, -2)]
X_bio_test <- as.data.frame(scale(X_bio_test))



# Train and target for testing
vis_miss(tbl_einstein[, cols_keep_einstein])
X_einstein <- tbl_einstein[, cols_keep_einstein] %>% drop_na()
y_einstein <- X_einstein$`SARS-Cov-2 exam result`
X_einstein <- X_einstein %>% select(all_of(cols_keep_einstein[7:9]))
colnames(X_bio)
colnames(X_einstein) <- colnames(X_bio)

tbl_bio[, cols_keep_bio]
dat_umap <- cbind.data.frame(y_bio, X_bio)
dat_umap_test <- cbind.data.frame(y_bio_test, X_bio_test)

dat_umap %>% write_csv("data/dat_bio_umap.csv")
dat_umap_test %>% write_csv("data/dat_bio_test_umap.csv")

# umap --------------------------------------------------------------------
dat_umap <- rbind(X_bio, X_einstein)
y_umap <- c(ifelse(y_bio == 1, "died", "survived"), 
            ifelse(y_einstein == "positive", "covid positive", "covid negative"))
config <- umap.defaults
config$n_neighbors <- 80
config$metric <- "euclidean"
mapper <- umap(d = dat_umap, config = config)
embeddings <- mapper$layout
tbl_emb <- as_tibble(embeddings)
tbl_emb$y <- y_umap

cbp1 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
tbl_emb %>%
  ggplot(aes(x = V1, y = V2, color = y)) +
  geom_point() +
  theme_classic() +
  scale_color_manual(values = cbp1)
