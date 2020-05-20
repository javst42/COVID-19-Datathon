# MIT COVID-19-Datathon, May 2020, team D002

## Table of Contents
1. [Motivation](#motivation)
2. [Methods](#methods)
  1. [Survival analysis](#survival-analysis)
  2. [Supervised learning](#supervised-learning)
  3. [Topological data analysis](#topological-data-analysis)
3. [Future work](#future-work)
4. [Data used](#data-used)


# Motivation
Our goal was to create a universal COVID-19 risk index for clinicians to guide patient care and allocate resources. We have shown that the severity of patient outcomes can be predicted to varying extends based on symptoms and blood biomarkers. We are continuing to improve on our work by using more datasets as they comes out.

![approach](https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/figures/approach.png)

# Methods

To best inform patients and healthcare workers we've taken a multifaceted approach

## Survival analysis
So far, we've used survival analysis for EDA by estimating the survival function across populations with Kaplan-Meier plots. We are investigating the use of patient-level analyses with techniques that can use symptom/biomarker data like the [Cox proportional hazards model](https://en.wikipedia.org/wiki/Proportional_hazards_model#The_Cox_model).


## Supervised learning

### Symptom data
Owing to the discrete, non-linear nature of symptom data, we chose to use tree-based models to handle the discrete nonlinearity of symptom data. We used the tree-based algorithm LightGBM to learn from patient symptoms and predict their duration of hospitalization. GBM was trained on recovered and deceased patients to predict the probability of patient death. We wanted our pipeline to explain which symptoms were most important, so our models were evaluated using [Shapley values](https://christophm.github.io/interpretable-ml-book/shapley.html).

## Topological data analysis

### Biomarker data
We found a dataset containing hundreds of severely ill COVID-19 patients. We used [Boruta](https://www.datacamp.com/community/tutorials/feature-selection-R-boruta) to determine which biomarkers are important for survival. Using those important biomarkers, we used a topological data analysis technique, BallMapper, to cluster these patients. Once clustered, the patient clusters were colored by their survival rates. The result is an unsupervised model for assesssing the risks of new patients. We have also been experimenting with UMAP for dimensionally reducing and clustering new data points onto our original data.


BallMapper             |  Supervised UMAP generalizes to new data
:-------------------------:|:-------------------------:
![ballmapper](https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/figures/ballmapper_biomarkers.png)  |  ![umap](https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/figures/nature_biomarkers_test_80_neighbors_manhattan.png)




# Future work
Data available during the datathon were limited and overrepresented patient populations from locations early on in the outbreak. The biomarker data, especially, is from a non-representative COVID-19 cohort, where the death rate was ~50%. We seek to develop this project by adding more data, finding better ways to use the data we already have (handling survival censorship), and testing our models on separate datasets. We also hope to identify potential confounding factors like country, so that we can make our pipeline more robust.

# Data used
## Symptom data
Data comes from nCoV2019 and was cleaned for survival analysis and ML.
https://github.com/beoutbreakprepared/nCoV2019/tree/master/latest_data


Link to processed data: https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/survival_master_dummy.csv

## Biomarker data
<p>
Original: https://www.nature.com/articles/s42256-020-0180-7#Sec10

### Train data
<br>
Easy access: https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/time_series_375_prerpocess_en.csv
</p>
<p>
There are multiple ways to group this data. I think using the first value is the most useful because it allows for more time to prepare. A test the day prior to discharge isn't very useful.
</p>
<p>
Mean aggregated: https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/clean_biomarker_mean.csv
</p>
<p>
First value aggregated: https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/clean_biomarker_first.csv
</p>
<p>
Last value aggregated: https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/clean_biomarker_last.csv
</p>

### Test data

<br>
Easy access: https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/time_series_test_110_preprocess_en.csv
</p>
<p>
Mean aggregated: https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/clean_biomarker_test_mean.csv
</p>
<p>
First value aggregated: https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/clean_biomarker_test_first.csv
</p>
<p>
Last value aggregated: https://brunods10.s3-us-west-2.amazonaws.com/MIT_COVID/clean_biomarker_test_last.csv
</p>