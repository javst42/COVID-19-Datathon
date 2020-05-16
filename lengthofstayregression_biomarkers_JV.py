# -*- coding: utf-8 -*-
"""lengthofstayregression-biomarkers.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1TTE6Tj033P4buWT0nBx_Gh7g_VsRi5IR
"""

#imports
#basics
import pandas as pd
import numpy as np
import random
from math import sqrt
from scipy.stats import gaussian_kde

#sklearn tools
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import OneHotEncoder
from sklearn.model_selection import train_test_split
from sklearn import metrics
from sklearn.manifold import TSNE
from sklearn.impute import KNNImputer
from sklearn.model_selection import StratifiedKFold
#classifiers
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.svm import LinearSVC
from sklearn.linear_model import SGDClassifier
#regressions
import xgboost as xgb
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LinearRegression
from sklearn.linear_model import SGDRegressor
from sklearn.ensemble import GradientBoostingRegressor
import lightgbm as lgb
#plotting
import matplotlib.pyplot as plt
import seaborn as sns

#set options
pd.options.display.max_columns = None
pd.options.display.max_rows = None

#read in data, take a look
#df = pd.read_csv('data/survival_master_dummy.csv')
df = pd.read_csv('https://github.com/javst42/COVID-19-Datathon/raw/master/data/clean_biomarker_first.csv')

#print(df.head(6))
#print(df.shape)
#print(df.isnull().sum())
print(list(df))
#df.info()

#make sure all dates are correct dtype
dates = ['Admission time', 'Discharge time', 'RE_DATE']
for date in dates:
    df[date] = pd.to_datetime(df[date])

#make column encoding length of stay
df['length_of_stay'] = df['Discharge time'] - df['Admission time']
df['length_of_stay'] =  df['length_of_stay'] / pd.Timedelta(1, unit='d')
#print(df['length_of_stay'].head(10))

#print(df['gender'].value_counts())

#dummify gender
def gender_ohe(df):
    # add dummies
    one_hot = pd.get_dummies(df['gender'])
    # drop column as it is now encoded
    df = df.drop('gender', axis = 1)
    # join the encoded df
    df = df.join(one_hot)
    df.rename(columns={1: 'sex_male', 2: 'sex_female'}, inplace = True)
    return df

df = gender_ohe(df)

#dummify outcome
#print(df['outcome'].value_counts())
def outcome_ohe(df):
    # add dummies
    one_hot = pd.get_dummies(df['outcome'])
    # drop column as it is now encoded
    df = df.drop('outcome', axis = 1)
    # join the encoded df
    df = df.join(one_hot)
    df.rename(columns={1: 'outcome_died', 0: 'outcome_discharged'}, inplace = True)
    return df
  
df = outcome_ohe(df)

#make lists of columns split into features (categorical and numerical); index or id; target variable
id_col = ['PATIENT_ID'] #patient id - not a feature
target_col = ['length_of_stay'] #target variable - not a feature

numer_cols = ['age','Hypersensitive cardiac troponinI', 'hemoglobin', 'Serum chloride', 'Prothrombin time', 'procalcitonin', 
              'eosinophils(%)', 'Interleukin 2 receptor', 'Alkaline phosphatase', 'albumin', 'basophil(%)', 'Interleukin 10', 
              'Total bilirubin', 'Platelet count', 'monocytes(%)', 'antithrombin', 'Interleukin 8', 'indirect bilirubin', 
              'Red blood cell distribution width', 'neutrophils(%)', 'total protein', 'Quantification of Treponema pallidum antibodies', 
              'Prothrombin activity', 'HBsAg', 'mean corpuscular volume', 'hematocrit', 'White blood cell count', 'Tumor necrosis factorα', 
              'mean corpuscular hemoglobin concentration', 'fibrinogen', 'Interleukin 1β', 'Urea', 'lymphocyte count', 'PH value', 
              'Red blood cell count', 'Eosinophil count', 'Corrected calcium', 'Serum potassium', 'glucose', 'neutrophils count', 
              'Direct bilirubin', 'Mean platelet volume', 'ferritin', 'RBC distribution width SD', 'Thrombin time', '(%)lymphocyte', 
              'HCV antibody quantification', 'D-D dimer', 'Total cholesterol', 'aspartate aminotransferase', 'Uric acid', 'HCO3-', 
              'calcium', 'Amino-terminal brain natriuretic peptide precursor(NT-proBNP)', 'Lactate dehydrogenase', 
              'platelet large cell ratio', 'Interleukin 6', 'Fibrin degradation products', 'monocytes count', 'PLT distribution width', 
              'globulin', 'γ-glutamyl transpeptidase', 'International standard ratio', 'basophil count(#)', 
              '2019-nCoV nucleic acid detection', 'mean corpuscular hemoglobin', 'Activation of partial thromboplastin time', 
              'Hypersensitive c-reactive protein', 'HIV antibody quantification', 'serum sodium', 'thrombocytocrit', 'ESR', 
              'glutamic-pyruvic transaminase', 'eGFR', 'creatinine']
categ_cols = ['sex_male', 'sex_female', 'outcome_died', 'outcome_discharged'] #drop country, continent since we have so few samples

feature_cols = numer_cols + categ_cols
print(feature_cols)

# #make dataframe copy for feature selection
# df_copy = df.copy()

# #we have to impute missing data to use tsne (but don't want to impute for the full analysis using LightGBM; this imputation is only 
# #to enable feature selection)

# imputer = KNNImputer(n_neighbors=3, weights="uniform")
# df_copy = imputer.fit_transform(df_copy[numer_cols])


# #using tsne for feature selection
# df_copy_x=df_copy[]
# df_copy_y=df_copy[target_col]

# X_tsne = df_copy_x
# y_tsne = df_copy_y
# X_tsne_fitted = TSNE(perplexity = 20, early_exaggeration = 12, learning_rate = 500, n_iter = 500).fit_transform(X_tsne)

#scale numerical data using StandardScaler
scaler = StandardScaler()
#scale and fit on train:
for i in range(len(numer_cols)):
    train[numer_cols[i]] = scaler.fit_transform(train[numer_cols[i]].values.reshape(-1, 1))
#scale with fitted on test:
for i in range(len(numer_cols)):    
    test[numer_cols[i]] = scaler.transform(test[numer_cols[i]].values.reshape(-1,1))


#set random.seed() for reproducibility
np.random.seed(1234)
r_seed = 3

# choose classifier/regressor, and then fit model
#estimators
num_est = 1000


#split into test/train sets
train = df_copy.sample(frac = 0.8, random_state=0)
test = df_copy.drop(train.index)

#add flags for testing and training data
train['from_set']='train' 
test['from_set']='test'

#recombine testing and training data
test_train = pd.concat([train,test],axis=0) 

#split training set into training/validating
train['train_validate_filter'] = np.random.uniform(0, 1, len(train)) <= .80
train_train, train_validate = train[train['train_validate_filter']==True], train[train['train_validate_filter']==False]

#define inputs, outputs for training, validation, testing sets
in_train = train_train[feature_cols].values
out_train = train_train[target_col].values
out_train = out_train.ravel()


in_validate = train_validate[feature_cols].values
out_validate = train_validate[target_col].values

in_test = test[feature_cols].values
out_test = test[target_col].values

target_max = float(df[target_col].max())
target_min = float(df[target_col].min()) 
print(target_min, target_max)

#LightGBM
lgbr = lgb.LGBMRegressor(max_depth = -1, n_estimators = 1000, silent = False)

#XGBoost
import xgboost as xgb
xgbr = xgb.XGBRegressor(seed = r_seed)


#run model
model = xgbr
model.fit(in_train, out_train)
out_validate_pred = model.predict(in_validate)
out_test_pred = model.predict(in_test)

from sklearn.metrics import mean_squared_error
from math import sqrt

def rmse(y_actual, y_predicted):
   return sqrt(mean_squared_error(y_actual, y_predicted))

print(model)
print('RMSE, validation set: ', rmse(out_validate, out_validate_pred))
print('RMSE, testing set: ', rmse(out_test_pred, out_test))
print('range for length of stay from {0} to {1}'.format(target_min, target_max))
#print('raw accuracy:', metrics.accuracy_score(out_validate, out_validate_pred))
#print('log-loss metric', metrics.log_loss(out_validate, out_validate_pred))

# results = test['customer_id']
# out = pd.DataFrame({'pr_y':out_test_pred})

# results = pd.concat([results, out], axis=1)

# sns.set()
# sns.lineplot(out_validate.ravel(), out_validate_pred.ravel())
# sns.lineplot(out_test.ravel(), out_test_pred.ravel())

#print(xgbr.feature_importances_)

# plot feature importance
feature_imp = pd.DataFrame(feature_cols)
feature_imp['importance']=xgbr.feature_importances_
feature_imp.columns=['feature', 'importance']

desc_feature_imp = feature_imp.sort_values('importance', ascending=True)
print(feature_imp.head(5))


fig = plt.figure()
ax = fig.add_axes([0,0,1,1])
langs = ['C', 'C++', 'Java', 'Python', 'PHP']
students = [23,17,35,29,12]
ax.barh(desc_feature_imp['feature'].head(10), desc_feature_imp['importance'].head(10))
plt.show()

# xgb.plot_importance(xgbr)
# plt.show()