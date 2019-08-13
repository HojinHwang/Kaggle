# Costa Rican Household Poverty Level Prediction : 코스타리카 가계 빈곤 수준 예측
library(tidyverse) # importing, cleaning, visualising
library(xgboost) # gradient boosted machines
library(caret) # ml pipelines made easy
library(VIM) # missing data
library(corrplot) # correlation visualisation
library(ggthemes) # extra plotting themes
library(plotly) # interactive plots 
library(factoextra) # pca visualisation
library(Rtsne) # visualisation of high dimensial datsets
# library(largeVis) # visualisation of high dimensial datsets
library(breakDown) # visualising model results
library(gridExtra) # arranging plots
# library(lightgbm) # leaf-wise gbm
library(glmnet) # regularised regression

options(scipen = 999)

# function for macro F1 score evalution
evalerror <- function(preds, dtrain){
  preds = matrix(preds, ncol = 4, byrow = T)
  preds = apply(preds, 1, which.max) -1
  preds = factor(preds, levels = c(0, 1, 2, 3))
  
  labels <- factor(getinfo(dtrain, "label"), levels = c(0, 1, 2, 3))
  cm = confusionMatrix(data = preds, reference = labels)$table
  
  precision <- diag(cm) / colSums(cm) # diag 함수 = 대각 원소를 나머지 값이 0 인 행렬로 만듦
  recall <- diag(cm) / rowSums(cm)
  
  f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  f1[is.na(fa)] <- 0
  
  err = ifelse(nlevels(labels) == 2, f1[1], mean(f1))
  
  return(list(metric = "f1_score", value = err))
}

# Import data
train <- read_csv("C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\costa rican household\\train.csv")
test <- read_csv("C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\costa rican household\\test.csv")

cat("Train dimensions : ", dim(train))
cat("Test dimensions : ", dim(test))

head(train)

# Categorical data
# Combine data before reversing

full = as_tibble(rbind(train %>% select(-Target), test))

# Create a list of the features names that need to be reverse engineered

feature_list = c(
  "pared", "piso", "techo", "abasta", "sanitario", "energcocinar",
  "elimbasu", "epared", "etecho", "eviv", "estadocivil", "parentesco",
  "instlevel", "tipovivi", "lugar", "area"
)

# Matrix to store our new features
new_features_integer = data.frame(matrix(ncol = length(feature_list), nrow = nrow(full)))


















