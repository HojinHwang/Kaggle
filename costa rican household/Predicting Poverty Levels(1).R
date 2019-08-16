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

# Cycle through and reverse the OHE process for these

ohe_names = vector()

for(i in 1:length(feature_list)){
  # Grab the feature
  feature_to_fix = full %>% select(starts_with(feature_list[i]))
  
  # Fix and enter into our new feature matrix
  new_features_integer[, i] = as.integer(factor(names(feature_to_fix)[max.col(feature_to_fix)], 
                                                ordered = F))
  names(new_features_integer)[i] = paste0(feature_list[i], "_int")
  
  ohe_names = c(ohe_names, as.vector(names(feature_to_fix)))
}

head(new_features_integer)

# Bring it together
full = data.frame(cbind(full, new_features_integer), stringsAsFactors = F)

# Grab OHE features
# OHE = One Hot Encoding

new_features_integer = data.frame(apply(new_features_integer, 2, factor))
ridge_features = model.matrix(~ . -1, new_features_integer[1:nrow(train),]) # One Hot Encoding
ridge_response = ifelse(train$Target == 4, 0, 1)

# Run Ridge
cv.fit = cv.glmnet(x = as.matrix(ridge_features), y = as.factor(ridge_response),
                   nfolds = 5, alpha = 0, family = 'binomial')

plot(cv.fit)

tmp_coeffs = coef(cv.fit, s = "lambda.min")
coefs = data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficients = tmp_coeffs@x)

# Adjust original feature level names

for(i in 1:ncol(new_features_integer)){
  new_features_integer[,i] = paste0(names(new_features_integer)[i], new_features_integer[,i])
}

new_features_ridge <- new_features_integer
new_features_ridge[] <- coefs$coefficients[match(unlist(new_features_integer), coefs$name)]
new_features_ridge

# Zeros can replace the NA since these are just base level

new_features_ridge[is.na(new_features_ridge)] = 0

names(new_features_ridge) = gsub("int", "ridge", names(new_features_ridge))
names(new_features_ridge)

full = data.frame(cbind(full, new_features_ridge), stringsAsFactors = F)
full

# Visualisation
# Filter the data for those features that actually have some missing data
full_missing = full[, sapply(full, anyNA), drop = F]

cat("Missing data found in ", ncol(full_missing) / ncol(full)*100, "% of features")

# Plot missing information for these features
aggr(full_missing, prop = T, numbers = T, cex.axis = 0.8)

missing_feature = apply(full, 1, function(x){sum(is.na(x))})
full = full %>% cbind(missing_feature)

# Select numeric features (removing the categorial features for now and those with high % NA)
full_numerical = full %>% select_if(is.numeric) %>% select(-one_of(ohe_names)) %>%
  select(-v2a1, -v18q1, -rez_esc)

# Since the number of NA is so low for the remaining features I will just replace them with their column mean
replace_na = function(x) replace(x, is.na(x), mean(x, na.rm = T))
full_numerical = apply(full_numerical, 2, replace_na)

# NA 값들을 평균값으로 대체

# Create the matrix
full_numerical_cor = cor(full_numerical, use = "complete.obs")
# ?cor

# Plot the matrix with some clusting
corrplot(full_numerical_cor, order = "hclust", tl.cex = 0.5) # tl.cex = text.label size, hclust = hierarchical clustering order
# ?corrplot

# PCA = Principal Component Analysis

# We have already created a numerical matrix with the categorial information excluded , so we will use this
full_pca = prcomp(full_numerical, center = T, scale. = T)
# ?prcomp = Performs a principal components analysis
full = full %>% cbind(full_pca$x[, 1:3])

# screen plot
fviz_eig(full_pca)
?fviz_eig

# plot first two components
fviz_pca_var(full_pca, col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             select.var = list(contrib = 30), # top 30 contribution
             repel = T # Avoid text overlapping
             )

# Grab training set for visualisation

train_pca = full[1:nrow(train),] %>% select(PC1, PC2, PC3) %>%
  cbind(train$Target) %>% rename(Target = 'train$Target') %>% 
  mutate(Target = as.factor(Target), Target_Binary = ifelse(Target == 4, "Non-vunerable", "Vuneralbe"))

# Plot
plot_ly(train_pca, x = ~PC1, y = ~PC2,  z= ~PC3, color = ~Target) %>%
  add_markers(marker = list(size = 3, opacity = 0.5)) %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3'),
                      title = 'PCA Plot (original target)'))






















































