# porto_analysis_practice(2) : second practice of porto dataset

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(verification))
library(repr)

# generate fake insurance data
set.seed(12)
insurance <- data.frame(
  claim = c(rep(1,20), rep(0,20)),
  age = c(rnorm(20, mean = 25, sd = 5), rnorm(20, mean = 37, sd = 7))
)

# plot the insurance plot
ggplot(data = insurance, aes(x = age, y = claim)) +
  geom_point(color = "grey50") +
  theme_bw()

# independenct of age and claim
# regress claim on age
lmmod <- lm(claim ~ age, data = insurance)

# plot the data and regression line
ggplot(data = insurance, aes(x = age, y = claim)) +
  geom_point(color = "grey50") +
  geom_abline(intercept = coef(lmmod)[1], slope = coef(lmmod)[2], color = "red") +
  theme_bw()

sigmoid <- function(x){
  1 / (1 + exp(-x))
}

data.frame(x = seq(-5, 5, 0.5), y = sigmoid(seq(-5, 5, 0.5))) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(color = 'red', size = 0.3) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  labs(y = "sigmoid(x)", title = "The sigmoid function") +
  theme_bw()

# estimate the logistic regression model
logmod <- glm(claim ~ age, data = insurance, family = binomial(link = 'logit')) # 로지스틱 회귀모형 구축

# make predictions on the training data
insurance$preds <- predict(logmod, type = 'response') # type = 'response' 는 로지스틱 회귀분석방법으로 예측

# plot results
ggplot(data = insurance, aes(x = age, y = claim)) +
  geom_point(color = 'grey50') +
  geom_line(aes(x = age, y = preds), color = 'red', size = 0.3) +
  theme_bw()

dtrain <- read_csv("C:/Users/HOJIN/Desktop/Kaggle Data/porto/train.csv") # 결측치는 -1로 표시돼있음

# Set missing values to NA
dtrain[dtrain == -1] <- NA

# collect the categorial variable names
cat_vars <- names(dtrain)[grepl('_cat$', names(dtrain))]

# convert categorial features to factors
dtrain <- dtrain %>% mutate_at(.vars = cat_vars, .funs = as.factor)

# One hot encode the factor variables
dtrain <- model.matrix(~ . -1, data = dtrain)

# set seed for reproducibility
set.seed(123)

# making a train index
train_index <- sample(c(TRUE, FALSE), replace = T, size = nrow(dtrain), prob = c(0.2, 0.8))

# split the data according to the train index
training <- as.data.frame(dtrain[train_index,])
testing <- as.data.frame(dtrain[!train_index,])

# find any linear combos in features
lin_comb <- findLinearCombos(training)

# take set difference of feature names and linear combos
d <- setdiff(seq(1:ncol(training)), lin_comb$remove) # 제거해야할 위치의 백터를 보여줌 , setdiff 를 사용함으로써 선형 종속성 제거

# remove linear combo columns
training <- training[,d]
training <- training[, setdiff(names(training), 'ps_ind_02_cat4')] # ps_ind_02_cat4 변수 제거

# estimate logistic regression model on training data
logmod <- glm(target ~ . -id, data = training, family = binomial(link = 'logit'))

# make predictions on the test set
preds <- predict(logmod, newdata = testing, type = 'response')

# plot histogram of predictios
data.frame(preds = preds) %>% ggplot(aes(x = preds)) +
  geom_histogram(bins = 50, fill = 'grey50') +
  labs(title = 'Histogram of Predictions') +
  theme_bw()

# print range of predictions
print(round(range(preds), 2))

# print median of predictions
print(median(preds))

roc_data <- data.frame(
  p0.3 = ifelse(preds > 0.3, 1, 0),
  p0.2 = ifelse(preds > 0.2, 1, 0),
  p0.1 = ifelse(preds > 0.1, 1, 0),
  p0.05 = ifelse(preds > 0.05, 1, 0),
  p0.04 = ifelse(preds > 0.04, 1, 0),
  p0.03 = ifelse(preds > 0.03, 1, 0),
  p0.02 = ifelse(preds > 0.02, 1, 0),
  p0.01 = ifelse(preds > 0.01, 1, 0)
)

# true positive (hit) rate
tpr <- function(pred, actual){
  res <- data.frame(pred, actual)
  sum(res$actual == 1 & res$pred == 1) / sum(actual == 1)
}

# false positive rate
fpr <- function(pred, actual){
  res <- data.frame(pred, actual)
  sum(res$actual == 0 & res$pred == 1) / sum(actual == 0)
}

# get actual values from testing data
actual <- testing$target

# reshape to long format and get fpr and tpr for each threshold
roc_data <- roc_data %>%
  gather(key = 'threshold', value = 'pred') %>%
  group_by(threshold) %>%
  summarize(tpr = tpr(pred, actual = actual),
            fpr = fpr(pred, actual = actual))

# set x and y tick marks
breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)

# get label for plotting break points
labels <- substr(roc_data$threshold, start = 2, stop = 5)

# plot the ROC curve
ggplot(data = roc_data, aes(x = fpr, y = tpr)) +
  geom_line() + geom_text(aes(label = labels), nudge_x = 0.05) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  scale_x_continuous(limits = c(0,1), breaks = breaks) +
  scale_y_continuous(limits = c(0,1), breaks = breaks) +
  labs(x = "False positive rate", y = "True positive rate", title = "ROC curve") +
  theme_bw()

# verification packes , roc.plot()
roc.plot(testing$target, preds, threshold = seq(0, max(preds), 0.01),
         plot.thres = c(0.03, 0.05, 0.1))




















