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





















































