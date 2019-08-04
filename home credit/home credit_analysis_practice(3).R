library(tidyverse)
library(xgboost)
library(magrittr)
set.seed(0)

# read_csv 는 티블형태로 불러오고, read.csv 는 df 형태로 불러온다
tr <- read_csv("C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\home credit\\home-credit-default-risk\\application_train.csv")
te <- read_csv("C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\home credit\\home-credit-default-risk\\application_test.csv")

bureau <- read_csv("C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\home credit\\home-credit-default-risk\\bureau.csv") %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer()))

cred_card_bal <- read_csv("C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\home credit\\home-credit-default-risk\\credit_card_balance.csv") %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer()))

pos_cash_bal <- read_csv("C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\home credit\\home-credit-default-risk\\POS_CASH_balance.csv") %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer()))

prev <- read_csv("C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\home credit\\home-credit-default-risk\\previous_application.csv") %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer()))

avg_bureau <- bureau %>% group_by(SK_ID_CURR) %>% summarise_all(funs(mean), na.rm = T) %>%
  mutate(buro_count = bureau %>% group_by(SK_ID_CURR) %>% count() %$% n)
head(avg_bureau, 10)
# ?magrittr ; # %$% = exposition pipe-operator (파이프 조작에 대한 설명 ?)

avg_cred_card_bal <- cred_card_bal %>% group_by(SK_ID_CURR) %>% summarise_all(funs(mean), na.rm = T) %>%
  mutate(card_count = cred_card_bal %>% group_by(SK_ID_CURR) %>% count() %$% n)

avg_pos_cash_bal <- pos_cash_bal %>% group_by(SK_ID_CURR) %>% summarise_all(funs(mean), na.rm = T) %>%
  mutate(pos_count = pos_cash_bal %>% group_by(SK_ID_PREV, SK_ID_CURR) %>%
           group_by(SK_ID_CURR) %>% count() %$% n)

avg_prev <- prev %>% group_by(SK_ID_CURR) %>% summarise_all(funs(mean), na.rm = T) %>%
  mutate(nb_app = prev %>% group_by(SK_ID_CURR) %>% count() %$% n)

tri <- 1:nrow(tr)
y <- tr$TARGET

tr_te <- tr %>% select(-TARGET) %>% bind_rows(te) %>%
  left_join(avg_bureau, by = "SK_ID_CURR") %>% 
  left_join(avg_cred_card_bal, by = "SK_ID_CURR") %>%
  left_join(avg_pos_cash_bal, by = "SK_ID_CURR") %>%
  left_join(avg_prev, by = "SK_ID_CURR") %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer())) %>%
  data.matrix()

rm(tr, te, prev, avg_prev, bureau, avg_bureau, cred_card_bal, avg_cred_card_bal, pos_cash_bal, avg_pos_cash_bal) ; gc()

dtest <- xgb.DMatrix(data = tr_te[-tri, ])
tr_te <- tr_te[tri, ]
tri <- caret::createDataPartition(y, p = 0.9, list = F) %>% c()
dtrain <- xgb.DMatrix(data = tr_te[tri, ], label = y[tri])
dval <- xgb.DMatrix(data = tr_te[-tri, ], label = y[-tri])
cols <- colnames(tr_te)

rm(tr_te, y, tri) ; gc()

p <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "auc",
          nthread = 8,
          eta = 0.025,
          max_depth = 6,
          min_child_weight = 19,
          gamma = 0,
          subsample = 0.8,
          colsample_bytree = 0.632,
          alpha = 0,
          lambda = 0.05,
          nrounds = 2000)

m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 200)

xgb.importance(cols, model = m_xgb) %>% xgb.plot.importance(top_n = 30)



















