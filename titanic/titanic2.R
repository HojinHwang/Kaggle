###### 타이타닉 분석 2번째 ###
library(readr)
library(stringr) # 문자열 처리 패키지
library(doBy)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(corrplot)
library(doBy)
library(dplyr) # 전처리
library(randomForest)
library(gridExtra)
setwd("C:\\Users\\HOJIN\\Desktop\\kaggle")
train <- readr::read_csv('C:/Users/HOJIN/Desktop/kaggle/train.csv')
test <- readr::read_csv('C:/Users/HOJIN/Desktop/kaggle/test.csv')
full <- dplyr::bind_rows(train, test) # test의 survived 변수를 NA로 처리
full <- full %>% mutate(Survived = factor(Survived),
                        Pclass = factor(Pclass),
                        Name = factor(Name),
                        Sex = factor(Sex),
                        Embarked = factor(Embarked))
str(full)
head(full)
summary(full)
sapply(train, function(x) length(unique(x))) # 벡터나 행렬로 반환하는 sapply, 중복되지 않는 고유 값이 얼마인지 알아보자.
colSums(is.na(full))
missing_values <- full %>% dplyr::summarize_all(funs(sum(is.na(.))/n()))
missing_values <- tidyr::gather(missing_values, key = "feature", value = "missing_pct")
missing_values # 각 특성에 대한 결측값 비율

missing_values %>% 
  ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) +
  geom_bar(stat = "identity", fill = "red") + # bar plot 그리기 stat = 'identity' 데이터프레임 값을 그대로 이용하여 그리라는 옵션
  ggtitle("Rate of missing values in each features") + theme(plot.title = element_text(face = "bold",
                                                                                       hjust = 0.5,
                                                                                       size = 15, color = "darkblue")) +
  labs(x = "Feature names", y = "Rate") + coord_flip()

# 결측값이 있는 변수로만 시각화
missing_values <- missing_values[missing_values$missing_pct > 0,]
missing_values %>% ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) +
  geom_bar(stat = "identity", fill = "blue") + ggtitle("Rate of missing values in each features") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + 
  labs(x = "Feature names", y = "Rate") + coord_flip() # Plot 의 x, y 축 반환

# EDA
table(full$Sex) # 남성이 여성보다 많다.
full %>% group_by(Survived, Sex) %>% summarise(freq = n()) # 빈도수로 표현 , 남성이 여성보다 많이 탔지만 생존율은 여성보다 낮다.
prop.table(table(full$Sex, full$Survived), 1) # 여자들이 생존할 확률이 높음 , prop.table 은 전체 도수에 대한 비율을 나타냄

# 성별 막대그래프
sex.p1 <- full %>% dplyr::group_by(Sex) %>% summarize(N = n()) %>% ggplot(aes(Sex, N)) + geom_col() +
  geom_text(aes(label = N), size = 5, vjust = 1.2, color = "#FFFFFF") + ggtitle("Bar plot of Sex") + 
  labs(x = "Sex", y = "Count")

sex.p2 <- full %>% filter(!is.na(Survived)) %>% ggplot(aes(factor(Sex), fill = factor(Survived))) +
  geom_bar(position = "fill") + scale_y_continuous(labels = percent) + scale_fill_brewer(palette = "Set1") +
  # palette에 어떤색 넣을지 지정
  # 일정한 간격으로 x축과 y축 설정 : scale_x_continuous(breaks=seq())
  # 분석가 마음대로 x축과 y축 설정 : scale_x_continuous(breaks=c())
  ggtitle("Survivla Rate of Sex") + labs(x = "Sex", y = "Rate")

grid.arrange(sex.p1, sex.p2, ncol = 2)

# 선실 등급에 따른 생존확률
table(full$Pclass)
prop.table(table(full$Pclass, full$Survived), 1)

pclass.p1 <- full %>% 
  dplyr::group_by(Pclass) %>% 
  summarize(N = n()) %>% 
  ggplot(aes(Pclass, N)) +
  geom_col() +
  geom_text(aes(label = N), size = 5, vjust = 1.2, color = "#FFFFFF") + 
  ggtitle("Bar plot of Pclass") +
  labs(x = "Pclass", y = "Count")

pclass.p2 <- full%>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "fill") + 
  scale_fill_brewer(palette = "Set1") +  
  ggtitle("Survival Rate by Pclass") + 
  labs(x = "Pclass", y = "Rate")

grid.arrange(pclass.p1,pclass.p2,ncol=2)

hist(full$Fare)
Fare.p1 <- full %>% ggplot(aes(Fare)) + geom_histogram(col = "yellow", fill = "blue", alpha = .5) +
  ggtitle("Histogram of passengers Fare") + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))

# 생존여부에 따른 fare box plot
Fare.p2 <- full %>% filter(!is.na(Survived)) %>% ggplot(aes(Survived, Fare)) + # x축에 생존 y에 fare
 geom_jitter(col = "gray") + geom_boxplot(alpha = 0.5) + ggtitle("Boxplot of passengers Fare") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))
  
grid.arrange(Fare.p1, Fare.p2, ncol = 2)

hist(full$Age)
#나이분포 히스토그램
Age.p1 <- full %>%  ggplot(aes(Age)) + 
  geom_histogram(breaks = seq(0, 80, 1),
                 col = "red" ,
                 fill = "green" ,
                 alpha = .5) +
  ggtitle("All titanic passengers age histogram") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

Age.p2 <- full %>% filter(!is.na(Survived)) %>% ggplot(aes(Age, fill = Survived)) +
  geom_density(alpha = .5) + # 밀도그래프니까 plot으로 축 지정하고 geom_density
  ggtitle("Titanic passengers age density plot") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

grid.arrange(Age.p1, Age.p2, ncol = 2)

table(full$SibSp)
train %>% group_by(Survived, SibSp) %>% summarise(freq = n())
prop.table(table(train$SibSp, train$Survived),1) # 배우자, 형제자매가 많을수록 생존률 떨어짐

table(train$Parch)
train %>% group_by(Survived, Parch) %>% summarise(freq = n())
prop.table(table(train$Parch, train$Survived), 1) # 뒤에 1은 각 행에 대해 계산한 후 비율 구함.

table(full$Embarked) # 결측값 두개
train %>% group_by(Survived, Embarked) %>% summarise(freq = n()) # 결측값 2개 존재
prop.table(table(train$Embarked, train$Survived), 1)

# 결측치 처리
# Cabin 변수는 결측치가 너무 많아 제거하고 차후 파생변수 Deck 을 생성한다.

colSums(is.na(full)) # 결측치 수 확인
full[is.na(full$Embarked),] # 두개의 관측치 모두 Fare가 80이고 Pclass가 1임
embark_fare <- full[!is.na(full$Embarked),]
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() + geom_hline(aes(yintercept=80), # fare가 80에 라인생성
                              colour = 'red', linetype = 'dashed', lwd = 2) +
  scale_y_continuous()

# Fare가 80이고 Pclass가 1인 Embarked는 대부분 C이다
full$Embarked[c(62, 830)] <- 'C'
full[c(62, 830),]

# Name 변수에서 성별과 관련된 이름만을 추출하고 범주화
Title <- full$Name
Title <- gsub("^.* (.*?)\\..*$", "\\1", Title)
full$Title <- Title
unique(full$Title)

# 범주별 빈도수, 비율 확인
descr::CrossTable(full$Title) # 범주가 너무 많다 줄여주자.

full <- full %>% 
  # "%in%" 대신 "=="을 사용하게되면 Recyling Rule 때문에 원하는대로 되지 않습니다.
  mutate(Title = ifelse(Title %in% c("Mlle", "Ms", "Lady", "Dona"), "Miss", Title),
         Title = ifelse(Title == "Mme", "Mrs", Title),
         Title = ifelse(Title %in% c("Capt", "Col", "Major", "Dr", "Rev", "Don", "Sir", "Countess", "Jonkheer"), "Officer", 
                        Title),
         Title = factor(Title))

descr::CrossTable(full$Title)

# 성별을 더미화
full$Sex <- ifelse(full$Sex == "male", 0, 1)
full$Sex <- as.factor(full$Sex)

# Fszie (Family Size) 생성
full$Fsize <- full$SibSp + full$Parch + 1 # 1은 자신도 포함
table(full$Fsize)

# Fsize 에 대한 생존률 시각화
Fsize.p1 <- full %>% filter(!is.na(Survived)) %>%
  ggplot(aes(Fsize, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) + # 일정한 간격으로 x축, y축 설정
  scale_x_continuous(breaks = c(1:11)) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Survival Rate of Fsize") + 
  labs(x = "Fsize", y = "Rate")

Fsize.p1

# 범주화
full$Familysize[full$Fsize == 1] <- 'single'
full$Familysize[full$Fsize >=2 & full$Fsize < 5] <- 'small'
full$Familysize[full$Fsize >= 5] <- 'large'
full$Familysize <- as.factor(full$Familysize)
table(full$Familysize)

# Familysize 에 따른 생존율 시각화
ggplot(full[1:891,], aes(x = Familysize, fill = Survived)) +
  geom_bar(position = "fill") + ggtitle("Survival of Familysize") + labs(x = "Familysize", y = "Rate")

full$Cabin[1:28]
strsplit(full$Cabin[2], NULL)[[1]] # 한 단위로 쪼개네 ?

full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1])) # 1 자리로 쪼개서 맨 앞의 글자 따오네
full$Deck <- as.character(full$Deck)
str(full)
full = full[, -11] # Cabin 변수 제거
head(full)

full$Deck[is.na(full$Deck)] <- "U"
cabin = full %>% filter(!is.na(Survived) & Deck != "U")
ggplot(cabin, aes(x = Deck, fill = factor(Survived), na.rm = T)) +
  geom_bar(stat = "count") + facet_grid(.~Pclass) + labs(title = "Survival split by Pclass and Deck")

# ifelse 문 안에 ifelse 쓰기
full <- full %>% mutate(Deck = ifelse(Pclass == 1 & Deck == "U", "X",
                                      ifelse(Pclass == 2 & Deck == "U", "Y",
                                             ifelse(Pclass == 3 & Deck == "U", "Z", Deck))))

full %>% count(Deck)
age.sex <- full %>% ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = .5) +
  ggtitle("Titanic Passengers Age density plot") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

age.sex # 성별에 따른 나이의 밀도 확인

age.pclass <- full %>% ggplot(aes(Age, fill = Pclass)) +
  geom_density(alpha = .5) +
  ggtitle("Titanic Passengers Age density plot") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

age.pclass

age.title <- full %>% ggplot(aes(Age, fill = Title)) +
  geom_density(alpha = .5) +
  ggtitle("Titanic Passengers Age density plot") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

age.title

plot(full$Title)

full <- as.data.frame(full)
summaryBy(Age ~ Title, data = full, FUN = c(mean, sd, median), na.rm = T) ## ddply로

# 결측값을 중앙값으로 대체
full$Age <- ifelse((is.na(full$Age) & full$Title == 'Master'), 4, full$Age)
full$Age <- ifelse((is.na(full$Age) & full$Title == 'Miss'), 22, full$Age)
full$Age <- ifelse((is.na(full$Age) & full$Title == 'Mr'), 29, full$Age)
full$Age <- ifelse((is.na(full$Age) & full$Title == 'Mrs'), 35, full$Age)
full$Age <- ifelse((is.na(full$Age) & full$Title == 'Officer'), 48, full$Age)

hist(full$Age, freq = F, main = 'Age', col = 'lightgreen', ylim = c(0, 0.05)) # freq = F 이면 밀도로 나타내기.

# child : 18세 이하, adult : 19세이상 64세 이하, senior : 65세 이상
full$Age <- ifelse(full$Age <= 18, "child", 
                   ifelse(full$Age >= 19 & full$Age <= 64, 'adult', 'senior'))

# Ticket 변수를 이용한 GroupSize 파생
length(unique(full$Ticket))
head(full$Ticket)
full %>% arrange(full$Ticket) # 같은 티켓인데도 불구하고 Family가 single, 친구 등과 같이 온것으로 유추

full$TravelGroup <- NA
full <- (transform(full, TravelGroup = match(Ticket, unique(Ticket)))) # transform : 다수 변수를 이용해서 다수 변수 생성
full <- full %>% group_by(TravelGroup) %>% mutate(GroupSize = n()) %>% ungroup()

full %>% arrange(Ticket) %>% head() # 똑같은 티켓을 가지고 있지만 이름이 다르다.
str(full)

# 변수 선택 = Pclass, Sex, Age, Fare, Embarked, Title, Fsize, GroupSize, Deck
# 범주화 안된 변수들 범주화 차리
factor_vars <- c('Age', 'GroupSize', 'Deck')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
# Fare log 변환
full$Fare = log1p(full$Fare)
full <- full %>% select(-c(1, 4, 7, 8, 9, 13, 16))
str(full)

train <- full %>% filter(is.na(Survived) == F)
test <- full %>% filter(is.na(Survived) == T)
head(train$Survived) ; head(test$Survived)

train_label <- as.numeric(train$Survived) -1 # 왜 -1 일까 찾아보자
test_label <- test$Survived # 결측값들 

x_train <- model.matrix(~.-1, data = train[,-1]) %>% data.frame()
x_test <- model.matrix(~.-1, data = test[,-1]) %>% data.frame()
head(x_train)

# XGBOOST
# cross validation

install.packages("xgboost")
library(xgboost)

dtrain <- xgb.DMatrix(data = as.matrix(x_train), label = train_label)
dtest <- xgb.DMatrix(data = as.matrix(x_test))

set.seed(2019)
param <- list(objective = "binary:logistic",
              eval_metric = "auc",
              max_depth = 6,
              eta = 0.01,
              gammma = 0,
              subsamle = 0.5,
              colsample_bytree = 0.5,
              min_child_weight = 5)

# xgb_cv <- xgb.cv(params = param,
#                 data = dtrain,
#                 nrounds = 5000,
#                 nfold = 5,
#                 nthread = -1,
#                 silent = 1,
#                 print_every_n = 100,
#                 vebose = 0)

# best = xgb_cv$best_iteration # optimal number of tree
# auc = xgb_cv$evaluation_log
# auc %>% filter(test_auc_mean == max(auc[,4]))

xgb <- xgb.train(params = param,
                 data = dtrain,
                 nrounds = 4790,
                 silent = 1,
                 print_every_n = 100,
                 verbose = 0)

# Threshold

library(caret)
set.seed(123)
split <- createDataPartition(y = train$Survived, p = 0.7, list = F) # 7:3 = train:test 

new_train <- train[split,]
new_test <- train[-split,]

x_label <- as.numeric(new_train$Survived) -1
y_label <- as.numeric(new_test$Survived) -1

new_train2 <- model.matrix(~.-1, data = new_train[,-1]) %>% data.frame()
new_test2 <- model.matrix(~.-1, data = new_test[,-1]) %>% data.frame()

dtrain2 <- xgb.DMatrix(data = as.matrix(new_train2), label = x_label)
dtest2 <- xgb.DMatrix(data = as.matrix(new_test2), label = y_label)

xgb2 <- xgb.train(params = param,
                  data = dtrain2,
                  nrounds = 4790,
                  silent = 1,
                  print_every_n = 100,
                  verbose = 0)

set.seed(2019)
XGB_pred2 <- predict(xgb2, dtest2)

head(XGB_pred2, 10)
head(new_test$Survived, 10)

set.seed(2019)
XGB_pred2 <- predict(xgb2, dtest2)
XGB_pred2 <- ifelse(XGB_pred2 >= 0.5, 1, 0)
# plot ROC
library(ROCR)
install.packages("Metrics")
library(Metrics)
pr <- prediction(XGB_pred2, new_test$Survived)
perf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf) 
auc(new_test$Survived, XGB_pred2)

set.seed(2019)
XGB_pred2 <- predict(xgb2, dtest2)
XGB_pred2 <- ifelse(XGB_pred2 >= 0.4, 1, 0)
# plot ROC
pr <- prediction(XGB_pred2, new_test$Survived)
perf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf)
auc(new_test$Survived, XGB_pred2) # 임계값을 0.4로 했을때 성능이 더 좋음

set.seed(2019)
XGB_pred <- predict(xgb, dtest)
XGB_pred <- ifelse(XGB_pred >= 0.4, 1, 0)
xgb.importance(colnames(dtrain), model = xgb) %>% # 영향력을 많이 주는 상위 30순위
  xgb.plot.importance(top_n = 30)


submission_xgb <- read.csv('gender_submission.csv', header = T)
submission_xgb$Survived <- XGB_pred
write.csv(submission_xgb, file = 'submission.csv', row.names = F)












