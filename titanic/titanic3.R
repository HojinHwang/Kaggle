library(readr)
library(dplyr)
library(ggplot2)
library(doBy)
library(RColorBrewer)
library(corrplot)
library(stringr)
library(scales)
library(randomForest)
library(gridExtra)

train <- readr::read_csv('C:/Users/HOJIN/Desktop/kaggle/train.csv')
test <- readr::read_csv('C:/Users/HOJIN/Desktop/kaggle/test.csv')
full <- dplyr::bind_rows(train, test) # test의 survived 변수를 NA로 처리

full <- full %>% mutate(Survived = factor(Survived),
                        Pclass = factor(Pclass, ordered = T),
                        Name = factor(Name),
                        Sex = factor(Sex),
                        Embarked = factor(Embarked))

str(full)
head(full)
summary(full)
sapply(train, function(x) length(unique(x))) # 고유 값 확인
colSums(is.na(full))

missing_values <- full %>% dplyr::summarise_all(funs(sum(is.na(.)) / n())) # 결측값 비율 확인
missing_values <- tidyr::gather(missing_values, key = "feature", value = "missing_pct") # gatehr 함수로 wide에서 long으로 전환
missing_values

missing_values %>% ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) + # 정렬을 위한 reorder축 지정
  geom_bar(stat = "identity", fill = "red") + # barplot 그리기 stat = 'identity' 는 데이터 프레임 값을 이용하여 그려라
  ggtitle("Rate of missing values in each features") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  labs(x = "Feature names", y = "Rate") +
  coord_flip()

# 결측값이 있는 변수로만 시각화
missing_values <- missing_values[missing_values$missing_pct > 0,]
missing_values %>% ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) +
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("Rate of missing values in each features") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  labs(x = "Feature names", y = "Rate") +
  coord_flip()

# 변수 EDA
table(full$Sex)
full %>% group_by(Survived, Sex) %>% summarise(freq = n()) # 남성이 여성보다 생존률 낮음
prop.table(table(full$Sex, full$Survived), 1) # 성별에 따른 생존률 테이블

# 성별 막대그래프
sex.p1 <- full %>% dplyr::group_by(Sex) %>% summarise(N = n()) %>% ggplot(aes(Sex, N)) +
  geom_col () + geom_text(aes(label = N), size = 5, vjust = 1.2, color = "#FFFFFF") +
  ggtitle("Bar plot of Sex") + labs(x = "Sex", y = "Count")

# 성별에 따른 생존률 막대그래프
sex.p2 <- full %>% filter(!is.na(Survived)) %>% ggplot(aes(factor(Sex), fill = factor(Survived))) +
  geom_bar(position = "fill") + scale_y_continuous(labels = percent) + # y축 설정
  scale_fill_brewer(palette = "Set1") + # palette 에 색 넣을것 결정
  ggtitle("Survival Rate by Sex") +
  labs(x = "Sex", y = "Rate")

grid.arrange(sex.p1, sex.p2, ncol = 2)

table(full$Pclass)
prop.table(table(full$Pclass, full$Survived), 1) # 선실 등급에 따른 생존율

# Pclass 막대그래프
Pclass.p1 <- full %>% dplyr::group_by(Pclass) %>% summarise(N = n()) %>% ggplot(aes(Pclass, N)) +
  geom_col() + geom_text(aes(label = N), size = 5, vjust = 1.2, color = "#FFFFFF") +
  ggtitle("Bar plot of Pclass") + labs(x = "Pclass", y = "Count")
# Pclass 에 따른 생존
Pclass.p2 <- full %>% filter(!is.na(Survived)) %>% ggplot(aes(factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "fill") + scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set1") + 
  ggtitle("Survival Rate by Pclass") + labs(x = "Pclass", y = "Rate")

grid.arrange(Pclass.p1, Pclass.p2, ncol = 2)

hist(full$Fare)
# Fare에 대한 히스토그램
Fare.p1 <- full %>% ggplot(aes(Fare)) + geom_histogram(col = "yellow",
                                                       fill = "blue",
                                                       alpha = .5) +
  ggtitle("Histogram of passengers Fare") + theme(plot.title = element_text(face = "bold", hjust = .5, size = 15))

# 생존여부에 따른 Fare box plot
Fare.p2 <- full %>% filter(!is.na(Survived)) %>% ggplot(aes(Survived, Fare)) + # x 축에 생존, y 축에 요금
  geom_jitter(col = "gray") + # 관측치를 회색으로 찍되, 중복되는 부분은 퍼지게 그려줍니다.
  geom_boxplot(alpha = .5) + ggtitle("Boxplot of passengers Fare") + 
  theme(plot.title = element_text(face = "bold", hjust = .5, size = 15))

grid.arrange(Fare.p1, Fare.p2, ncol = 2)

hist(full$Age)
# 나이 분포 히스토그램
Age.p1 <- full %>% ggplot(aes(Age)) + geom_histogram(breaks = seq(0, 80, 1),
                                                     col = "red",
                                                     fill = "green",
                                                     alpha = .5) +
  ggtitle("All Titanic Passengers Age Histogram") + theme(plot.title = element_text(face = "bold",
                                                                                    hjust = .5,
                                                                                    size = 15,
                                                                                    color = "darkblue"))

# 나이에 따른 생존 분포 파악
Age.p2 <- full %>% filter(!is.na(Survived)) %>% ggplot(aes(Age, fill = Survived)) +
  geom_density(alpha = .5) + # 밀도그래프다.
  ggtitle("Titanic passenger age density plot") +
  theme(plot.title = element_text(face = "bold", hjust = .5, size = 15, color = "darkblue"))
  
grid.arrange(Age.p1, Age.p2, ncol = 2)

table(full$SibSp)
train %>% group_by(Survived, SibSp) %>% summarise(freq = n())
prop.table(table(full$SibSp, full$Survived), 1) # 1명일때 생존률 제일 높음, 배우자 형제자매가 많을수록 생존률 떨어짐

table(train$Parch)
train %>% group_by(Survived, Parch) %>% summarise(freq = n())
prop.table(table(train$Parch, train$Survived), 1) # 부모, 자녀를 1명에서 3명정도 동승했을때 생존률 가장 높음.

table(train$Embarked)
train %>% group_by(Survived, Embarked) %>% summarise(freq = n())
prop.table(table(train$Embarked, train$Survived), 1) # C 에서 탑승한 인원들이 생존률이 높음

# 결측치 처리
colSums(is.na(full)) # 결측값 갯수 확인

# Embarked 결측치 처리
full[is.na(full$Embarked),] # 두개의 관측치 모두 Pclass가 1이고 Fare가 80임
embark_free <- full[!is.na(full$Embarked),]
ggplot(embark_free, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() + geom_hline(aes(yintercept = 80), # fare가 80에 라인 생성
                              colour = 'red', linetype = 'dashed', lwd = 2) +
  scale_y_continuous()

# Fare가 80이면서 Pclass가 1인 승객들 대다수는 Embark가 C다.
full$Embarked[c(62, 830)] <- 'C'
full[c(62, 830),]

# Fare 결측 처리 (중앙값 이용)
full %>% filter(is.na(Fare)) # Pclass가 3이고 Embarked 는 S이다.
full$Fare[1044] <- median(full[full$Pclass == 3 & full$Embarked == 'S', ]$Fare, na.rm = T)
full[1044,]

# Feature Engineering
# Name 에서 성별과 관련된 이름을 추출하여 범주화해서 Title 이라는 파생변수 생성한다.
Title <- full$Name
Title <- gsub("^.*, (.*?)\\..*$", "\\1", Title) # 정규 표현
full$Title <- Title
unique(full$Title)

# 범주별 빈도수, 비율 확인
descr::CrossTable(full$Title)

# 5개 범주로 단순화
full <- full %>% mutate(Title = ifelse(Title %in% c("Mlle", "Ms", "Lady", "Dona"), "Miss", Title),
                        Title = ifelse(Title == "Mme", "Mrs", Title),
                        Title = ifelse(Title %in% c("Capt", "Col", "Major", "Dr", "Rev", "Don", "Sir",
                                                    "the Countess", "Jonkheer"), "Officer", Title),
                        Title = factor(Title))


descr::CrossTable(full$Title)

full$Sex <- ifelse(full$Sex == "male",0 , 1)
full$Sex <- as.factor(full$Sex)

# Sibsp와 Parch를 이용하여 Fsize 파생변수 생성한다.
full$Fsize <- full$SibSp + full$Parch + 1
table(full$Fsize)

# Fsize에 따른 생존률 시각화
Fsize.p1 <- full %>% filter(!is.na(Survived)) %>% ggplot(aes(Fsize, fill = Survived)) + 
  geom_bar(position = "fill") + scale_y_continuous(labels = percent) + scale_x_continuous(breaks = c(1:11)) + # x축 간격 설정
  scale_fill_brewer(palette = "Set1") + ggtitle("Survival Rate by Fsize") + labs(x = "Fsize", y = "Rate")

Fsize.p1  

# ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) + 
#   geom_bar(stat = 'count', position = 'fill') + # position = 'fill', 'dodge' 구분하기
#   scale_x_continuous(breaks = c(1:11)) +
#   labs(x = "Family Size", y = "Rate")

full$Familysize[full$Fsize == 1] <- 'single'
full$Familysize[full$Fsize > 1 & full$Fsize < 5] <- 'small'
full$Familysize[full$Fsize > 4] <- 'large'

full$Familysize <- as.factor(full$Familysize)
table(full$Familysize)

# 범주화 후 FamilySize에 따른 생존률 시각화
ggplot(full[1:891, ], aes(x = Familysize, fill = Survived)) + geom_bar(position = "fill") +
  ggtitle("Survival Rate by Familysize") + labs(x = "Familysize", y = "Rate")

full$Cabin[1:28]
strsplit(full$Cabin[2], NULL)[[1]]

full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
str(full$Deck) ; head(full$Deck, 10)
full$Deck = as.character(full$Deck)
str(full)

# Cabin 변수 제거
full = full[,-11]
str(full) ; head(full)

full$Deck[is.na(full$Deck)] <- "U" # Deck 결측값에 U라는 값을 대입
cabin = full %>% filter(!is.na(full$Survived) & full$Deck != 'U') # 결측값이 아닌것들만 뽑아서 cabin에 대입

ggplot(cabin, aes(x = Deck, fill = factor(Survived), na.rm = T)) +
  geom_bar(stat = 'Count') + facet_grid(.~Pclass) + labs(title = "Survival split by Pclass and Deck")
# facet_grid = 집단간 비교를 위한 면 분할 즉, 위의 코드는 Pclass을 기준으로 면 분할을 실행함.

full = full %>% mutate(Deck = ifelse(Pclass == 1 & Deck =="U", "X",
                       ifelse(Pclass == 2 & Deck == "U", "Y",
                              ifelse(Pclass == 3 & Deck == "U", "Z", Deck))))

full %>% count(Deck)

# Age 결측 처리
# Sex에 따른 Age 탐색
age.sex <- full %>% ggplot(aes(Age, fill = Sex)) + geom_density(alpha = .5) +
  ggtitle("Titanic passengers Age density plot") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

age.sex

# Pclass에 따른 Age 탐색
age.pclass <- full %>% ggplot(aes(Age, fill = Pclass)) +
  geom_density(alpha = 0.5) +
  ggtitle("Titanic passengers Age density plot") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

age.pclass

# Title에 따른 Age 탐색
age.title <- full %>% ggplot(aes(Age, fill = Title)) +
  geom_density(alpha = 0.5) +
  ggtitle("Titanic passengers Age density plot") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

age.title

# title에 따른 결측처리 방법 선택한다. 각 분포가 정규분포라고 보기 힘드므로 중앙값 사용한다.
plot(full$Title)

# title별 Median Age를 통한 결측값 처리
full = as.data.frame(full)
summaryBy(Age ~ Title, data = full, FUN = c(mean, sd, median), na.rm = T)

full$Age <- ifelse((is.na(full$Age) & full$Title == 'Master'), 4, full$Age)
full$Age <- ifelse((is.na(full$Age) & full$Title == 'Miss'), 22, full$Age)
full$Age <- ifelse((is.na(full$Age) & full$Title == 'Mr'), 29, full$Age)
full$Age <- ifelse((is.na(full$Age) & full$Title == 'Mrs'), 35, full$Age)
full$Age <- ifelse((is.na(full$Age) & full$Title == 'Officer'), 48, full$Age)

# Age 변수 가공
hist(full$Age, freq = F, main = "Age", col = "lightgreen", ylim = c(0, 0.05))
full$Age <- ifelse(full$Age <= 18, "child",
                   ifelse(full$Age > 18 & full$Age <= 64, "adult", "senior"))

# Ticket 변수를 이용하여 GroupSize 파생변수를 생성한다.
length(unique(full$Ticket))
head(full$Ticket)
full %>% arrange(Ticket) # 같은 티켓인데도 불구하고 Family가 single, 친구등과 같이 온것으로 유추

full$TravelGroup <- NA
full <- (transform(full, TravelGroup = match(Ticket, unique(Ticket)))) # TravelGroup = 여행자 수
full <- full %>% group_by(TravelGroup) %>% mutate(GroupSize = n()) %>% ungroup()
full %>% arrange(Ticket) %>% head()

# Predict

str(full)
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

#Fare log변환
full$Fare=log1p(full$Fare)

full=full  %>%  select(-c(1,4,7,8,9,13,16))
str(full)

train <- full %>% filter(is.na(Survived) == F)
test <- full %>% filter(is.na(Survived) == T)

train_label <- as.numeric(train$Survived) -1
test_label <- test$Survived

x_train <- model.matrix(~.-1, data = train[,-1]) %>% data.frame
x_test <- model.matrix(~.-1, data = test[,-1]) %>% data.frame

# XGBOOST
# cross validation
library(xgboost)

dtrain <- xgb.DMatrix(data = as.matrix(x_train), label = train_label)
dtest <- xgb.DMatrix(data = as.matrix(x_test))

set.seed(2019)
param <- list(objective = "binary:logistic",
              eval_metric = "auc",
              max_depth = 6,
              eta = 0.01,
              gamma = 0,
              submsamle = 0.5,
              colsample_bytree = 0.5,
              min_child_weight = 5)

# xgb_cv <- xgb.cv(params = param,
#                 data = dtrain,
#                 nrounds = 5000,
#                 nfold = 5,
#                 nthread = -1,
#                 silent = 1,
#                 print_every_n = 100,
#                 verbose = 0)

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
split <- createDataPartition(y = train$Survived,p = 0.7,list = FALSE)

new_train <- train[split,] 
new_test <- train[-split,]


x_label= as.numeric(new_train$Survived)-1
y_label= as.numeric(new_test$Survived)-1

new_train2 <- model.matrix(~.-1, data = new_train[,-1]) %>% data.frame
new_test2 <- model.matrix(~.-1, data = new_test[,-1]) %>% data.frame

dtrain2 <- xgb.DMatrix(data = as.matrix(new_train2), label=x_label)
dtest2 <- xgb.DMatrix(data = as.matrix(new_test2), label=y_label)

xgb2 <- xgb.train(params  = param,
                 data    = dtrain2,
                 nrounds = 4790,
                 silent = 1,
                 print_every_n = 100,
                 verbose = 0)

set.seed(2019)
XGB_pred2 <- predict(xgb2, dtest2)
head(XGB_pred2,10)
head(new_test$Survived,10)

set.seed(2019)
XGB_pred2 <- predict(xgb2, dtest2)
XGB_pred2 <- ifelse(XGB_pred2 >= 0.5, 1, 0)
#plot ROC
library(ROCR)
library(Metrics)
pr <- prediction(XGB_pred2, new_test$Survived)
perf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf)
auc(new_test$Survived, XGB_pred2)

set.seed(2019)
XGB_pred2 <- predict(xgb2, dtest2)
XGB_pred2 <- ifelse(XGB_pred2 >= 0.4, 1, 0)
pr <- prediction(XGB_pred2, new_test$Survived)
perf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf)
auc(new_test$Survived, XGB_pred2) # 임계값을 0.4로 했을때 값이 더 좋음

# Submission
set.seed(2019)
XGB_pred <- predict(xgb, dtest)
XGB_pred <- ifelse(XGB_pred >= 0.4, 1, 0)
xgb.importance(colnames(dtrain), model = xgb) %>% 
  xgb.plot.importance(top_n = 30)

submission_xgb <- read.csv("submission.csv")
submission_xgb$Survived <- XGB_pred
write.csv(submission_xgb, file = "submission_xgb.csv", row.names = F)










