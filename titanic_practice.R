### 타이타닉 생존자 예측해보기 ###
### 캐글 입문 ###

# Data input, assesment : 데이터 불러들이기, 확인하는 과정
library(readr) # Data input with readr::read_csv()
# install.packages("descr")
library(descr) # descr::CrossTable() - 범주별 빈도수, 비율 수치로 확인

# Visualzation : 데이터 시각화
library(ggplot2)
# install.packages("VIM")
library(VIM) # Missing values assesment used by VIM::aggr() # 결측값 시각화 도
library(RColorBrewer) # plot의 색깔 설정
library(scales) # plot setting - x, y 축 설정

# Feature engineering, Data Pre-processing - 데이터 전처리과정
library(dplyr)
library(purrr) # check missing values
library(tidyr) # tidyr::gather()

# Model generation
# install.packages("randomForest")
library(randomForest)

# Model validation : 원래는 하는게 맞지만 이번 과정에서는 생략
# library(caret)           # caret::confusionMatrix() 
# library(ROCR)            # Plotting ROC Curve

# multiplot() function generation
# 한 화면에 여러개 plot들을 볼 때 유용한 함수가 multiplot() 입니다.
# 하지만 제 개인 노트북의 문제인지 함수가 작동하지 않아서 CRAN을 참고하여 여기서 multiplot() 함수를 생성해서 사용했습니다.

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# 데이터 불러드리기
train <- readr::read_csv('C:/Users/HOJIN/Desktop/kaggle/train.csv')
test <- readr::read_csv('C:/Users/HOJIN/Desktop/kaggle/test.csv')
full <- dplyr::bind_rows(train, test) # test의 survived 변수를 NA로 처리
# 문자열과 요인 변수 구분 못하고 모두 Chr 속성으로 저장한다는 단점이 있다.
str(train) ; str(test) # test 데이터에는 종속변수(생존 유무)가 없다, 두 데이터의 차원이 맞지않아 rbind 사용 불가능
str(full)

# 속성 변환 chr -> factor 로 변환해줌으로써 요인변수로 변인
full <- full %>% 
  dplyr::mutate(Survived = factor(Survived),
                Pclass = factor(Pclass, ordered = T),
                Name = factor(Name),
                Sex = factor(Sex),
                Ticket = factor(Ticket),
                Cabin = factor(Cabin),
                Embarked = factor(Embarked))

head(full, 10) # 결측값들 확인할 수 있다.
str(full) # Cabin 변수에 결측값들이 많다.
summary(full)

# 각 변수의 결측값을 세고 그림으로 나타냄.
VIM::aggr(full, prop = F, combined = T, numbers = T, sortVars = T, sortCombs = T)
# 변수들에 존재하는 결측치 비율 계산
full %>% dplyr::summarize_all(funs(sum(is.na(.))/n()))
# 각 feature의 결측치 비율 계산 -> Data Frame 속성 but 1행 12열 구조로 되어있음.
missing_values <- full %>% dplyr::summarize_all(funs(sum(is.na(.))/n()))
missing_values <- tidyr::gather(missing_values, key = "feature", value = "missing_pct") # 12 x 2 data frame으로 생성
missing_values 
# missing_values를 이용한 시각화 
missing_values %>% ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) + # 내림차순으로 정렬
geom_bar(stat = "identity", fill = "red") + 
  # Title generation
ggtitle("Rate of missing values in each features") +
# Title detail setting
theme(plot.title = element_text(face = "bold", # 글씨체
                                hjust = 0.5, # 가로비율 = 0.5
                                size = 15, color = "darkblue")) +
# x, y axis label setting
labs(x = "Feature names", y = "Rate") +
# Plot의 x, y축 변환
  coord_flip()

#  purrr 패키지를 이용해서 결측치 비율을 계산한 후, 하나라도 존재하는 변수만 추출한 뒤에 시각화 해보았습니다.
# 변수별 결측치 비율 계산
miss_pct <- purrr::map_dbl(full, function(x){round((sum(is.na(x))/ length(x))* 100, 1)})
# 결측치 비율이 0%보다 큰 변수들만 선택
miss_pct <- miss_pct[miss_pct > 0]
# DataFrame 생성
a <- data.frame(miss = miss_pct, var = names(miss_pct), row.names = NULL)
a
# 결측치 비율 시각화
data.frame(miss = miss_pct, var = names(miss_pct), row.names = NULL) %>%
  # Aesthetic setting : miss 내림차순으로 정렬
  ggplot(aes(x = reorder(var, miss), y = miss)) + 
  # Bar plot
  geom_bar(stat = "identity", fill = 'red') + 
  # Plot title setting
  ggtitle("Rate of missing values") + 
  # Title detail Setting
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 15, color = "darkblue")) + 
  # x, y axis label setting
  labs(x = "Feature names", y = "Rate of missing values") + 
  # Plot의 x, y축 반환
  coord_flip()

# 시각화를 통해서 각 변수들의 특징들을 분석, 탐색
age.p1 <- full %>% 
  ggplot(aes(Age)) + 
  geom_histogram(breaks = seq(0, 80, 1), # 간격 설정
                 col = "red", # 막대 경계선 
                 fill = "green", # 막대 내부 색깔
                 alpha = .5) + # 막대 투명도 = 50%
# plot title
  ggtitle("All Titanic passenger age histogram") +
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 15, color = "darkblue"))

# 생존 유무에 따른 age density 그림
age.p2 <- full %>%
  # test data set의 Survived == NA 값들 제외
  filter(!is.na(Survived)) %>%
  ggplot(aes(Age, fill = Survived)) +
  geom_density(alpha = 0.5) + 
  ggtitle("Titanic passengers age density plot") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

# multiplot layout 형식 지정
multi.layout = matrix(c(1,1,2,2), 2, 2, byrow = T)
# 위에서 생성한 2개의 그래프 한 화면에 출력
multiplot(age.p1, age.p2, layout = multi.layout)

# Pclass에 해당하는 탑승객의 빈도수 시각화
full %>% group_by(Pclass) %>% summarize(N = n()) %>%
  ggplot(aes(Pclass, N)) + geom_col() +
  # Pclass 빈도수 plot에 출력
  geom_text(aes(label = N), # Plot의 y에 해당되는 N(빈도수)를 매핑
            size = 5, # 글씨 크기
            vjust = 1.2, # vertical(가로) 위치 설정
            color = "#FFFFFF") + # 글씨 색깔 : 흰색
  # Plot title
  ggtitle("Number of each Pclass's Passengers") + 
  # Title setting
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  # x, y axis name change
  labs(x = "Pclass", y = "Count")

# Fare를 히스토그램으로 표현
Fare.p1 <- full %>% ggplot(aes(Fare)) + geom_histogram(col = "yellow",
                                                       fill = "blue",
                                                       alpha = 0.5) +
  ggtitle("Histogram of passengers fare") +
  theme(plot.title = element_text(face = "bold", hjust = .5, size = 15))

# Fare를 박스플롯으로 표현 (생존자와 사망자 사이의 Fare를 비교할 수 있다.)
Fare.p2 <- full %>% filter(!is.na(Survived)) %>% ggplot(aes(Survived, Fare)) +
  # 관측치를 회색으로 찍되, 중복되는 부분은 퍼지게 그려줍니다.
  geom_jitter(col = "gray") +
  # 상자그림 : 투명도 50%
  geom_boxplot(alpha = 0.5) +
  ggtitle("Boxplot of passengers Fare") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))

# multi layout 형식 지정
multi.layout = matrix(c(1,1,2,2), 2, 2)
# 위에서 생성한 2개의 그래프 한 화면에 출력
multiplot(Fare.p1, Fare.p2, layout = multi.layout)

# 성별에 따른 생존 비율 시각화
# 성별 횟수 세기
sex.p1 <- full %>% dplyr::group_by(Sex) %>% summarize(N = n()) %>%
  ggplot(aes(Sex, N)) +
  geom_col() +
  geom_text(aes(label = N), size = 5, vjust = 1.2, color = "#FFFFFF") +
              ggtitle("Bar plot of Sex") + 
              labs(x = "Sex", y = "Count")

# 성별에 따른 생존 비율 세기
sex.p2 <- full[1:891,] %>%
  ggplot(aes(Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = percent) +
  ggtitle("Survival Rate by Sex") +
  labs(x = "Sex", y = "Rate")

multi.layout = matrix(rep(c(1,2), times = 2), 2, 2, byrow = T)
multiplot(sex.p1, sex.p2, layout = multi.layout)

mosaicplot(Survived ~ Sex,
           data = full[1:891,], col = T,
           main = "Survival rate by passengers gender")
# 그래프를 해석해보면, 남성이 여성보다 훨씬 많은 반면에 생존율은 여성 탑승객이 높음을 알 수 있습니다.

# 4. Feature engineering & Data Pre-processing
# Chapter 3 EDA의 내용들을 바탕으로 결측치(Missing value, NA)를 채우고 동시에 파생변수를 생성하는 과정입니다.

full <- full %>%
  # 결측치(NA)를 먼저 채우는데 결측치를 제외한 값들의 평균으로 채움.
  mutate(Age = ifelse(is.na(Age), mean(full$Age, na.rm = T), Age),
         # Age 값에 따라 범주형 파생 변수 Age.Group 를 생성
         Age.group = case_when(Age < 13 ~ "Age.0012", # case_when 함수 처음 알았음 재밌누
         Age >= 13 & Age < 18 ~ "Age.1317",
         Age >= 18 & Age < 60 ~ "Age.1859",
         Age >= 60 ~ "Age.60inf"),
        # Chr 속성을 Factor로 변환
         Age.group = factor(Age.group))

# SibSp & Parch -> FamilySized
full <- full %>%
  mutate(FamilySize = .$SibSp + .$Parch + 1,
  # SibSp, Parch와 1(본인)을 더해서 FamilySize라는 파생변수를 먼저 생성  
  FamilySized = dplyr::case_when(FamilySize == 1 ~ "Single",
                                 FamilySize >= 2 & FamilySize < 5 ~ "Small",
                                 FamilySize >= 5 ~ "Big"),
# Chr 속성인 FamilySized를 factor로 변환하고
# 집단 규모 크기에 따라 levels를 새로 지정
FamilySized = factor(FamilySized, levels = c("Single", "Small", "Big"))) # 라벨링

# Name & Sex -> title
# 우선 Name 열벡터만 추출해서 title 벡터에 저장 
title <- full$Name
# 정규표현식과 gsub()을 이용해서 성별과 관련성이 높은 이름만 추출해서 title 벡터로 저장 
title <- gsub("^.*, (.*?)\\..*$", "\\1", title)
# 위에서 저장한 title 벡터를 다시 full 에 저장하되, title 파생변수로 저장 
full$title <- title
# 그 다음 고유한(Unique한) title들에는 어떤 것들이 있는지 확인해봅니다.
unique(full$title)

# 범주별 빈도수, 비율 확인
descr::CrossTable(full$title)

full <- full %>% mutate(title = ifelse(title %in% c("Mile", "Ms", "Lady", "Dona"), "Miss", title),
                        title = ifelse(title == "Mme", "Mrs", title),
                        title = ifelse(title %in% c("Capt", "Col", "Don", "Dr", "Jonkheer", "Major", "Master",
                                                    "Rev", "Sir", "the Countess"), "Officer", title),
                        title = factor(title))
descr::CrossTable(full$title)

# Ticket -> Ticket.size
length(unique(full$Ticket)) # 승객은 1309명인데 티켓은 929개로 같은 티켓을 가진 사람들이 존재한다.
head(summary(full$Ticket), 10) # CA.2343 으로 완전히 같은 인원이 11명
full %>% 
  # 티켓이 일치하는 11명의 승객들만 필터링 
  filter(Ticket == "CA. 2343") %>% 
  # 모든 변수에 대해 확인할 필요는 없으므로 아래 변수들만 보려고 합니다.
  select(Pclass, Name, Age, FamilySized)
# 위 코드의 결과는 11명 승객들이 가족인것을 알 수 있다.

# 우선 ticket.unique가 모두 0이라고 저장함
ticket.unique <- rep(0, nrow(full))

# Ticket Feature에서 고유한 것들만 추출해서 tickets 벡터에 저장 
tickets <- unique(full$Ticket)
head(tickets) ; str(tickets) # 929개
# 반복문을 중첩 활용해서 티켓이 같은 승객들만 추출 후, 각 티켓들의 길이(문자 갯수)를 추출해서 저장한다.
for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(full$Ticket == current.ticket)
  # For loop 중첩 
  for (k in 1:length(party.indexes)) {
    ticket.unique[party.indexes[k]] <- length(party.indexes)
  }
}
str(ticket.unique) ; class(ticket.unique) ; class(ticket.size)
# 위에서 계산한 ticket.unique 을 파생변수로 저장 
full$ticket.unique <- ticket.unique

# ticket.unique에 따라 세가지 범주로 나눠서 ticket.size 변수 생성 
full <- full %>% 
  mutate(ticket.size = dplyr::case_when(ticket.unique == 1 ~ 'Single',
                                 ticket.unique < 5 & ticket.unique >= 2 ~ "Small",
                                 ticket.unique >= 5 ~ "Big"),
         ticket.size = factor(ticket.size,
                              levels = c("Single", "Small", "Big")))

head(full$ticket.size) ; head(train$FamilySized) ; head(train$ticket.sized)


# Embarked의 결측값 2개 가장 최빈값인 S로 치환해주기
full$Embarked <- replace(full$Embarked,
                         which(is.na(full$Embarked)), 'S')

# Fare
full$Fare <- replace(full$Fare, which(is.na(full$Fare)), 0)

# 각 변수들이 생존율에 얼마나 영향을 미치는지 확인하기
train <- full[1:891,]
test <- full[892:1309,]

# 시각화
train %>% ggplot(aes(Pclass, fill = Survived)) + geom_bar(position = "fill") +
  # plot 테마설정
  scale_fill_brewer(palette = "Set1") + # Y axis setting
  scale_y_continuous(labels = percent) + # x, y 축의 이름과 plot의 main title, sub title 설정
  labs(x = "Pclass", y = "Rate", title = "Bar plot", subtitle = "How many People survived in Pclass?")

# 성별에 따른 생존율
mosaicplot(Survived ~ Sex, data = train, col = T, main = "Survived rate by passengers gender")

# Embarked
train %>% ggplot(aes(Embarked, fill = Survived)) + geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") + scale_y_continuous(labels = percent) +
  labs(x = "Embarked", y = "Rate", title = "Bar Plot", subtitle = "How many people survived in ecah Embarked")

# FamilySized , 비선형 관계이다. 이유는 가족크기가 커지거나 작아짐에따라 생존률의 변화가 없고 랜덤이기때문.
train %>% ggplot(aes(FamilySized, fill = Survived)) + geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") + scale_y_continuous(labels = percent) +
  labs(x = "FamilySized", y = "Rate", title = "Barplot", subtitle = "Survived by FamilySized")

# Age.group
train %>% ggplot(aes(Age.group, fill = Survived)) + geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") + scale_y_continuous(labels = percent) +
  labs(x = "Age.group", y = "Rate", title = "Barplot", subtitle = "Survival rate by Age group")

# title
train %>% ggplot(aes(title, fill = Survived)) + geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") + scale_y_continuous(labels = percent) +
  labs(x = "title", y = "Rate", title = "Barplot", subtitle = "Survival rate by title")

# ticket.size
train %>% ggplot(aes(ticket.size, fill = Survived)) + geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") + scale_y_continuous(labels = percent) +
  labs(x = "ticket.size", y = "Rate", title = "Barplot", subtitle = "Survival rate by ticket.size")

train <- train %>% select("Pclass", "Sex", "Embarked", "FamilySized", "Age.group", "title", "ticket.size",
                          "Survived")

ID <- test$PassengerId
test <- test %>% select("Pclass", "Sex", "Embarked", "FamilySized", "Age.group", "title", "ticket.size")

# Random Forest model generation
set.seed(123)
titanic.rf <- randomForest(Survived ~ ., data = train, importance = T, ntree = 2000)
# Feature importance check
importance(titanic.rf)
varImpPlot(titanic.rf)

# Prediction
pred.rf <- predict(object = titanic.rf, newdata = test, type = "class")

# Data frame generation
submit <- data.frame(PassengerID = ID, survived = pred.rf)

# Write the submit data frame to file : setwd()로 지정해놓은 폴더에 csv로 생성
setwd("C:\\Users\\HOJIN\\Desktop\\kaggle")
write.csv(submit, file = '.\\titanic.csv', row.names = F)






















