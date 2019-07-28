sample_size <- 1

load.libraries <- c('plyr', 'dplyr', 'data.table', 'readxl', 'reshape2', 'stringr', 'stringi', 'ggplot2',
                    'tidyverse', 'gridExtra', 'matrixStats', 'lubridate', 'corrplot', 'e1071', 'xgboost', 'caret', 'zoo',
                    'factoextra', 'plotly', 'DT')
install.lib <- load.libraries[!load.libraries %>% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = T)
sapply(load.libraries, require, character = T)

# Function to change index to column
index_to_col <- function(data, Column_Name){
  data <- cbind(newColName = rownames(data), data)
  rownames(data) <- 1:nrow(data)
  colnames(data)[1] <- Column_Name
  return (data)
}

# Loading all the plotting functions
plotHist <- function(data_in, i){
  data <- data.frame(x = data_in[[i]])
  p <- ggplot(data = data, aes(x = x)) + geom_histogram(bins = 100, fill = "#0072B2", alpha = .9) +
    xlab(colnames(data_in)[i]) + theme_light() +
    theme(axis.title = element_text(angle = 90, hjust = 1))
  return(p)
}

plotBar <- function(data_in, i){
  data <- data.frame(cbind(x = data_in[[i]], y = dt1_tran[,c("TARGET")]))
  data %>% mutate(x = x, class = as.character(y.TARGET)) %>%
    group_by(x, class) %>% summarise(count_class = n()) %>%
    group_by(x) %>% mutate(count_man = sum(count_class)) %>%
    mutate(percent = count_class / count_man * 100) %>%
    ungroup() %>%
    ggplot(aes(x = x, y = percent, group = class)) +
    geom_bar(aes(fill = class, color = class), stat = "identity") +
    geom_text(aes(label = sprintf("%0.1f%%", percent)), 
              position = position_stack(vjust = 0.5)) + theme_light() + theme_light() +
    theme(axis.text = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
}

plotCorr <- function(data_in, list1, list2, i){
  data <- data.frame(x = data_in[[list1[i]]], y = data_in[[list2[i]]])
  p <- ggplot(data, aes(x = x, y = y)) + geom_smooth(method = lm) + geom_point(aes(x = x, y = y)) +
    geom_jitter(width = 0.1, height = 0.1) + xlab(paste0(list[i], '\n', 'R-squared : ', round(cor(data_in[[list1[i]]], data_in[[list2[i]]], 
                                                                                                  use = 'pairwise.complete.obs'), 3))) +
    theme_light() + ylab(paste0(list2[i]))
  return(suppressWarnings(p))
}

doPlotsCorr <- function(data_in, fun, list1, list2, ii, ncol = 3){
  pp <- list()
  for (i in ii){
    p <- fun(data_in = data_in, list1, list2, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol = ncol))
}

plotDen <- function(data_in, i){
  data <- data.frame(x = data_in[[i]])
  p <- ggplot(data = data, aes(x = x)) + geom_density(aes(group = as.factor(dt1_tran$TARGET), color = as.factor(dt1_tran$TARGET),
                                                          fill = as.factor(dt1_tran$TARGET), alpha = 0.2)) +
    xlab(colnames(data_in)[i]) + theme_light() +
    theme(axis.text = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
  return(p)
}

doPlots <- function(data_in, fun, ii, ncol = 3){
  pp <- list()
  for (i in ii){
    p <- fun(data_in = data_in, i = i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol = ncol))
}

# Data Overview
dt1 <- fread("C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\home credit\\home-credit-default-risk\\application_train.csv", showProgress = F)
test <- fread("C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\home credit\\home-credit-default-risk\\application_test.csv", showProgress = F)
head(dt1, 10)
summary(dt1)

# Cleaning the data
dt1$DAYS_EMPLOYED <- replace(dt1$DAYS_EMPLOYED, dt1$DAYS_EMPLOYED == 365243, NA)
col_neg <- unlist(dt1[, sapply(dt1, FUN = function(x) all (x <= 0, na.rm = T))])
dt1_abs <- setDT(dt1)[,..col_neg]
dt1_abs <- abs(dt1_abs)

rm_col <- colnames(dt1_abs)
dt1 <- as.data.frame(dt1)[, !(colnames(dt1) %in% rm_col)]
dt1 <- cbind(dt1_abs, dt1)

numeric_list <- unlist(lapply(dt1, is.numeric))
dt1_num <- setDT(dt1)[,..numeric_list]

doPlots(dt1_num, plotHist, ii = 1:20)
doPlots(dt1_num, plotHist, ii = 21:41)
doPlots(dt1_num, plotHist, ii = 42:62)
doPlots(dt1_num, plotHist, ii = 63:83)
doPlots(dt1_num, plotHist, ii = 84:106)

# Skewness of the data
skewValues <- as.data.frame(apply(dt1_num, 2, function(x) skewness(x, na.rm = T)))
colnames(skewValues)[1] <- "skew_values"
skewValues <- index_to_col(skewValues, 'Column')
skewValues <- setDT(skewValues)[order (skew_values, decreasing = T)]
skewValues[sample(1:nrow(skewValues), size = nrow(skewValues)), ] %>% # 왜도 확인 가능
  datatable(filter = 'top', options = list(pageLength = 15, autoWidth = F))

BoxCoxValues <- apply(dt1_num, 2, function(x) BoxCoxTrans(x, na.rm = T))
x = list()
for(i in 1:ncol(dt1_num)){
  lambda <- BoxCoxValues[[i]][[1]]
  x[[i]] <- lambda
}

lambda = do.call(rbind, x)
lambda_df <- as.data.frame(cbind(colnames(dt1_num), lambda))
colnames(lambda_df)[1] <- "Column"
colnames(lambda_df)[2] <- "lambda"
knitr::kable(setDT(lambda_df)[!is.na(lambda)])

BoxCoxValues <- apply(dt1_num, 2, function(x) BoxCoxTrans(x, na.rm = T))
x = list()

# lambda 값 반환
for (i in 1:ncol(dt1_num)){
  lambda <- BoxCoxValues[[i]][[1]]
  x[[i]] <- lambda
}

lambda = do.call(rbind, x)
lambda_df <- as.data.frame(cbind(colnames(dt1_num), lambda))
colnames(lambda_df)[1] <- "Column"
colnames(lambda_df)[2] <- "lambda"
knitr::kable(setDT(lambda_df)[!is.na(lambda)])

preProcValues <- preProcess(dt1, method = "BoxCox")
preProcValues

dt1_tran <- predict(preProcValues, dt1)
# Recreate numeric list with new dt1_tran
numeric_list <- unlist(lapply(dt1_tran, is.numeric))
dt1_num <- setDT(dt1_tran)[,..numeric_list]

col_trans <- lambda_df[!is.na(lambda)]$Column
i = 5
x <- list(
  title = as.character(col_trans[i])
)
p1 <- plot_ly(x = ~setDT(dt1)[,get(as.character(col_trans[i]))], type = "histogram", autobinx = F) %>%
  layout(showlegend = F)
p2 <- plot_ly(x = ~setDT(dt1_tran)[,get(as.character(col_trans[i]))], type = "histogram", autobinx = F) %>%
  layout(showlegend = F)
subplot(p1, p2) # 왼쪽으로 기울어 진 그래프 형태를 정규분포 형태로 만들어준것을 알 수 있다.

# without Transformation 왜도 그대로임.
doPlots(as.data.frame(dt1)[, (colnames(dt1) %in% as.character(col_trans))], plotHist, ii = 1:length(col_trans)) # lambda 값이 존재하는 열의 히스토그램

# After Transformation 정규분포를 하는 히스토그램으로 바뀜. dt1_tran 은 왜도를 수정해주는 함수.
doPlots(as.data.frame(dt1_tran)[, (colnames(dt1_tran) %in% as.character(col_trans))], plotHist, ii = 1:length(col_trans))

# Transform to resolve Outliers
# Finding the % of missing values for all columns
mv <- as.data.frame(apply(dt1_tran, 2, function(col)sum(is.na(col))/length(col)))
colnames(mv)[1] <- "missing_values"
head(mv, 10)
mv <- index_to_col(mv,'Column')
mv <- setDT(mv)[order (missing_values, decreasing = TRUE)]

ggplot (mv[1:40,], aes (reorder(Column, missing_values), missing_values)) + 
  geom_bar (position = position_dodge(), stat = "identity") + coord_flip () + xlab('Columns') + ylab('Missing Value %')

# 결측값의 비율이 60% 넘으면 제거, 나머지는 평균값으로 대체
dt1_num2 <- na.aggregate(dt1_num)

regexp <- "[[:digit:]]+"
pcaObject <- prcomp(dt1_num2, scale = T, center = T)
eig_tb <- cbind(Dimensions = rownames(get_eig(pcaObject)), get_eig(pcaObject)) # eig = 고유벡터
ts <- setDT(eig_tb)[cumulative.variance.percent > 80][1,1]
ts <- str_extract(as.character(ts[[1]]), regexp) # regualr express ? , regexp 함수로는 정의 안나와있음, regexpr 은 grep 과 비슷한 역할의 함수

n <- as.numeric(ts)
col_list <- list()
for (i in 1:n){
    col_list[i] <- paste('rotation.PC', i, sep = "")
  }

pca_df <- as.data.frame(pcaObject[2])
pca_df <- pca_df[, colnames(pca_df) %in% col_list]
pca_df <- cbind(Features = rownames(pca_df), pca_df)
pca_df <- setDT(pca_df)[order (rotation.PC1, decreasing = T)]

# The scree plot is used to determine the number of components and the variability
fviz_eig(pcaObject)

nzv <- nearZeroVar(dt1, saveMetrics = T)
nzv <- index_to_col(nzv, "Column")
nzv_tb <- setDT(nzv)[nzv == TRUE | zeroVar == TRUE]
nzv_tb[sample(1:nrow(nzv_tb), size = nrow(nzv_tb)),] %>% datatable(filter = 'top', options = list(
  pageLength = 15, autoWidth = T))

# Saving columns with nzv
rm_col_nzv <- as.character(setDT(nzv)[nzv == T | zeroVar == T]$Column)

df_corr <- cor(dt1_num2, use = "pairwise.complete.obs")
hc = findCorrelation(df_corr, cutoff = 0.80) # 0.8 이상의 상관계수 값을 찾는다.
hc = sort(hc)
dt1_num3 <- as.data.frame(dt1_num2)[,-c(hc)]

rm_col_hc <- setdiff(colnames(dt1_num2), colnames(dt1_num3))
rm_col_hc # 상관계수가 0.8 이상인 변수들

df_corr2 <- df_corr %>% as.data.frame() %>% mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>% arrange(desc(value)) %>% group_by(value)

corr_tb <- setDT(df_corr2)[abs(value) > 0.8 & var1 != var2 & var1 != "Tt1_Rating" & var2 != "Tt1_Rating"]
corr_tb <- corr_tb[!duplicated(corr_tb$value),]

l1 <- corr_tb$var1
l2 <- corr_tb$var2

corr_tb[sample(1:nrow(corr_tb), size = nrow(corr_tb)), ] %>% datatable(filter = 'top', options = list(
  pageLength = 15, autoWidth = T))

# Scatter Plots(Highly Correlated Variables)
doPlotsCorr(dt1_num2, plotCorr, l1, l2, 1:6)
doPlotsCorr(dt1_num2, plotCorr, l1, l2, 13:27)
doPlotsCorr(dt1_num2, plotCorr, l1, l2, 71:83)
doPlotsCorr(dt1_num2, plotCorr, l1, l2, 58:70)

# Removing all the columns identified as highly correlated and/or nzv
rm_col_all <- append(rm_col_hc, rm_col_nzv)
dt1_tran <- as.data.frame(dt1_tran)[, !colnames(dt1_tran) %in% rm_col_all]

# Recreate numeric list with new dt1_tran
numeric_list <- unlist(lapply(dt1_tran, is.numeric))
dt1_num <- setDT(dt1_tran)[,..numeric_list]

doPlots(dt1_num2, plotDen, ii = 1:20)
doPlots(dt1_num2, plotDen, ii = 21:40)

non_numeric_list <- unlist(lapply(dt1_tran, is.character))
dt1_non_num <- setDT(dt1_tran)[,..non_numeric_list]

# BarPlot of the categorial and Target Variable
doPlots(dt1_non_num, plotBar, ii = 1:9)
doPlots(dt1_non_num, plotBar, ii = c(9, 11, 13:16))
grid.arrange(plotBar(dt1_non_num, 10), plotBar(dt1_non_num, 12), ncol = 1, nrow = 2)

# Attaching numeric and non numeric columns
dt1_preproc <- cbind(dt1_non_num, dt1_num)
mv <- as.data.frame(apply(dt1_preproc, 2, function(col)sum(is.na(col)) / length(col))) # 결측값 비율 확인
colnames(mv)[1] <- "missing_values"
mv <- index_to_col(mv, 'Column')
mv <- setDT(mv)[order(missing_values, decreasing = T)]

ggplot(mv[1:40,], aes(reorder(Column, missing_values), missing_values)) + geom_bar(position = position_dodge(), stat = "identity") +
  coord_flip() + xlab('Columns') + ylab('Missing Values %')

dt1_preproc <- na.aggregate(dt1_preproc)
View(dt1_preproc)


## Over fitting and Model Tuning
# The simplest way to split the data into a training and test set is to take a random sample

set.seed(1234)
dt1_preproc_sample <- setDT(dt1_preproc)[sample(nrow(dt1_preproc), round(nrow(dt1_preproc)*0.01, 0)),]

# control <- rfeControl(functions = rfFuncs, method = "cv", number = 3)
# trainctrl <- trainControl(classProbs = T, summaryFunction = twoClassSummary)
# results <- rfe(as.data.frame(dt1_preproc_sample)[,-c(153)],
#                             as.data.frame(dt1_preproc_sample)[,c(153)],
#                             sizes = c(1:100),
#                             rfeControl = control, method = "rf", metric = "AUC", trControl = trainctrl)
# print(results)
# predictors(results)
# plot(results, type = c("g", "o"))

# boruta.train <- Boruta(TARGET ~., data = dt1_preproc, doTrace = 2)
# print(boruta.train)

# cols_to_keep <- c(predictors(results), "TARGET")
cols_to_keep <- c('FLAG_OWN_CARN','`ORGANIZATION_TYPEIndustry: type 1`','DAYS_ID_PUBLISH','SK_ID_CURR','REG_CITY_NOT_LIVE_CITY',
                  'YEARS_BEGINEXPLUATATION_MODE','COMMONAREA_MODE','FLOORSMAX_MODE','LIVINGAPARTMENTS_MODE','YEARS_BUILD_MEDI',
                  'CODE_GENDERM','OCCUPATION_TYPEWaiters/barmen staff','TARGET','EXT_SOURCE_1','EXT_SOURCE_2','EXT_SOURCE_3')
dt1_preproc_sample <- as.data.frame(dt1_preproc_sample)[,(colnames(dt1_preproc_sample) %in% cols_to_keep)]

# Creating a Data Partition for Training and Testing
predictors <- setDT(dt1_preproc_sample)[, -c('TARGET')]
classes <- as.factor(dt1_preproc_sample$TARGET)
trainingRows <- createDataPartition(y = classes, p = 0.8, list = F)
trainPredictors <- predictors[trainingRows,]
trainclasses <- classes[trainingRows]
testPredictors <- predictors[-trainingRows,]
testClasses <- classes[-trainingRows]

cvSplits <- createFolds(trainclasses, k = 10, returnTrain = T)
repeatedSplits <- createDataPartition(trainclasses, p = 0.8, times = 3)

# the Bootstrap : A bootstrap sample is a random sample of the data taken with replacement. 
bsSplits <- createResample(trainclasses, times = 10, list = T)

# Running a Simple model
dt1_preproc_sample <- mutate(dt1_preproc_sample, TARGET = ifelse(TARGET == 0, 'Yes', "No"))
dt1_preproc_sample$TARGET <- as.factor(dt1_preproc_sample$TARGET)

inTrain <- createDataPartition(dt1_preproc_sample$TARGET, p = .8)[[1]]
dtTrain <- dt1_preproc_sample[ inTrain, ]
dtTest <- dt1_preproc_sample[ -inTrain, ]

traincntrl <- trainControl(method = 'repeatedcv',
                           number = 5,
                           repeats = 2,
                           classProbs = TRUE, 
                           sampling = "down",
                           summaryFunction = twoClassSummary)

trainPredictors <- as.matrix(trainPredictors)



























































