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
                                                          fill = as.factor(dt_1tran$TARGET), alpha = 0.2)) +
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































