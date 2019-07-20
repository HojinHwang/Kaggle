# Porto Seguro

# general visualisation
library('ggplot2')
library('scales')
library('grid')
# install.packages('ggthemes')
library('ggthemes')
library('gridExtra')
library('RColorBrewer')
library('corrplot')

# general data manipulation
library('dplyr') # data manipulation
library('readr') # input / output
library(data.table) # data manipulation
library(tibble) # data wrangling
library(tidyr) # data wrangling
library(stringr) # string manipulation
library(forcats) # factor manipulation
library(rlang) # data manipulation

# specific visualisation
library(alluvial)
library(ggrepel)
library(ggridges)
library(VIM) # NAs
library(plotly)
library(ggforce)

# modeling
library(xgboost)
library(caret)
library(MLmetrics)


# Define multiple plot function
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL){
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if(is.null(layout)){
    # Make the panel
    # ncol : Number of columns of plots
    # nrow : Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if(numPlots == 1){
    print(plots[[1]])
  }
  else{
    # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
  
  # Make each plot, in the correct location
  for(i in 1:numPlots){
    # Get the i, j  matrix positions of the regions that contains this subplot
    matchidx <- as.data.frame(which(layout == i, arr.ind = T))
    
    print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                    layout.pos.col = matchidx$col))
  }
  
}
}

# function to extract binomial confidence levels
get_binCI <- function(x, n) as.list(setNames(binom.test(x, n)$conf.int, c("lwr", "upr")))


train <- as.tibble(fread('C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\porto\\train.csv', na.strings = c("-1", "-1.0")))
test <- as.tibble(fread('C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\porto\\test.csv', na.strings = c("-1", "-1.0")))
sample_submit <- as.tibble(fread('C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\porto\\sample_submission.csv'))

summary(train)
glimpse(train)
summary(test)
glimpse(test)

sum(is.na(train)) ; sum(is.na(test))

# "cat" 으로 끝나는 이름을 가진 열들을 factor 형태로 변환, "bin" 으로 끝나는 이름을 가진 열들을 logical 로 변환
train <- train %>% mutate_at(vars(ends_with("cat")), funs(factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical)) %>% mutate(target = factor(target))

test <- test %>% mutate_at(vars(ends_with("cat")), funs(factor)) %>% 
  mutate_at(vars(ends_with("bin")), funs(as.logical))

combine <- bind_rows(train %>% mutate(dset = "train"),
                     test %>% mutate(dset = "test", target = NA))

combine <- combine %>% mutate(dset = factor(dset))


# Binary Feature part
p1 <- train %>% ggplot(aes(ps_ind_06_bin, fill = ps_ind_06_bin)) +
  geom_bar() + theme(legend.position = "none") # legend.position : 범례 위치 바꾸기, "none" 바꾸지 않는다.

p2 <- train %>% ggplot(aes(ps_ind_07_bin, fill = ps_ind_07_bin)) +
  geom_bar() + theme(legend.position = "none")

p3 <- train %>% ggplot(aes(ps_ind_08_bin, fill = ps_ind_08_bin)) +
  geom_bar() + theme(legend.position = "none")

p4 <- train %>% ggplot(aes(ps_ind_09_bin, fill = ps_ind_09_bin)) +
  geom_bar() + theme(legend.position = "none")

p5 <- train %>% ggplot(aes(ps_ind_10_bin, fill = ps_ind_10_bin)) +
  geom_bar() + theme(legend.position = "none")

p6 <- train %>% ggplot(aes(ps_ind_11_bin, fill = ps_ind_11_bin)) +
  geom_bar() + theme(legend.position = "none")

p7 <- train %>% ggplot(aes(ps_ind_12_bin, fill = ps_ind_12_bin)) +
  geom_bar() + theme(legend.position = "none")

p8 <- train %>% ggplot(aes(ps_ind_13_bin, fill = ps_ind_13_bin)) +
  geom_bar() + theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,6,7,8), 2, 4, byrow = T)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, layout = layout)

p1 <- train %>% ggplot(aes(ps_ind_16_bin, fill = ps_ind_16_bin)) +
  geom_bar() + theme(legend.position = "none")

p2 <- train %>% ggplot(aes(ps_ind_17_bin, fill = ps_ind_17_bin)) +
  geom_bar() + theme(legend.position = "none")

p3 <- train %>% ggplot(aes(ps_ind_18_bin, fill = ps_ind_18_bin)) +
  geom_bar() + theme(legend.position = "none")

p4 <- train %>% ggplot(aes(ps_calc_15_bin, fill = ps_calc_15_bin)) +
  geom_bar() + theme(legend.position = "none")

p5 <- train %>% ggplot(aes(ps_calc_16_bin, fill = ps_calc_16_bin)) +
  geom_bar() + theme(legend.position = "none")

p6 <- train %>% ggplot(aes(ps_calc_17_bin, fill = ps_calc_17_bin)) +
  geom_bar() + theme(legend.position = "none")

p7 <- train %>% ggplot(aes(ps_calc_18_bin, fill = ps_calc_18_bin)) +
  geom_bar() + theme(legend.position = "none")

p8 <- train %>% ggplot(aes(ps_calc_19_bin, fill = ps_calc_19_bin)) +
  geom_bar() + theme(legend.position = "none")

p9 <- train %>% ggplot(aes(ps_calc_20_bin, fill = ps_calc_20_bin)) +
  geom_bar() + theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,6,7,8,9,9), 2,5, byrow = T)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, layout = layout)

# Categorial features part 1
library(hexbin)

p1 <- train %>% ggplot(aes(ps_ind_02_cat, fill = ps_ind_02_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

p2 <- train %>% ggplot(aes(ps_ind_04_cat, fill = ps_ind_04_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

p3 <- train %>% ggplot(aes(ps_ind_05_cat, fill = ps_ind_05_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

p4 <- train %>% ggplot(aes(ps_car_01_cat, fill = ps_car_01_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

p5 <- train %>% ggplot(aes(ps_car_02_cat, fill = ps_car_02_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

p6 <- train %>% ggplot(aes(ps_car_03_cat, fill = ps_car_03_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,6), 2, 3, byrow = T)
multiplot(p1,p2,p3,p4,p5,p6, layout = layout)

##
p1 <- train %>% ggplot(aes(ps_car_04_cat, fill = ps_car_04_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

p2 <- train %>% ggplot(aes(ps_car_05_cat, fill = ps_car_05_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

p3 <- train %>% ggplot(aes(ps_car_06_cat, fill = ps_car_06_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

p4 <- train %>% ggplot(aes(ps_car_07_cat, fill = ps_car_07_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

p5 <- train %>% ggplot(aes(ps_car_08_cat, fill = ps_car_08_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

p6 <- train %>% ggplot(aes(ps_car_09_cat, fill = ps_car_09_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

p7 <- train %>% ggplot(aes(ps_car_10_cat, fill = ps_car_10_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

p8 <- train %>% ggplot(aes(ps_car_11_cat, fill = ps_car_11_cat)) +
  geom_bar() + 
  scale_y_log10() +
  theme(legend.position = "none")

layout <- matrix(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8), 4,4, byrow = T)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, layout = layout)

# Indicator features part 1: "ind" and "car"

p1 <- train %>% mutate(ps_ind_01 = as.factor(ps_ind_01)) %>%
  ggplot(aes(ps_ind_01, fill = ps_ind_01)) +
  geom_bar() +
  theme(legend.position = "none")

p2 <- train %>% mutate(ps_ind_03 = as.factor(ps_ind_03)) %>%
  ggplot(aes(ps_ind_03, fill = ps_ind_03)) +
  geom_bar() +
  theme(legend.position = "none")

p3 <- train %>% mutate(ps_ind_14 = as.factor(ps_ind_14)) %>%
  ggplot(aes(ps_ind_14, fill = ps_ind_14)) +
  geom_bar() +
  theme(legend.position = "none")

p4 <- train %>% mutate(ps_ind_15 = as.factor(ps_ind_15)) %>%
  ggplot(aes(ps_ind_15, fill = ps_ind_15)) +
  geom_bar() +
  theme(legend.position = "none")

p5 <- train %>% mutate(ps_car_11 = as.factor(ps_car_11)) %>%
  ggplot(aes(ps_car_11, fill = ps_car_11)) +
  geom_bar() +
  theme(legend.position = "none")

layout <- matrix(c(1,1,2,2,3,4,4,5), 2,4, byrow = T)
multiplot(p1, p2, p3, p4, p5, layout = layout)

# Integer features part2 : "calc"

p1 <- train %>% mutate(ps_calc_04 = factor(ps_calc_04)) %>%
  ggplot(aes(ps_calc_04, fill = ps_calc_04)) +
  geom_bar() +
  theme(legend.position = "none")

p2 <- train %>% mutate(ps_calc_05 = factor(ps_calc_05)) %>%
  ggplot(aes(ps_calc_05, fill = ps_calc_05)) +
  geom_bar() +
  theme(legend.position = "none")

p3 <- train %>% mutate(ps_calc_06 = factor(ps_calc_06)) %>%
  ggplot(aes(ps_calc_06, fill = ps_calc_06)) +
  geom_bar() +
  theme(legend.position = "none")

p4 <- train %>% mutate(ps_calc_07 = factor(ps_calc_07)) %>%
  ggplot(aes(ps_calc_07, fill = ps_calc_07)) +
  geom_bar() +
  theme(legend.position = "none")

p5 <- train %>% mutate(ps_calc_08 = factor(ps_calc_08)) %>%
  ggplot(aes(ps_calc_08, fill = ps_calc_08)) +
  geom_bar() +
  theme(legend.position = "none")

p6 <- train %>% mutate(ps_calc_09 = factor(ps_calc_09)) %>%
  ggplot(aes(ps_calc_09, fill = ps_calc_09)) +
  geom_bar() +
  theme(legend.position = "none")

p7 <- train %>% 
  ggplot(aes(ps_calc_10, fill = ps_calc_10)) +
  geom_histogram(fill = "blue", binwidth = 1) +
  theme(legend.position = "none")

p8 <- train %>%
  ggplot(aes(ps_calc_11, fill = ps_calc_11)) +
  geom_histogram(fill = "blue", binwidth = 1) + # binwidth = 1 : 구간을 1로 설정
  theme(legend.position = "none")

p9 <- train %>% mutate(ps_calc_12 = as.factor(ps_calc_12)) %>%
  ggplot(aes(ps_calc_12, fill = ps_calc_12)) +
  geom_bar() +
  theme(legend.position = "none")

p10 <- train %>% mutate(ps_calc_13 = as.factor(ps_calc_13)) %>%
  ggplot(aes(ps_calc_13, fill = ps_calc_13)) +
  geom_bar() +
  theme(legend.position = "none")

p11 <- train %>% ggplot(aes(ps_calc_14, fill = ps_calc_14)) +
  geom_histogram(fill = "blue", binwidth = 1) +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,11), 3, 4, byrow = T)
multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11, layout = layout)

# Float features part1 : "reg" and "calc"
# 소수점은 히스토그램 그림

p1 <- train %>% ggplot(aes(ps_reg_01, fill = ps_reg_01)) +
  geom_histogram(fill = "dark green", binwidth = 0.1) +
  theme(legend.position = "none")

p2 <- train %>% ggplot(aes(ps_reg_02, fill = ps_reg_02)) +
  geom_histogram(fill = "dark green", binwidth = 0.1) +
  theme(legend.position = "none")

p3 <- train %>% ggplot(aes(ps_reg_03, fill = ps_reg_03)) +
  geom_histogram(fill = "dark green", binwidth = 0.1) +
  theme(legend.position = "none")

p4 <- train %>% ggplot(aes(ps_calc_01, fill = ps_calc_01)) +
  geom_histogram(fill = "blue", binwidth = 0.1) +
  theme(legend.position = "none")

p5 <- train %>% ggplot(aes(ps_calc_02, fill = ps_calc_02)) +
  geom_histogram(fill = "blue", binwidth = 0.1) +
  theme(legend.position = "none")

p6 <- train %>% ggplot(aes(ps_calc_03, fill = ps_calc_03)) +
  geom_histogram(fill = "blue", binwidth = 0.1) +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,6), 2,3, byrow = T)
multiplot(p1, p2, p3, p4, p5, p6, layout = layout)

# Float features part 2 : "car"
p1 <- train %>% ggplot(aes(ps_car_12, fill = ps_car_12)) +
  geom_histogram(fill = "blue", binwidth = 0.05) +
  theme(legend.position = "none")

p2 <- train %>% ggplot(aes(ps_car_13, fill = ps_car_13)) +
  geom_histogram(fill = "blue", binwidth = 0.1) +
  theme(legend.position = "none")  

p3 <- train %>% ggplot(aes(ps_car_14, fill = ps_car_14)) +
  geom_histogram(fill = "blue", binwidth = 0.01) +
  theme(legend.position = "none")

p4 <- train %>% ggplot(aes(ps_car_15, fill = ps_car_15)) +
  geom_histogram(fill = "blue", binwidth = 0.1) +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4), 2,2, byrow = T)
multiplot(p1, p2, p3, p4, layout = layout)

# Target variable
train %>% ggplot(aes(target, fill = target)) +
  geom_bar() + 
  theme(legend.position = "none")

train %>% group_by(target) %>%
  summarise(percentage = n()/ nrow(train) * 100)

train %>% select(which(colMeans(is.na(.)) > 0)) %>%
  aggr(prop = F, combined = T, numbers = T, bars = F, cex.axis = 0.7)

sum(is.na(train)) / (nrow(train)*ncol(train)) * 100 # 전체 결측값 수치
sum(is.na(test)) / (nrow(test)*ncol(test)) * 100

# Claim rates for individual features
# 각 변수가 보험료를 청구하는지에 대해 얼마나 영향을 미치는지 파악해보자.

# Binary features part 1
p1 <- train %>% 
  group_by(ps_ind_06_bin, target) %>%
  count() %>%
  spread(target, n) %>% # spread = long -> wide
  mutate(frac_claim = `1` / (`1` + `0`) * 100, # 전체중 claim 요구하는 사람의 비율
                               lwr = get_binCI(`1`, (`1` + `0`))[[1]]*100,
                               upr = get_binCI(`1`, (`1` + `0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_06_bin, frac_claim, fill = ps_ind_06_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") + labs(y = "Claims [%]")

p2 <- train %>% group_by(ps_ind_07_bin, target) %>%
  count() %>% spread(target, n) %>% 
  mutate(frac_claim = `1` / (`1` + `0`)*100,
         lwr = get_binCI(`1`, (`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1` + `0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_07_bin, frac_claim, fill = ps_ind_07_bin)) +
  geom_col() + geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") + # errorbar 는 오차를 나타내 줌.
  theme(legend.position = "none") + labs(y = "Claims [%]")

p3 <- train %>% group_by(ps_ind_08_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1` + `0`)*100,
         lwr = get_binCI(`1`, (`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_08_bin, frac_claim, fill = ps_ind_08_bin)) +
  geom_col() + geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") + labs(y = "Claims [%]")

p4 <- train %>% group_by(ps_ind_09_bin, target) %>% 
  count() %>% spread(target, n) %>% 
  mutate(frac_claim = `1` / (`1` + `0`)*100,
         lwr = get_binCI(`1`, (`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_09_bin, frac_claim, fill = ps_ind_09_bin)) + 
  geom_col() + geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") + labs(y = "Claims [%]")

p5 <- train %>% group_by(ps_ind_10_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1` / (`1` + `0`)*100,
         lwr = get_binCI(`1`, (`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_10_bin, frac_claim, fill = ps_ind_10_bin)) +
  geom_col() + geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") + labs(y = "Claims [%]")

p6 <- train %>% group_by(ps_ind_11_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1` / (`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_11_bin, frac_claim, fill = ps_ind_11_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims[%]")

p7 <- train %>% group_by(ps_ind_12_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1` / (`1`+`0`)*100,
         lwr = get_binCI(`1`, (`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_12_bin, frac_claim, fill = ps_ind_12_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims[%]")

p8 <- train %>% group_by(ps_ind_13_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1` / (`1`+`0`)*100,
         lwr = get_binCI(`1`, (`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_13_bin, frac_claim, fill = ps_ind_13_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")


layout <- matrix(c(1,2,3,4,5,6,7,8), 2,4 , byrow = T)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, layout = layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1; p6 <- 1; p7 <- 1; p8 <- 1

# Binary features
p1 <- train %>% group_by(ps_ind_16_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`, (`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_16_bin, frac_claim, fill = ps_ind_16_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p2 <- train %>% group_by(ps_ind_17_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`, (`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_17_bin, frac_claim, fill = ps_ind_17_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p3 <- train %>% group_by(ps_ind_18_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`, (`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_18_bin, frac_claim, fill = ps_ind_18_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p4 <- train %>% group_by(ps_calc_15_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1` / (`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_15_bin, frac_claim, fill = ps_calc_15_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p5 <- train %>% group_by(ps_calc_16_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_16_bin, frac_claim, fill = ps_calc_16_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p6 <- train %>% group_by(ps_calc_17_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_17_bin, frac_claim, fill = ps_calc_17_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p7 <- train %>% group_by(ps_calc_18_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_18_bin, frac_claim, fill = ps_calc_18_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p8 <- train %>% group_by(ps_calc_19_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_19_bin, frac_claim, fill = ps_calc_19_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p9 <- train %>% group_by(ps_calc_20_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_20_bin, frac_claim, fill = ps_calc_20_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

layout <- matrix(c(1,2,3,4,5,6,7,8,9,9), 2,5, byrow = T)
multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9, layout = layout) # ps_ind_16, ps_ind_17 둘만 유의미해 보이고 나머지는 차이가 얼마 없어서 무의미해보인다.

# Categorial features
p1 <- train %>% group_by(ps_ind_02_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_ind_02_cat, -frac_claim, FUN = max), frac_claim, fill = ps_ind_02_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p2 <- train %>% group_by(ps_ind_02_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_ind_02_cat, -frac_claim, FUN = max), frac_claim, fill = ps_ind_02_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_02_cat" ,y = "Claims [%]")

p2 <- train %>% group_by(ps_ind_04_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_ind_04_cat, -frac_claim, FUN = max), frac_claim, fill = ps_ind_04_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_04_cat",y = "Claims [%]")

p3 <- train %>% group_by(ps_ind_05_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_ind_05_cat, -frac_claim, FUN = max), frac_claim, fill = ps_ind_05_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_05_cat",y = "Claims [%]")

p4 <- train %>% group_by(ps_car_01_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_car_01_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_01_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_01_cat", y = "Claims [%]")

p4 <- train %>% group_by(ps_car_01_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_car_01_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_01_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_01_cat", y = "Claims [%]")

p5 <- train %>% group_by(ps_car_02_cat, target) %>%
  count() %>% spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_car_02_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_02_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_02_cat", y = "Claims [%]")

p6 <- train %>% group_by(ps_car_03_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_car_03_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_03_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_03_cat", y = "Claims [%]")

layout <- matrix(c(1,2,3,4,5,6), 2,3, byrow = T)
multiplot(p1, p2, p3, p4, p5, p6, layout = layout) # ps_ind_05를 보면 2번 카테고리가 많은 영향을 주는것을 볼 수 있다.

# cat(categorial) 형태의 변수들이 보험료를 얼마나 청구하는지 확인

p1 <- train %>% group_by(ps_car_04_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_car_04_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_04_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_04_cat", y = "Claims [%]")

p2 <- train %>% group_by(ps_car_05_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_car_05_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_05_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_05_cat", y = "Claims [%]")

p3 <- train %>% group_by(ps_car_06_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_car_06_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_06_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_06_cat", y = "Claims [%]")

p4 <- train %>% group_by(ps_car_07_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_car_07_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_07_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_07_cat", y = "Claims [%]")

p5 <- train %>% group_by(ps_car_08_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_car_08_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_08_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_08_cat", y = "Claims [%]")

p6 <- train %>% group_by(ps_car_09_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_car_09_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_09_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_09_cat", y = "Claims [%]")

p7 <- train %>% group_by(ps_car_10_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_car_10_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_10_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_10_cat", y = "Claims [%]")

p8 <- train %>% group_by(ps_car_11_cat, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(reorder(ps_car_11_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_11_cat)) +
  geom_col() + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_11_cat", y = "Claims [%]")

layout <- matrix(c(1,2,3,4,5,6,7,8), 2,4, byrow = T)
multiplot(p1,p2,p3,p4,p5,p6,p7,p8, layout = layout)

# Integel features
p1 <- train %>% group_by(ps_ind_01, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_01, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_01", y = "Claims [%]")

p2 <- train %>% group_by(ps_ind_03, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_03, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_03", y = "Claims [%]")

p3 <- train %>% group_by(ps_ind_14, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_14, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_14", y = "Claims [%]")

p4 <- train %>% group_by(ps_ind_15, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_15, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_15", y = "Claims [%]")

p5 <- train %>% filter(!is.na(ps_car_11)) %>%
  group_by(ps_car_11, target) %>% count() %>%
  spread(target, n, fill = 0) %>% 
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_car_11, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "ps_car_11", y = "Claims [%]")

layout <- matrix(c(1,1,2,2,3,4,4,5), 2,4 , byrow = T)
multiplot(p1,p2,p3,p4,p5, layout = layout)

p1 <- train %>% group_by(ps_calc_04, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_04, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_04", y = "Claims[%]")

p2 <- train %>% filter(ps_calc_05 < 6) %>%
  group_by(ps_calc_05, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_05, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_05", y = "Claims[%]")

p3 <- train %>% filter(ps_calc_06 > 2) %>% # ps_calc_06 <= 2 인 데이터 수가 139개로 매우 작음.
  group_by(ps_calc_06, target) %>% 
  count() %>%
  spread(target, n, fill = 0) %>% 
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`, (`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_06, frac_claim)) +
  geom_point(color = "blue") + geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_06", y = "Claims [%]")

p4 <- train %>% filter(ps_calc_07 < 8) %>%
  group_by(ps_calc_07, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_07, frac_claim)) +
  geom_point(color = "blue") + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_07", y = "Claims [%]")

p5 <- train %>% filter(ps_calc_08 > 2) %>%
  group_by(ps_calc_08, target) %>%
  count() %>% spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_08, frac_claim)) +
  geom_point(color = "blue") + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_08", y = "Claims [%]")

p6 <- train %>% group_by(ps_calc_09, target) %>%
  count() %>% spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_09, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_09", y = "Claims [%]")

p7 <- train %>% ggplot(aes(ps_calc_10, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.4) + theme(legend.position = "none") # bw = bandwidth

p8 <- train %>% ggplot(aes(ps_calc_11, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.4) + theme(legend.position = "none")

p9 <- train %>% filter(ps_calc_12 < 9) %>%
  group_by(ps_calc_12, target) %>% count() %>%
  spread(target, n, fill = 0) %>% 
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_12, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_12", y = "Claims [%]")

p10 <- train %>% filter(ps_calc_13 < 12) %>%
  group_by(ps_calc_13, target) %>%
  count() %>% spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_calc_13, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") + 
  labs(x = "ps_calc_13", y = "Claims [%]")

p11 <- train %>% ggplot(aes(ps_calc_14, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.4)

layout <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,11),3,4, byrow = T)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, layout = layout)

# Float features 
p1 <- train %>% ggplot(aes(ps_reg_01, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p2 <- train %>% ggplot(aes(ps_reg_02, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p3 <- train %>% ggplot(aes(ps_reg_03, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p4 <- train %>% ggplot(aes(ps_calc_01, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p5 <- train %>% ggplot(aes(ps_calc_02, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p6 <- train %>% ggplot(aes(ps_calc_03, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,6), 2,3, byrow = T)
multiplot(p1, p2, p3, p4, p5, p6, layout = layout)

p1 <- train %>% ggplot(aes(ps_car_12, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p2 <- train %>% ggplot(aes(ps_car_13, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p3 <- train %>% ggplot(aes(ps_car_14, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p4 <- train %>% ggplot(aes(ps_car_15, fill = target)) + 
  geom_density(alpha = 0.5, bw = 0.5) +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4),2,2, byrow =T)
multiplot(p1,p2,p3,p4, layout = layout)

# Multi-features comparisons
train %>% select(-starts_with("ps_calc"), -ps_ind_10_bin, -ps_ind_11_bin, -ps_car_10_cat, -id) %>%
  mutate_at(vars(ends_with("cat")), funs(as.integer)) %>% 
  mutate_at(vars(ends_with("bin")), funs(as.integer)) %>%
  mutate(target = as.integer(target)) %>% 
  cor(use="complete.obs", method = "spearman") %>%
  corrplot(type = "lower", tl.col = "black", diag = F)

# highly correlated features
train %>% select(ps_ind_12_bin, ps_ind_14, ps_ind_16_bin, ps_ind_17_bin, ps_ind_18_bin, ps_reg_02, ps_reg_03,
                 ps_car_12, ps_car_13, ps_car_14, ps_car_15, ps_car_02_cat, ps_car_04_cat) %>%
  mutate_at(vars(ends_with("cat")), funs(as.integer)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.integer)) %>%
  cor(use = "complete.obs", method = "spearman") %>%
  corrplot(type = "lower", tl.col = "black", diag = F, method = "number") # diag = F : 대각행렬 제외

# Alluvial diagram for key features
allu_train <- train %>% filter(!is.na(ps_car_02_cat)) %>%
  filter(ps_car_04_cat %in% c("0","1","2","8","9")) %>%
  group_by(target, ps_ind_17_bin, ps_ind_18_bin, ps_car_02_cat, ps_car_04_cat) %>%
  count() %>%
  ungroup

# 표의 넓이가 넓을수록 빈도수가 많다는 뜻.
alluvial(allu_train %>% select(-n), freq = allu_train$n, border = NA,
         col = ifelse(allu_train$target == 0, "red", "blue"),
         cex = 0.75, hide = allu_train$n < 200,
         ordering = list(order(allu_train$target == 1),
                         NULL,
                         NULL,
                         NULL,
                         NULL))

# Pairwise relationship
p1 <- train %>% ggplot(aes(ps_ind_14, fill = ps_ind_12_bin)) +
  geom_bar(position = "fill")

p2 <- train %>% ggplot(aes(ps_ind_16_bin, ps_ind_18_bin)) +
  geom_count(color = "orange")

p3 <- train %>% ggplot(aes(ps_ind_16_bin, ps_ind_17_bin)) +
  geom_count(color = "orange")

p4 <- train %>% ggplot(aes(ps_reg_02, ps_reg_03)) +
  geom_point() +
  geom_smooth(method = "gam", color = "dark green") # gam : 추세선 그리는 방

p5 <- train %>% ggplot(aes(ps_car_12, ps_car_13)) +
  geom_point() +
  geom_smooth(method = "gam", color = "red")

p6 <- train %>% ggplot(aes(ps_car_12, ps_car_14)) +
  geom_point() +
  geom_smooth(method = "gam", color = "red")

layout <- matrix(c(1,2,3,4,5,6),2,3, byrow = T)
multiplot(p1,p2,p3,p4,p5,p6, layout = layout)

# Interactive multi-dimensional relations
set.seed(4321)
train %>% filter(!is.na(ps_car_12) & !is.na(ps_car_13) & !is.na(ps_car_14)) %>%
  select(ps_car_12, ps_car_13, ps_car_14, target) %>% 
  sample_n(5e4) %>%
  plot_ly(x = ~ps_car_12, y = ~ps_car_13, z = ~ps_car_14, color = ~target,
          colors = c('#BF382A', '#0C4B8E'), text = ~paste('ps_car_12:', ps_car_12,
                                                          '<br>ps_car_13:', ps_car_13,
                                                          '<br>ps_car_14:', ps_car_14,
                                                          '<br>Target:', target)) %>%
  add_markers() %>%
  layout(title = "'Car' group correlations and target impact",
         scene = list(xaxis = list(title = 'ps_car_12'),
                      yaxis = list(title = 'ps_car_13'),
                      zaxis = list(title = 'ps_car_14')))

train %>% ggplot(aes(ps_car_14, reorder(ps_ind_14, -ps_car_14, FUN = median), fill = as.factor(ps_ind_14))) +
  geom_density_ridges() + labs(y = "ps_ind_14") +
  theme(legend.position = "none", plot.title = element_text(size = 11)) + 
  facet_grid(ps_ind_16_bin ~ target) + 
  ggtitle("Target (left/right) vs ps_ind_16_bin (top/bottom) for ps_car_14 (x) vs ps_ind_14 (y, col)")

p1 <- train %>% ggplot(aes(ps_car_14, reorder(ps_car_11, -ps_car_14, FUN = median), fill = as.factor(ps_car_11))) +
  geom_density_ridges() +
  labs(y = "ps_car_11") + theme(legend.position = "none")

p2 <- train %>% ggplot(aes(ps_car_14, reorder(ps_ind_01, -ps_car_14, FUN = median), fill = as.factor(ps_ind_01))) +
  geom_density_ridges() + labs(y = "ps_ind_01") +
  theme(legend.position = "none")

p3 <- train %>% ggplot(aes(ps_car_14, reorder(ps_ind_03, -ps_car_14, FUN = median), fill = as.factor(ps_ind_03))) +
  geom_density_ridges() + labs(y = "ps_ind_03") +
  theme(legend.position = "none")

p4 <- train %>% ggplot(aes(ps_car_14, reorder(ps_ind_15, -ps_car_14, FUN = median), fill = as.factor(ps_ind_15))) +
  geom_density_ridges() + labs(y = "ps_ind_15") +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4), 2,2, byrow = T)
multiplot(p1,p2,p3,p4, layout = layout)

# number of NAs
nano <- combine %>% is.na() %>% rowSums() %>% as.integer()

# Sum up "ind" binary columns
bin_ind <- combine %>% select(ends_with("bin")) %>% select(starts_with("ps_ind")) %>%
  rowSums() %>% as.integer()

# Sum up "calc" binary columns
bin_calc <- combine %>% select(ends_with("bin")) %>% select(starts_with("ps_calc")) %>%
  rowSums() %>% as.integer()

bins <- combine %>% select(ends_with("bin")) %>%
  select(starts_with("ps_ind")) %>% mutate_all(funs(as.integer))

ref_bin <- bins %>% summarise_all(median, na.rm = T)
ref_bin <- ref_bin[rep(1, nrow(combine)),]
diff_ind <- rowSums(abs(bins - ref_bin))

bins <- combine %>% select(ends_with("bin")) %>%
  select(starts_with("ps_calc")) %>% mutate_all(funs(as.integer))

ref_bin <- bins %>% summarise_all(median, na.rm = T)
ref_bin <- ref_bin[rep(1, nrow(combine)),]
diff_calc <- rowSums(abs(bins - ref_bin))

# Apply changes to combine frame
combine <- combine %>% mutate(nano = nano, bin_ind = bin_ind, bin_calc = bin_calc,
                              diff_ind = diff_ind, diff_calc = diff_calc)

# Split into train vs test
train <- combine %>% filter(dset == "train")
test <- combine %>% filter(dset == "test")

p1 <- train %>% ggplot(aes(nano, fill = as.factor(nano))) +
  geom_bar() + scale_y_log10() + theme(legend.position = "none") +
  labs(x = "number of NAs")

p2 <- train %>% group_by(nano, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ungroup() %>%
  ggplot(aes(nano, frac_claim)) + geom_point(color = "red") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "Number of NAs", y = "Claims [%]") +
  facet_zoom(x = nano < 5, y = frac_claim < 10)

layout <- matrix(c(1,2), 2,1, byrow = T)
multiplot(p1,p2, layout = layout)

p1 <- train %>% ggplot(aes(bin_ind, fill = as.factor(bin_ind))) +
  geom_bar() + scale_y_log10() +
  theme(legend.position = "none") + labs(x = "Sum of binary 'ind' features")

p2 <- train %>% filter(bin_ind < 6) %>%
  group_by(bin_ind, target) %>% count() %>% spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ungroup() %>%
  ggplot(aes(bin_ind, frac_claim)) +
  geom_point(color = "orange") + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "Sum of binary 'ind' features ", y = "Claims [%]")

p3 <- train %>% ggplot(aes(bin_calc, fill = as.factor(bin_calc))) +
  geom_bar() + scale_y_log10() + theme(legend.position = "none") +
  labs(x = "Sum of binary 'calc' features")

p4 <- train %>% filter(bin_calc < 6) %>% group_by(bin_calc, target) %>%
  count() %>% spread(target, n) %>% 
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ungroup() %>%
  ggplot(aes(bin_calc, frac_claim)) + geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") + labs(x = "Sum of binary 'calc' features", y = "Claims [%]")

layout <- matrix(c(1,2,3,4), 2,2, byrow = T)
multiplot(p1,p2,p3,p4,layout = layout)

p1 <- train %>% ggplot(aes(as.factor(diff_ind), fill = as.factor(diff_ind))) +
  geom_bar() + scale_y_sqrt() + 
  labs(fill = "diff_ind", x = "Absolute difference of binary 'ind' features")

p2 <- train %>% ggplot(aes(as.factor(diff_calc), fill = as.factor(diff_calc))) +
  geom_bar() + scale_y_sqrt() +
  labs(fill = "diff_calc", x = "Absolute difference of binary 'calc' features")

p3 <- train %>% filter(diff_ind < 7) %>% group_by(diff_ind, target) %>%
  count() %>% spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ungroup() %>%
  ggplot(aes(diff_ind, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "Absolute difference of binary 'ind' features", y = "Claims [%]")

p4 <- train %>% filter(diff_calc < 6) %>% group_by(diff_calc, target) %>%
  count() %>% spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100) %>%
  ungroup() %>%
  ggplot(aes(diff_calc, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "Absolute difference of binary 'calc' features", y = "Claims [%]")

layout <- matrix(c(1,2,3,4),2,2, byrow = T)
multiplot(p1,p2,p3,p4,layout = layout)

# Modeling preparation
# Train vs test Compairson
bin_col <- combine %>% select(ends_with("bin")) %>%
  colnames() %>% unlist(use.names = F) %>% as.character() # use.naems = F : 벡터, 리스트 등에서의 이름을 보전하여 반환할지 여부 설정

cat_col <- combine %>% select(ends_with("cat")) %>%
  colnames() %>% unlist(use.names = F) %>% as.character()

train_frac_bin <- NULL
for (i in bin_col){
  foo <- combine %>%
    group_by(!!sym(i), dset) %>%
    count() %>%
    spread(dset, n, fill = 0) %>%
    mutate(frac_train = train / (train+test)*100,
           lwr = get_binCI(train, (train+test))[[1]]*100,
           upr = get_binCI(train, (train+test))[[2]]*100)
  train_frac_bin <- tibble(name = i,
                           value = c(FALSE, TRUE),
                           tfrac = c(foo$frac_train),
                           lwr = c(foo$lwr),
                           upr = c(foo$upr)) %>%
    bind_rows(train_frac_bin)
}

plot_cat_train_test <- function(col){
  col <- enquo(col)
  combine %>%
    group_by(!!col, dset) %>%
    count() %>%
    spread(dset, n, fill = 0) %>%
    mutate(frac_train = train/(train+test)*100,
           lwr = get_binCI(train,(train+test))[[1]]*100,
           upr = get_binCI(train,(train+test))[[2]]*100,
           col = !!col
    ) %>%
    ggplot(aes(col, frac_train)) +
    geom_point() +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7) +
    labs(x = as.character(col)[2], y = "")
}

p1 <- combine %>% ggplot(aes(id, fill = dset)) +
  geom_density(bw = 1, alpha = 0.5) +
  coord_cartesian(ylim = c(7.3e-4, 8.3e-4))

p2 <- train_frac_bin %>% ggplot(aes(name, tfrac, color = value)) +
  geom_point() + geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7) +
  labs(x = "Binary 'ind' features", y = "Training set percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.9))
  
layout <- matrix(c(1,2), 2,1, byrow = T)
multiplot(p1,p2,layout = layout)

# str_c("p", seq(1, length(cat_col)), " <- plot_cat_train_test(", cat_col)
p1 <- plot_cat_train_test(ps_ind_02_cat)
p2 <- plot_cat_train_test(ps_ind_04_cat)
p3 <- plot_cat_train_test(ps_ind_05_cat)
p4 <- plot_cat_train_test(ps_car_01_cat)
p5 <- plot_cat_train_test(ps_car_02_cat)
p6 <- plot_cat_train_test(ps_car_03_cat)
p7 <- plot_cat_train_test(ps_car_04_cat)
p8 <- plot_cat_train_test(ps_car_05_cat)
p9 <- plot_cat_train_test(ps_car_06_cat)
p10 <- plot_cat_train_test(ps_car_07_cat)
p11 <- plot_cat_train_test(ps_car_08_cat)
p12 <- plot_cat_train_test(ps_car_09_cat)
p13 <- plot_cat_train_test(ps_car_10_cat)
p14 <- plot_cat_train_test(ps_car_11_cat)

layout <- matrix(seq(1,14), 2,7, byrow = T)
multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,layout = layout)

p1 <- combine %>% ggplot(aes(ps_reg_01, fill = dset)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p2 <- combine %>% ggplot(aes(ps_reg_02, fill = dset)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p3 <- combine %>% ggplot(aes(ps_reg_03, fill = dset)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p4 <- combine %>% ggplot(aes(ps_calc_01, fill = dset)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p5 <- combine %>% ggplot(aes(ps_calc_02, fill = dset)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p6 <- combine %>% ggplot(aes(ps_calc_03, fill = dset)) +
  geom_density(alpha = 0.5, bw = 0.05)

layout <- matrix(c(1,2,3,4,5,6), 2,3 , byrow = T)
multiplot(p1, p2, p3, p4, p5, p6, layout = layout)

# Feature selection, evaluation metric, and validation split
# Feature formatting
combine <- combine %>% mutate_at(vars(ends_with("cat")), funs(as.integer)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.integer)) %>%
  mutate(target = as.integer(levels(target))[target])

# specific definitions:
# predictor features

ind_cols <- c("ps_ind_01","ps_ind_02_cat","ps_ind_03","ps_ind_04_cat","ps_ind_05_cat","ps_ind_06_bin","ps_ind_07_bin","ps_ind_08_bin",
              "ps_ind_09_bin","ps_ind_10_bin","ps_ind_11_bin","ps_ind_12_bin","ps_ind_13_bin","ps_ind_14","ps_ind_15","ps_ind_16_bin",
              "ps_ind_17_bin","ps_ind_18_bin")

reg_cols <- c("ps_reg_01", "ps_reg_02", "ps_reg_03")

car_cols <- c("ps_car_01_cat","ps_car_02_cat","ps_car_03_cat","ps_car_04_cat","ps_car_05_cat","ps_car_06_cat","ps_car_07_cat","ps_car_08_cat",
              "ps_car_09_cat","ps_car_10_cat","ps_car_11_cat","ps_car_11","ps_car_12","ps_car_13","ps_car_14","ps_car_15")

calc_cols <- c("ps_calc_01","ps_calc_02","ps_calc_03","ps_calc_04","ps_calc_05","ps_calc_06","ps_calc_07","ps_calc_08","ps_calc_09",
               "ps_calc_10","ps_calc_11","ps_calc_12","ps_calc_13","ps_calc_14","ps_calc_15_bin","ps_calc_16_bin","ps_calc_17_bin",
               "ps_calc_18_bin","ps_calc_19_bin","ps_calc_20_bin")

eng_cols <- c("nano", "bin_ind", "bin_calc", "diff_ind", "diff_calc")

train_cols <- c(ind_cols, reg_cols, car_cols, eng_cols)

# target feature
y_col <- c("target")

# identification feature
id_col <- c("id")

# auxilliary features
aux_cols <- c("dset")

test_id <- combine %>% filter(dset == "test") %>%
  select(!!sym(id_col))

cols <- c(train_cols, y_col, aux_cols)
combine <- combine %>% select_(.dots = cols)

train <- combine %>% filter(dset == "train") %>%
  select_(.dots = str_c("-", c(aux_cols)))

test <- combine %>% filter(dset == "test") %>%
  select_(.dots = str_c("-", c(aux_cols)))

xgb_normalizedgini <- function(preds, dtrain){
  actual <- getinfo(dtrain, "label")
  score <- NormalizedGini(preds, actual)
  return(list(metric = "NormalizedGini", value = score))
}

set.seed(4321)
trainIndex <- createDataPartition(train$target, p = 0.8, list = FALSE, times = 1)

train <- train[trainIndex,]
valid <- train[-trainIndex,]

# XGBoost parameters and fitting
# convert to XGB matrix
foo <- train %>% select(-target)
bar <- valid %>% select(-target)

dtrain <- xgb.DMatrix(as.matrix(foo), label = train$target)
dvalid <- xgb.DMatrix(as.matrix(bar), label = valid$target)
dtest <- xgb.DMatrix(as.matrix(test))

xgb_params <- list(colsample_bytree = 0.7, # variables per tree
                   subsample = 0.7, # data subset per tree
                   booster = "gbtree", 
                   max_depth = 5, # tree levels
                   eta = 0.3, # shrinkage
                   eval_metric = xgb_normalizedgini,
                   objective = "reg:logistic",
                   seed = 4321,
                   nthread = -1)

watchlist <- list(train = dtrain, valid = dvalid)

set.seed(1234)
xgb_cv <- xgb.cv(xgb_params,dtrain,early_stopping_rounds = 5, nfold = 5, nrounds=50, maximize = TRUE)

set.seed(4321)
gb_dt <- xgb.train(params = xgb_params,
                   data = dtrain,
                   print_every_n = 5,
                   watchlist = watchlist,
                   nrounds = xgb_cv$best_iteration)

# Feature importance
imp_matrix <- as.tibble(xgb.importance(feature_names = colnames(train %>% select(-target)), model = gb_dt))

imp_matrix <- ggplot(aes(reorder(Feature, Gain, FUN = max), Gain, fill = Feature)) +
  geom_col() + coord_flip() + theme(legend.position = "none") + labs(x = "Features", y = "Importance")

pred <- test_id %>% mutate(target = predict(gb_dt, dtest))
pred %>% write.csv('submit.csv')

identical(dim(sample_submit), dim(pred))
glimpse(sample_submit)
glimpse(pred)
bind_cols(sample_submit, pred) %>% head(5)




































































































