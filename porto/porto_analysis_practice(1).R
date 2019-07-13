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
  geom_col() + geom_errorbar(aes(ylim = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") + labs(y = "Claims [%]")

p4 <- train %>% group_by(ps_ind_09_bin, target) %>% 
  count() %>% spread(target, n) %>% 
  mutate(frac_claim = `1` / (`1` + `0`)*100,
         lwr = get_binCI(`1`, (`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_09_bin, frac_claim, fill = ps_09_bin)) + 
  geom_col() + geom_errorbar(aes(ylim = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") + labs(y = "Claims [%]")

p5 <- train %>% group_by(ps_ind_10_bin, target) %>%
  count() %>% spread(target, n) %>%
  mutate(frac_claim = `1` / (`1` + `0`)*100,
         lwr = get_binCI(`1`, (`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`, (`1`+`0`))[[2]]*100) %>%
  ggplot(aes(ps_ind_10_bin, frac_claim, fill = ps_ind_10_bin)) +
  geom_col() + geom_errorbar(aes(ylim = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
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

























































































