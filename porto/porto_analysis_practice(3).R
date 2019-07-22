# porto_analysis_practice(3)

# set up
library(readr)
library(data.table)
library(reshape)
library(ggplot2)
library(gridExtra)
options(repr.plot.width = 6, repr.plot.height = 3)
df <- read_csv("C:\\Users\\HOJIN\\Desktop\\Kaggle Data\\porto\\train.csv")
class(df) <- "data.frame"

### Plotting functions
plot_one_way <- function(df,lines, bars = NULL, factor){
  ## If bars not given, do count
  if (is.null(bars)){
    bars <- "Count"
    df$Count <- 1
  }
  
  ## Check df contains lines, bars, factor
  if (prod(c(lines,bars,factor)%in%names(df))==0){
    stop("lines, bars and factor not all present in df")
  }
  
  if (!is.factor(df[,factor])){
    df[,factor] <- as.factor(df[,factor])
  }
  
  ## Weight lines by bars for weighted average
  df[,lines] <- df[,lines] * df[,bars]
  
  ## Crunch lines (1 row per factor level and line)
  df.melt <- melt(df [,c(factor,lines)],id=factor)
  df.crunch <- as.data.frame(data.table(df.melt)[,.(value=sum(value))
                                                 ,by = c(factor,"variable")])
  
  ## Crunch weight (1 row per factor level)
  df$wt <- df[,bars]
  df.crunch.wt <- as.data.frame(data.table(df)[,.(wt=sum(wt))
                                               ,by = c(factor)])
  
  ## Join weight to line data
  df.crunch$wt <- df.crunch.wt[match(df.crunch[,factor],df.crunch.wt[,factor]),"wt"]
  
  ## Average response
  line.avg <- sum(df.crunch$value)/sum(df.crunch$wt)
  
  ## Convert value to average
  df.crunch$value <- df.crunch$value/df.crunch$wt
  
  ## Rescale weight so that max == line.avg
  df.crunch.wt$wt_rescaled <- df.crunch.wt$wt * line.avg / max(df.crunch.wt$wt)
  
  ## Plot a chart
  plot.one_way<-ggplot(df.crunch.wt, aes_string(x=factor,y="wt_rescaled",group=1))+
    geom_bar(stat="identity",fill="yellow",colour="white",alpha=0.3)+
    geom_line(data=df.crunch,aes_string(x=factor,y="value",colour="variable",group="variable"))+
    geom_point(data=df.crunch,aes_string(x=factor,y="value",colour="variable",group="variable"))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    labs(y = "value" )+ggtitle(factor)
  
  return(plot.one_way)
}

plot_two_way <- function(df,line, bars = NULL, factor1, factor2, rescale = F){
  ## If bars not given, do count
  if (is.null(bars)){
    bars <- "Count"
    df$Count <- 1
  }
  
  ## Check df contains lines, bars, factor
  if (prod(c(line,bars,factor1,factor2)%in%names(df))==0){
    stop("lines, bars and factor not all present in df")
  }
  
  if (!is.factor(df[,factor1])){
    df[,factor1] <- as.factor(df[,factor1])
  }
  
  if (!is.factor(df[,factor2])){
    df[,factor2] <- as.factor(df[,factor2])
  }
  
  ## Weight lines by bars for weighted average
  df[,line] <- df[,line] * df[,bars]
  
  ## Crunch data
  df$wt <- df[,bars]
  df$value <- df[,line]
  df.crunch <- as.data.frame(data.table(df)[,.(value=sum(value),
                                               wt=sum(wt))
                                            ,by = c(factor1,factor2)])
  
  ## Crunch weight 1-way
  df.crunch.1way <- as.data.frame(data.table(df)[,.(wt=sum(wt))
                                                 ,by = c(factor1)])
  
  ## Convert value to average
  df.crunch$value <- df.crunch$value/df.crunch$wt
  
  ## Rescale
  if (rescale){
    df.crunch$lp <- ifelse(df.crunch$value%in%c(0,1),0,log(df.crunch$value/(1-df.crunch$value)))
    lm.noint <- lm(as.formula(paste("lp~",factor1,"+",factor2)),data = df.crunch,weights = df.crunch$wt)
    df.crunch$lp_noint <- predict(lm.noint,newdata = df.crunch)
    df.crunch$lp_int <- df.crunch$lp - df.crunch$lp_noint
    df.crunch$value <- exp(df.crunch$lp_int)/(1+exp(df.crunch$lp_int))
  }
  
  ## Average response
  line.avg <- sum(df.crunch$value*df.crunch$wt)/sum(df.crunch$wt)
  
  ## Rescale weight so that max == line.avg
  df.crunch$wt_rescaled <- df.crunch$wt * line.avg / max(df.crunch.1way$wt)
  
  df.crunch <- df.crunch[order(df.crunch[,factor1],df.crunch[,factor2]),]
  
  chart.title <- paste(factor1,"by",factor2)
  if (rescale){
    chart.title <- paste(chart.title,"(rescaled)")
  }
  
  ## Plot a chart
  plot.two_way<-ggplot(df.crunch)+
    geom_bar(stat="identity", aes_string(x=factor1,y="wt_rescaled",fill=factor2,group=factor2),col="white",alpha=0.3)+
    geom_line(aes_string(x=factor1,y="value",colour=factor2,group=factor2))+
    geom_point(aes_string(x=factor1,y="value",colour=factor2,group=factor2))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    labs(y = "value" ) + ggtitle(chart.title)
  
  return(plot.two_way)
}

# ps_ind_03
# 변수의 속성값에 따른 보험 청구 비율 비율

plot_one_way(df = df, lines = "target", factor = "ps_ind_03")
plot_one_way(df = df, lines = "target", factor = "ps_ind_02_cat")
df$ps_ind_02_cat.001 <- 1 * (df$ps_ind_02_cat == 1)
plot_two_way(df = df, line = "target", factor1 = "ps_ind_03", factor2 = "ps_ind_02_cat.001")
plot_two_way(df = df, line = "target", factor1 = "ps_ind_03", factor2 = "ps_ind_02_cat.001", rescale = T)
df$ps_ind_0203 <- ifelse(df$ps_ind_02_cat == 1, 23 - df$ps_ind_03, df$ps_ind_03)
plot_one_way(df = df, lines = "target", factor = "ps_ind_0203")

df$ps_ind_06070809 <- paste(df$ps_ind_06_bin, df$ps_ind_07_bin, df$ps_ind_08_bin, df$ps_ind_09_bin, sep = "")
plot_one_way(df = df, lines = "target", factor = "ps_ind_06070809")

plot_two_way(df = df, line = "target", factor1 = "ps_ind_03", factor2 = "ps_ind_06070809")
plot_two_way(df = df, line = "target", factor1 = "ps_ind_03", factor2 = "ps_ind_06070809", rescale = T)

df$ps_ind_0708 <- df$ps_ind_07_bin + df$ps_ind_08_bin
plot_two_way(df = df, line = "target", factor1 = "ps_ind_03", factor2 = "ps_ind_0708")
df$ps_ind_030708 <- ifelse(df$ps_ind_0708 == 1, df$ps_ind_03, -1)
df$ps_ind_030609 <- ifelse(df$ps_ind_0708 == 0, df$ps_ind_03, -1)
plot_one_way(df = df, lines = "target", factor = "ps_ind_030708")
plot_one_way(df = df, lines = "target", factor = "ps_ind_030609")

plot_one_way(df = df, lines = "target", factor = "ps_ind_01")

df$ps_ind_01_grp <- cut(df$ps_ind_01, breaks = c(-Inf, 2, 4, 7)) # Inf : Infinite(무한대)
plot_two_way(df = df, line = "target", factor1 = "ps_ind_03", factor2 = "ps_ind_01_grp")

df$ps_ind_05_cat.000 <- 1*(df$ps_ind_05_cat == 0)
plot_two_way(df = df, line = "target", factor1 = "ps_ind_0708", factor2 = "ps_ind_05_cat.000")
plot_two_way(df = df, line = "target", factor1 = "ps_ind_0708", factor2 = "ps_ind_05_cat.000", rescale = T)
df$ps_ind_050708 <- df$ps_ind_0708 + 2*(1-df$ps_ind_05_cat.000)
plot_one_way(df = df, lines = "target", factor = "ps_ind_050708")

plot_one_way(df = df, lines = "target", factor = "ps_ind_05_cat")
plot_two_way(df = df, line = "target", factor1 = "ps_ind_17_bin", factor2 = "ps_ind_05_cat.000")
plot_two_way(df = df, line = "target", factor1 = "ps_ind_17_bin", factor2 = "ps_ind_05_cat.000", rescale = T)

df$ps_ind_04_cat.001 <- 1*(df$ps_ind_04_cat == 1)
plot_two_way(df = df, line = "target", factor1 = "ps_ind_04_cat.001", factor2 = "ps_ind_0708")
df$ps_ind_040708 <- df$ps_ind_04_cat + 3*df$ps_ind_0708 - 2*(df$ps_ind_04_cat.001 * df$ps_ind_0708) # 뒤의 식 값은 0or1 의 값만 가진다.
plot_one_way(df = df, lines = "target", factor = "ps_ind_040708")

plot_one_way(df = df, lines = "target", factor = "ps_ind_10_bin")
plot_one_way(df = df, lines = "target", factor = "ps_ind_11_bin")
plot_one_way(df = df, lines = "target", factor = "ps_ind_12_bin")
plot_one_way(df = df, lines = "target", factor = "ps_ind_13_bin")

df$ps_ind_10111213 <- rowSums(df[,c("ps_ind_10_bin", "ps_ind_11_bin", "ps_ind_12_bin", "ps_ind_13_bin")])
# 아래 둘 그래프 모형은 매우 비슷해 보임.
plot_one_way(df = df, lines = "target", factor = "ps_ind_10111213")
plot_one_way(df = df, lines = "target", factor = "ps_ind_14")

plot_two_way(df = df, line = "target", factor1 = "ps_ind_14", factor2 = "ps_ind_10111213") # 그림 그려보니 ps_ind_14 는 정말 4개 변수의 합이였다.

plot_one_way(df = df, lines = "target", factor = "ps_ind_16_bin")
plot_one_way(df = df, lines = "target", factor = "ps_ind_17_bin")
plot_one_way(df = df, lines = "target", factor = "ps_ind_18_bin")

df$ps_ind_161718 <- paste(df$ps_ind_16_bin, df$ps_ind_17_bin, df$ps_ind_18_bin, sep = "")
plot_one_way(df = df, lines = "target", factor = "ps_ind_161718")

plot_one_way(df = df, lines = "target", factor = "ps_ind_15")
plot_two_way(df = df, line = "target", factor1 = "ps_ind_15", factor2 = "ps_ind_161718")
plot_two_way(df = df, line = "target", factor1 = "ps_ind_15", factor2 = "ps_ind_06070809")

plot_two_way(df = df, line = "target", factor1 = "ps_ind_15", factor2 = "ps_ind_01")
plot_two_way(df = df[df$ps_ind_02_cat != -1,], line = "target", factor1 = "ps_ind_15", factor2 = "ps_ind_02_cat")
plot_two_way(df = df, line = "target", factor1 = "ps_ind_15", factor2 = "ps_ind_03")
plot_two_way(df = df[df$ps_ind_04_cat != -1,], line = "target", factor1 = "ps_ind_15", factor2 = "ps_ind_04_cat")
plot_two_way(df = df[df$ps_ind_04_cat != -1,], line = "target", factor1 = "ps_ind_15", factor2 = "ps_ind_04_cat", rescale = T)

df$ps_car_13_grp <- cut(df$ps_car_13,c(-Inf, quantile(df$ps_car_13, probs = 1:19 / 20), Inf))
plot_one_way(df = df, lines = "target", factor = "ps_car_13_grp")

plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_ind_06070809")
plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_ind_06070809", rescale = T)

plot_two_way(df = df[df$ps_car_02_cat != -1,], line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_02_cat")
plot_two_way(df = df[df$ps_car_02_cat != -1,], line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_02_cat", rescale = T)

plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_03_cat")
plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_03_cat", rescale = T)

df$ps_car_04_cat_t <- pmin(df$ps_car_04_cat, 2)
plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_04_cat_t")
plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_04_cat_t", rescale = T)

plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_05_cat")
plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_05_cat", rescale = T)

plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_07_cat")
plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_07_cat", rescale = T)

plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_08_cat")
plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_08_cat", rescale = T)

plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_09_cat")
plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_09_cat", rescale = T)

plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_03_cat")
plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_03_cat", rescale = T)

df$ps_car_11_cat_104 <- 1 * (df$ps_car_11_cat == 104)
plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_11_cat_104")
plot_two_way(df = df, line = "target", factor1 = "ps_car_13_grp", factor2 = "ps_car_11_cat_104", rescale = T)

df$ps_car_15_t <- round(df$ps_car_15^2, 4)
df$ps_car_15_t.lt14 <- 1*(df$ps_car_15_t < 14) # 14 보다 값이 작으면 1로 처리함.
plot_two_way(df = df, line = "target", factor1 = "ps_car_15_t", factor2 = "ps_car_03_cat")
plot_two_way(df = df, line = "target", factor1 = "ps_car_15_t", factor2 = "ps_car_03_cat", rescale = T)
plot_two_way(df = df, line = "target", factor1 = "ps_car_03_cat", factor2 = "ps_car_15_t.lt14")

df$ps_car_0315 <- df$ps_car_03_cat * (1+df$ps_car_15_t.lt14)
plot_one_way(df = df, lines = "target", factor = "ps_car_0315")















































































