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



























































































