# Libraries ####
library(ggplot2)
library(plyr)
library(reshape2)

# Analysis ####

# Load info on breaks used
break_df <- read.csv("EPIC_breaks.csv")

# Basic plotting of breaks
point_plot <- ggplot(melt(break_df, id.var="Index"), aes(x=value, y=Index)) + geom_point() + facet_wrap(facets="variable", scales="free_x", ncol=5) + xlab("Value")

# Fill in complete line showing step function used

step_ends <- function(upper, lower, score)
{
  return(data.frame(value=c(lower, upper), Index=score))
}

make_step_df <- function (breaks, index, variable){
  step_df <- data.frame(value=NA, Index=NA)[0,]  
  for (i in 1:(length(index)-1)){
    score <- index[i+1]
    lower <- breaks[i]
    upper <- breaks[i+1]
    
    step_df <- rbind(step_df, step_ends(upper, lower, score))
  }
  step_df$variable <- variable
  return(step_df)
}

step_list <- lapply(2:11, function(x){make_step_df(break_df[[x]], break_df$Index, names(break_df)[x])})
# Correct order of decreasing valuation function (soil maintenance)
step_list[[4]] <- step_list[[4]][nrow(step_list[[4]]):1, ]
step_df <- do.call(rbind, step_list)

# Plot stepped breaks
step_plot <- ggplot(step_df, aes(x=value, y=Index)) + geom_line() + facet_wrap(facets="variable", scales="free_x", ncol=5) + xlab("Value") + theme_bw()

# Saving figures ####
ggsave("break_point_plot.pdf", point_plot, width=11, height=8.5)
ggsave("break_step_plot.pdf", step_plot, width=11, height=8.5)