library(dplyr)
library(magrittr)
library(assertthat)
library(testthat)
library(ggplot2)
library(grid)
library(gridExtra)

SampleAndRename <- function(df, use.seed, samp.size){
  
  set.seed(use.seed)
  ids <- unique(df$id)
  sample.ids <- sample(ids, size=samp.size)
  df.plot <- df %>% filter(id %in% sample.ids)
  
  df.ids <- data.frame(new.id = NA, id = sample.ids)
  df.ids$new.id <- 1:nrow(df.ids)
  return(df.ids)
}

PlotSmokingOutcome <- function(df.smoking, df.ids){
  samp.size <- nrow(df.ids)
  df.plot.smoking <- left_join(x = df.ids, y = df.smoking, by = "id")
  df.plot.smoking <- df.plot.smoking %>% select(-id) %>% rename(id = new.id)
  
  df.plot.smoking <- df.plot.smoking %>%
    mutate(LB.ts.scaled = LB.ts - start.clock) %>%
    mutate(UB.ts.scaled = UB.ts - start.clock) %>%
    mutate(t.LB = LB.ts.scaled/(60*60*24) + 4/24) %>%  # time from 12AM on Quit Day
    mutate(t.UB = UB.ts.scaled/(60*60*24) + 4/24) %>%  # time from 12AM on Quit Day
    mutate(smoking.label = as.factor(smoking.label))
  
  gg.all <- ggplot(df.plot.smoking)
  
  all.inc <- seq(1,21,1)
  for(i in 1:length(all.inc)){
    inc <- all.inc[i]
    gg.all <- gg.all + annotate("rect", xmin= -2/24 + inc, xmax=8/24 + inc, ymin=0, ymax=Inf, alpha=0.2, fill="black") 
  }
  
  cols <- c("YES" = "#03a838", "NO" = "#0374a8", "UNKNOWN" = "#2c3e50")
  gg.all <- gg.all + geom_segment(aes(x = t.LB, y = id, xend = t.UB, yend = id, colour = smoking.label), size =2)
  gg.all <- gg.all + scale_colour_manual(values = cols)
  gg.all <- gg.all + labs(x = "No. of Days since 12AM on Quit Day")
  gg.all <- gg.all + labs(y=paste("Each row is one participant's data (sample size=",samp.size,")", sep=""))
  gg.all <- gg.all + scale_x_continuous(breaks = 0:21)
  gg.all <- gg.all + scale_y_continuous(breaks = 1:samp.size)
  gg.all <- gg.all + theme(legend.position = "bottom") + labs(color = "Smoking Label")
  
  # Add title
  gg.all <- gg.all + labs(title = "Moments of time with any indication of smoking \nAll EMAs except end-of-day assessment within 21-Day Post Quit Period")
  gg.all <- gg.all + labs(subtitle = "Shaded area denotes time between 10PM - 8AM")
  
  # Plot all days
  return(gg.all)
}

PlotPostQuitEMATime <- function(df.post.quit, df.ids, plot.days, ema.type){
  
  samp.size <- nrow(df.ids)
  df.plot.post.quit <- left_join(x = df.ids, y = df.post.quit, by = "id")
  df.plot.post.quit <- df.plot.post.quit %>% select(-id) %>% rename(id = new.id)
  
  df.plot.post.quit <- df.plot.post.quit %>%
    mutate(t = time.unixts.scaled/(60*60*24) + 4/24) %>%  # t is 12AM on Quit Day
    mutate(engaged.yes = as.factor(engaged.yes))
  
  gg.all <- ggplot(df.plot.post.quit)
  
  all.inc <- seq(1,21,1)
  for(i in 1:length(all.inc)){
    inc <- all.inc[i]
    gg.all <- gg.all + annotate("rect", xmin= -2/24 + inc, xmax=8/24 + inc, ymin=0, ymax=Inf, alpha=0.2, fill="black") 
  }
  
  gg.all <- gg.all + geom_point(aes(t, id, color=engaged.yes), alpha=0.5)
  gg.all <- gg.all + labs(x = "No. of Days since 12AM on Quit Day")
  gg.all <- gg.all + labs(y=paste("Each row is one participant's data (sample size=",samp.size,")", sep=""))
  gg.all <- gg.all + scale_x_continuous(breaks = 0:21, limits = c(0,plot.days))
  gg.all <- gg.all + scale_y_continuous(breaks = 1:samp.size)
  gg.all <- gg.all + theme(legend.position = "bottom")
  
  # Add title
  gg.all <- gg.all + labs(title = "Time of EMA delivery of EMAs within 21-Day Post Quit Period")
  gg.all <- gg.all + labs(subtitle = paste("Shaded area denotes time between 10PM - 8AM \nEach point denotes one post-quit ",ema.type," EMA",sep=""))
  
  return(gg.all)
  
}

PlotPostQuitNumericResponses <- function(df.post.quit, var.name, df.ids){
  
  samp.size <- nrow(df.ids)
  ids <- df.ids$new.id
  df.plot.post.quit <- left_join(x = df.ids, y = df.post.quit, by = "id")
  df.plot.post.quit <- df.plot.post.quit %>% select(-id) %>% rename(id = new.id)
  
  df.plot.post.quit <- df.plot.post.quit %>%
    mutate(t = time.unixts.scaled/(60*60*24) + 4/24) %>%  # t is 12AM on Quit Day
    mutate(newvar = if_else(engaged.yes==0, as.integer(0), .data[[var.name]])) %>% # For plotting
    mutate(engaged.yes = as.factor(engaged.yes)) %>%
    select(id, t, engaged.yes, newvar)
  
  cols <- c("0" = "red", "1" = "blue")
  
  collect.plots <- list()
  for(i in 1:length(ids)){
    use.this.id <- ids[i]
    df.plot.this.participant <- df.plot.post.quit %>% filter(id==use.this.id)
    gg.all <- ggplot(df.plot.this.participant)
    
    all.inc <- seq(1,21,1)
    for(i in 1:length(all.inc)){
      inc <- all.inc[i]
      gg.all <- gg.all + annotate("rect", xmin= -2/24 + inc, xmax=8/24 + inc, ymin=0, ymax=5.2, alpha=0.2, fill="black") 
    }
    
    gg.all <- gg.all + geom_point(aes(t, newvar, color=engaged.yes), alpha=0.5)
    gg.all <- gg.all + scale_colour_manual(values = cols)
    #gg.all <- gg.all + labs(x = "No. of Days since 12AM on Quit Day")
    #gg.all <- gg.all + labs(y="Response to EMA item")
    gg.all <- gg.all + labs(x="", y="")
    gg.all <- gg.all + scale_y_continuous(breaks = c(1,2,3,4,5), labels = c("1","2","3","4","5"))
    gg.all <- gg.all + theme(legend.position = "None")
    
    # Plot all days
    collect.plots <- append(collect.plots, list(gg.all))
  }
  
  return(collect.plots)
}

