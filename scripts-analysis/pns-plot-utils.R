library(dplyr)
library(magrittr)
library(assertthat)
library(testthat)
library(ggplot2)
library(grid)
library(gridExtra)

SampleAndRename <- function(df, use.seed, samp.size){
  # About: chooses a sample of participant ID's of size samp.size
  #   and provide new participant ID's beginning and ending 
  #   at 1 and `use.samp.size`, respectively
  # Args:
  #   df: a data frame with a column for participant IDs
  #   use.seed: seed to be specified in set.seed(use.seed)
  #   samp.size: number of individuals to sample
  # Output:
  #   a data frame with one column for the original ID
  #   and another column with a new ID which
  #   can be used for plotting
  
  set.seed(use.seed)
  ids <- unique(df$id)
  sample.ids <- sample(ids, size=samp.size)
  df.plot <- df %>% filter(id %in% sample.ids)
  
  df.ids <- data.frame(new.id = NA, id = sample.ids)
  df.ids$new.id <- 1:nrow(df.ids)
  return(df.ids)
}

PlotSmokingOutcome <- function(df.smoking, df.ids){
  # About: plots beginning and end time of smoking intervals
  #   and colors smoking interval according to whether
  #   it is labelled as YES, NO, UNKNOWN
  # Args:
  #   df.smoking: a data frame with a columns for beginning and end times
  #     of smoking intervals, and labels YES, NO, UNKNOWN; this
  #     data frame can contain rows from more than 1 participant
  #   df.ids: data frame which is an output of the SampleAndRename() function;
  #     the rows of df.ids only belong to the subset of participants in the PNS
  #     study whose data we wish to visualize
  # Output:
  #   a ggplot2 object that can be used to display the plot
  
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
  # About: plots time when Post-Quit EMA (any type) was either delivered
  #   (for EMAs with engaged.yes=0) or time when participant began completion
  #   (for EMAs with engaged.yes=1)
  # Args:
  #   df.post.quit: any curated post-quit dataset
  #   df.ids: data frame which is an output of the SampleAndRename() function;
  #     the rows of df.ids only belong to the subset of participants in the PNS
  #     study whose data we wish to visualize
  #   plot.days: number of days of data to use for plotting;
  #     set plot.days=22 to plot the entire Post-Quit period for the PNS study;
  #     set plot.days=3 to zoom into the first 3 days Post-Quit period
  #   ema.type: a string describing the type of EMA; this will be
  #     used to provide titles to plots; valid inputs are 
  #     "random" for Post-Quit Random EMA
  #     "urge" for Post-Quit Urge EMA
  # Output:
  #   a ggplot2 object that can be used to display the plot;
  #   display multiple participants' data in a single plot
  
  assert_that(ema.type %in% c("random", "urge"), msg = "valid ema.type must be used")
  
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
  cols <- c("0" = "red", "1" = "blue")
  gg.all <- gg.all + scale_colour_manual(values = cols)
  
  gg.all <- gg.all + labs(x = "No. of Days since 12AM on Quit Day")
  gg.all <- gg.all + labs(y=paste("Each row is one participant's data (sample size=",samp.size,")", sep=""))
  gg.all <- gg.all + scale_x_continuous(breaks = 0:21, limits = c(0,plot.days))
  gg.all <- gg.all + scale_y_continuous(breaks = 1:samp.size)
  
  # Add title and legend
  if(ema.type=="random"){
    gg.all <- gg.all + theme(legend.position = "bottom")
    use.title <- "Time of EMA delivery (if engaged.yes=0) or time when participant began completion\nof EMA (if engaged.yes=1) within 21-Day Post Quit Period"
    use.subtitle <- paste("Shaded area denotes time between 10PM - 8AM \nEach point denotes one post-quit ",ema.type," EMA",sep="")
  }else{
    gg.all <- gg.all + theme(legend.position = "none")
    use.title <- "Time when participant began completion of EMA within 21-Day Post Quit Period"
    use.subtitle <- paste("Shaded area denotes time between 10PM - 8AM \nEach point denotes one post-quit ",ema.type," EMA",sep="")
  }
  gg.all <- gg.all + labs(title = use.title)
  gg.all <- gg.all + labs(subtitle = use.subtitle)
  
  return(gg.all)
  
}

PlotPostQuitNumericResponses <- function(df.post.quit, var.name, df.ids){
  # About: plots time when Post-Quit EMA (any type) was either delivered
  #   (for EMAs with engaged.yes=0) or time when participant began completion
  #   (for EMAs with engaged.yes=1), and in addition, responses to the EMA item
  #   var.name
  # Args:
  #   df.post.quit: any curated post-quit dataset
  #   var.name: column name in df.post.quit of variable we wish to plot
  #   df.ids: data frame which is an output of the SampleAndRename() function;
  #     the rows of df.ids only belong to the subset of participants in the PNS
  #     study whose data we wish to visualize
  # Output:
  #   a ggplot2 object that can be used to display the plot;
  #   display only one participant's data in a single plot: each participant
  #   will have their own ggplot2 object; output of this function can the be used
  #   with the function gridExtra::arrangeGrob() to display plots arranged in a
  #   grid on a single page.
  
  samp.size <- nrow(df.ids)
  ids <- df.ids$new.id
  df.plot.post.quit <- left_join(x = df.ids, y = df.post.quit, by = "id")
  df.plot.post.quit <- df.plot.post.quit %>% select(-id) %>% rename(id = new.id)
  
  df.plot.post.quit <- df.plot.post.quit %>%
    mutate(t = time.unixts.scaled/(60*60*24) + 4/24) %>%  # t is 12AM on Quit Day
    mutate(newvar = if_else(engaged.yes==0, as.integer(0), .data[[var.name]])) %>% # For plotting
    mutate(engaged.yes = as.factor(engaged.yes)) %>%
    select(id, t, engaged.yes, newvar)
  
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
    cols <- c("0" = "red", "1" = "blue")
    gg.all <- gg.all + scale_colour_manual(values = cols)
    #gg.all <- gg.all + labs(x = "No. of Days since 12AM on Quit Day")
    #gg.all <- gg.all + labs(y="Response to EMA item")
    gg.all <- gg.all + labs(x="", y="")
    gg.all <- gg.all + scale_y_continuous(breaks = c(1,2,3,4,5), labels = c("1","2","3","4","5"))
    gg.all <- gg.all + theme(legend.position = "None")
    gg.all <- gg.all + labs(title = paste("Participant",use.this.id,sep=" ")) + theme(plot.title = element_text(size = 8, face = "bold"))
    
    # Plot all days
    collect.plots <- append(collect.plots, list(gg.all))
  }
  
  return(collect.plots)
}

