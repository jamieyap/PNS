if(isTRUE(zoom.part3)){
  # Zoom view of plot
  df.smoking.plotdat <- df.all %>% filter(id %in% inspect.these.participants3)
  df.smoking.plotdat <- left_join(x = df.infer.quitdate3, y = df.smoking.plotdat, by = "id")
  
  df.smoking.plotdat <- df.smoking.plotdat %>% filter(time.unixts >= as.numeric(EMA_Qday) - 7*24*60*60)
  
  use.dates <- df.infer.quitdate3
  
  for(j in 1:length(inspect.these.participants3)){
    use.id <- inspect.these.participants3[j]
    df.this.participant <- df.smoking.plotdat %>% filter(id==use.id)
    
    
    postquit.earliest <- df.this.participant %>%
      filter(grepl("Post-Quit",assessment.type)) %>% 
      select(time.unixts) %>% 
      min(.)
    
    df.this.participant <- df.this.participant %>%
      filter(time.unixts >= as.numeric(EMA_Qday) - 7*24*60*60) %>%
      mutate(diffdays.postquit.earliest = (time.unixts - as.numeric(postquit.earliest))/(3600*24)) %>%
      mutate(diffdays.postquit.earliest = round(diffdays.postquit.earliest, digits=6))
    
    jpeg(filename=file.path(path.pns.output_data, "plots_inferred_QD_part3", paste("ZOOM_Participant_", use.id-3000,".jpg",sep="")), width=600, height=600, units="px")
    use.dates[use.dates$id==use.id,"diffdays.EMA_Qday"] <- 0
    use.dates[use.dates$id==use.id,"diffdays.quitday"] <- 0
    source(file.path(path.pns.code, "construct_quit_dates", "create-smoking-plots.R"))
    dev.off()
  }
}

if(isTRUE(zoom.part4)){
  use.dates <- df.other.participants.dates
  use.dates$plotid <- 1:nrow(use.dates)
  use.dates <- use.dates %>% filter(plotid %in% c(6, 37, 92, 101))
  these.participants <- use.dates$id
  
  df.smoking.plotdat <- df.all %>% filter(id %in% these.other.participants)
  df.smoking.plotdat <- left_join(x = df.other.participants.dates, y = df.smoking.plotdat, by = "id")
  df.smoking.plotdat <- df.smoking.plotdat %>% filter(id %in% these.participants)
  df.smoking.plotdat <- df.smoking.plotdat %>% filter(time.unixts >= as.numeric(EMA_Qday) - 7*24*60*60 & time.unixts <= as.numeric(EMA_Qday) + 21*24*60*60)
  
  for(j in 1:length(these.participants)){
    use.id <- these.participants[j]
    df.this.participant <- df.smoking.plotdat %>% filter(id==use.id)
    
    postquit.earliest <- df.this.participant %>%
      filter(grepl("Post-Quit",assessment.type)) %>% 
      select(time.unixts) %>% 
      min(.)
    
    df.this.participant <- df.this.participant %>%
      mutate(diffdays.postquit.earliest = (time.unixts - as.numeric(postquit.earliest))/(3600*24)) %>%
      mutate(diffdays.postquit.earliest = round(diffdays.postquit.earliest, digits=6))
    
    jpeg(filename=file.path(path.pns.output_data, "plots_known_QD", paste("ZOOM_Participant_", use.id-3000,".jpg",sep="")), width=600, height=600, units="px")
    source(file.path(path.pns.code, "construct_quit_dates", "create-smoking-plots.R"))
    dev.off()
  }
}


