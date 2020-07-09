# Zoom view of plot
df.smoking.plotdat <- df.all %>% filter(id %in% inspect.these.participants3)
df.smoking.plotdat <- left_join(x = df.infer.quitdate3, y = df.smoking.plotdat, by = "id")

postquit.earliest <- df.smoking.plotdat %>% 
  filter(time.unixts >= as.numeric(EMA_Qday) - 7*24*60*60) %>%
  filter(grepl("Post-Quit",assessment.type)) %>% 
  select(time.unixts) %>% 
  min(.)

df.smoking.plotdat <- df.smoking.plotdat %>%
  filter(time.unixts >= as.numeric(EMA_Qday) - 7*24*60*60) %>%
  mutate(diffdays.postquit.earliest = (time.unixts - as.numeric(postquit.earliest))/(3600*24)) %>%
  mutate(diffdays.postquit.earliest = round(diffdays.postquit.earliest, digits=6))

use.dates <- df.infer.quitdate3

for(j in 1:length(inspect.these.participants3)){
  use.id <- inspect.these.participants3[j]
  df.this.participant <- df.smoking.plotdat %>% filter(id==use.id)
  jpeg(filename=file.path(path.pns.output_data, "plots_inferred_QD_part3", paste("ZOOM_Participant_", use.id-3000,".jpg",sep="")), width=600, height=600, units="px")
  use.dates[use.dates$id==use.id,"diffdays.EMA_Qday"] <- 0
  use.dates[use.dates$id==use.id,"diffdays.quitday"] <- 0
  source(file.path(path.pns.code, "smoking-plots.R"))
  dev.off()
}
