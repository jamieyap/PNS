# About this script: In this script, we perform a series of data curation tasks 
# specific to data from the Break Free study

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------

library(assertthat)
library(dplyr)
path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
source(file.path(path.code,"data-manip-utils.R"))
use.study <- "BreakFree"

#------------------------------------------------------------------------------
# Create a masterlist for the start and end of clock for each participant
#------------------------------------------------------------------------------

breakfree.quit.dates <- read.csv(file.path(path.input_data, "BreakFree/breakfree_quit_dates.csv"))  
breakfree.quit.dates <- SetClock(df.quit.dates = breakfree.quit.dates, study.duration = 10, addtime = 1)
write.csv(breakfree.quit.dates, file.path(path.output_data, "BreakFree/out_quit_dates.csv"), row.names = FALSE)

#------------------------------------------------------------------------------
# Read in raw data from Break Free study
#------------------------------------------------------------------------------

breakfree.ema <- read.csv(file.path(path.input_data, "BreakFree/Rice_EMAs.csv"), header = TRUE)
breakfree.puffmarker.episodes <- read.csv(file.path(path.input_data, "BreakFree/puffmarker_unzipped/puffmarker.smoking.episodes.csv"), header = TRUE)

#------------------------------------------------------------------------------
# Among EMAs delivered, check whether there is any indication that participant 
# completed any question; Perform initial clean up of responses
#------------------------------------------------------------------------------

tmpdf <- breakfree.ema[,-c(1:9, 116, 117)]
tmpdf <- tmpdf[, grep("Time", colnames(tmpdf), invert = TRUE)]
tmpdf <- apply(tmpdf, 2, as.character)
tmpdf <- apply(tmpdf, 2, function(x){
  x <- replace(x, (x=="")|(x=="None"), NA_character_)
  return(x)
})
tmpdf <- as.data.frame(tmpdf)
tmpdf <- CheckAnyResponse(df = tmpdf)
tmpdf.responses <- tmpdf
remove(tmpdf)

tmpdf <- breakfree.ema[,-c(1:9, 116, 117)]
tmpdf <- tmpdf[, grep("Time", colnames(tmpdf), invert = FALSE)]
tmpdf <- apply(tmpdf, 2, function(x){
  x <- replace(x, x==-1, NA_real_)
  x <- x/1000
  return(x)
})
tmpdf.timestamps <- tmpdf
remove(tmpdf)

breakfree.ema <- cbind(breakfree.ema[,c("Participant","Event","Status","Starttime")], tmpdf.responses, tmpdf.timestamps)
breakfree.ema$Starttime <- breakfree.ema$Starttime/1000
breakfree.ema$Time <- if_else(breakfree.ema$with.any.response==1 & is.na(breakfree.ema$Time), breakfree.ema$Starttime, breakfree.ema$Time)
breakfree.ema$Starttime <- if_else((breakfree.ema$Starttime > breakfree.ema$Time) & !is.na(breakfree.ema$Time), breakfree.ema$Time, breakfree.ema$Starttime)
remove(tmpdf.responses, tmpdf.timestamps)

#------------------------------------------------------------------------------
# Merge start.clock and end.clock information with EMA records; only retain
# observations delivered between start.clock and end.clock
#------------------------------------------------------------------------------

# Create IDs for each person-EMA delivered
breakfree.ema <- breakfree.ema %>% 
  mutate(Event = as.character(Event)) %>%
  mutate(Status = as.character(Status)) %>%
  rename(id = Participant, 
         assessment.type = Event, 
         delivered.unixts = Starttime, 
         assessment.unixts = Time, 
         record.status = Status) %>% 
  arrange(id, delivered.unixts) %>%
  left_join(x = ., y = breakfree.quit.dates, by = "id") %>%
  filter(delivered.unixts >= start.clock & delivered.unixts <= end.clock)

# Create IDs for each person-EMA with >=1 question completed
breakfree.ema[,"delivered.assessment.id"] <- CreateID(dat = breakfree.ema, sequence.along = "delivered.unixts", by.var = "id", id.name = "delivered.assessment.id")

#------------------------------------------------------------------------------
# Merge start.clock and end.clock information with puffmarker records; 
# only retain observations delivered between start.clock and end.clock
#------------------------------------------------------------------------------

breakfree.puffmarker.episodes <- left_join(breakfree.quit.dates, breakfree.puffmarker.episodes, by = "id")

breakfree.puffmarker.episodes <- breakfree.puffmarker.episodes %>% 
  mutate(puffmarker.episode.unixts = V1/1000) %>%
  mutate(puffmarker.episode.secs = puffmarker.episode.unixts - start.clock) %>%
  filter(puffmarker.episode.unixts >= start.clock & puffmarker.episode.unixts <= end.clock) %>%
  select(id, 
         #start.clock, end.clock, 
         #puffmarker.episode.unixts, 
         puffmarker.episode.secs
  )

write.csv(breakfree.puffmarker.episodes, file.path(path.output_data,"BreakFree/breakfree.puffmarker.episodes.csv"), row.names = FALSE)