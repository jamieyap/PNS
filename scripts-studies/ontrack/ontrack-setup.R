# About this script: In this script, we perform a series of data curation tasks 
# specific to data from the On Track study

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------

library(assertthat)
library(dplyr)
path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
source(file.path(path.code,"data-manip-utils.R"))
use.study <- "OnTrack"
ontrack.drop.ids <- Sys.getenv("ontrack.drop.ids")
ontrack.drop.ids <- strsplit(ontrack.drop.ids, ", ")
ontrack.drop.ids <- unlist(ontrack.drop.ids)
ontrack.drop.ids <- as.numeric(ontrack.drop.ids)

#------------------------------------------------------------------------------
# Create a masterlist for the start and end of clock for each participant
#------------------------------------------------------------------------------

ontrack.quit.dates <- read.csv(file.path(path.input_data, "ontrack/ontrack_quit_dates.csv"))  
ontrack.quit.dates <- ontrack.quit.dates %>% filter(!(id %in% ontrack.drop.ids))
ontrack.quit.dates <- SetClock(df.quit.dates = ontrack.quit.dates, study.duration = 10, addtime = 1)
write.csv(ontrack.quit.dates, file.path(path.output_data, "ontrack/out_quit_dates.csv"), row.names = FALSE)

#------------------------------------------------------------------------------
# Read in raw data from On Track study
#------------------------------------------------------------------------------

ontrack.ema <- read.csv(file.path(path.input_data, "OnTrack/Utah_ema.csv"), header = TRUE)

#------------------------------------------------------------------------------
# Among EMAs delivered, check whether there is any indication that participant 
# completed any question; Perform initial clean up of responses
#------------------------------------------------------------------------------

tmpdf <- ontrack.ema[,-c(1:9, 116, 117)]
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

tmpdf <- ontrack.ema[,-c(1:9, 116, 117)]
tmpdf <- tmpdf[, grep("Time", colnames(tmpdf), invert = FALSE)]
tmpdf <- apply(tmpdf, 2, function(x){
  x <- replace(x, x==-1, NA_real_)
  x <- x/1000
  return(x)
})
tmpdf.timestamps <- tmpdf
remove(tmpdf)

ontrack.ema <- cbind(ontrack.ema[,c("Participant","Event","Status","Starttime","Date.Time")], tmpdf.responses, tmpdf.timestamps)
ontrack.ema$Starttime <- ontrack.ema$Starttime/1000
ontrack.ema$Time <- if_else(ontrack.ema$with.any.response==1 & is.na(ontrack.ema$Time), ontrack.ema$Starttime, ontrack.ema$Time)
ontrack.ema$Starttime <- if_else((ontrack.ema$Starttime > ontrack.ema$Time) & !is.na(ontrack.ema$Time), ontrack.ema$Time, ontrack.ema$Starttime)
remove(tmpdf.responses, tmpdf.timestamps)

#------------------------------------------------------------------------------
# Merge start.clock and end.clock information with EMA records; only retain
# observations delivered between start.clock and end.clock
#------------------------------------------------------------------------------

# Create IDs for each person-EMA delivered
ontrack.ema <- ontrack.ema %>% 
  mutate(Event = as.character(Event)) %>%
  mutate(Status = as.character(Status)) %>%
  rename(id = Participant, 
         assessment.type = Event, 
         delivered.unixts = Starttime, 
         assessment.unixts = Time, 
         record.status = Status) %>% 
  arrange(id, delivered.unixts) %>%
  left_join(x = ., y = ontrack.quit.dates, by = "id") %>%
  filter(delivered.unixts >= start.clock & delivered.unixts <= end.clock) %>%
  filter(!(id %in% ontrack.drop.ids))

# Create IDs for each person-EMA with >=1 question completed
ontrack.ema[,"delivered.assessment.id"] <- CreateID(dat = ontrack.ema, sequence.along = "delivered.unixts", by.var = "id", id.name = "delivered.assessment.id")








