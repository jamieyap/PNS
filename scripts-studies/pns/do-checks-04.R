#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------
library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)
library(ggplot2)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
path.pns.staged_data <- Sys.getenv("path.pns.staged_data")
path.pns.code <- Sys.getenv("path.pns.code")
path.shared.code <- Sys.getenv("path.shared.code")
source(file.path(path.shared.code, "shared-data-manip-utils.R"))
source(file.path(path.pns.code, "data-manip-utils.R"))

load(file = file.path(path.pns.staged_data, "alldates.RData"))

df.alldates$diffdays.EMA_Qday <- df.alldates$EMA_Qday - df.alldates$postquit.earliest.shortformatdate  # this will be in seconds
df.alldates$diffdays.quitday <- df.alldates$quitday - df.alldates$postquit.earliest.shortformatdate  # this will be in seconds
df.alldates$diffdays.EMA_Qday <- as.numeric(df.alldates$diffdays.EMA_Qday)/(24*60*60)  # convert to days
df.alldates$diffdays.quitday <- as.numeric(df.alldates$diffdays.quitday)/(24*60*60)  # convert to days

#------------------------------------------------------------------------------
# Determine participants for whom quit date must be inferred
#------------------------------------------------------------------------------
# Participants with missing values for is.equal will be dropped from the
# analytic dataset
df.alldates <- df.alldates %>% filter(!is.na(is.equal))

# Participants for whom Quit Dates must be inferred
df.infer.quitdate <- df.alldates %>% filter(is.equal==0 & prepost.is.lessthan==1)
inspect.these.participants <- unique(df.infer.quitdate$id)

# Participants for whom Quit Dates must be inferred
df.infer.quitdate2 <- df.alldates %>% filter(is.equal==0 &  prepost.is.equal==1)
inspect.these.participants2 <- unique(df.infer.quitdate2$id)

# Participants for whom Quit Dates must be inferred
df.infer.quitdate3 <- df.alldates %>% filter(is.equal==0 &  prepost.is.greaterthan==1)
inspect.these.participants3 <- unique(df.infer.quitdate3$id)

# Participants for whom Quit Date is known
these.other.participants <- df.alldates %>% 
  filter(!(id %in% inspect.these.participants)) %>% 
  filter(!(id %in% inspect.these.participants2)) %>%
  filter(!(id %in% inspect.these.participants3)) %>%
  select(id) %>% 
  unique(.)

these.other.participants <- these.other.participants[["id"]]
df.other.participants.dates <- df.alldates %>% filter(id %in% these.other.participants)

#------------------------------------------------------------------------------
# Read in EMA data
#------------------------------------------------------------------------------

load(file = file.path(path.pns.staged_data, "all_ema_processed.RData"))

# Obtain rows & columns to be used in constructing the smoking outcome
list.all <- lapply(list.all, function(dat){
  # Select first few columns
  dat <- dat %>% select(id, record.id, assessment.type, with.any.response,
                        delivered.hrts, begin.hrts, end.hrts, time.hrts,
                        delivered.unixts, begin.unixts, end.unixts, time.unixts,
                        rawdata.indicator, rawdata.qty, rawdata.timing,
                        smoking.qty)
  
  return(dat)
})

df.all <- bind_rows(list.all)
df.all <- df.all %>% arrange(id, time.unixts)

#------------------------------------------------------------------------------
# Select participant data to keep - part 1
#------------------------------------------------------------------------------
df.smoking.plotdat <- df.all %>% filter(id %in% inspect.these.participants)
df.smoking.plotdat <- left_join(x = df.infer.quitdate, y = df.smoking.plotdat, by = "id")

df.smoking.plotdat <- df.smoking.plotdat %>%
  mutate(diffdays.postquit.earliest = (time.unixts - as.numeric(postquit.earliest.longformatdate))/(3600*24)) %>%
  mutate(diffdays.postquit.earliest = round(diffdays.postquit.earliest, digits=6))

use.dates <- df.infer.quitdate

for(j in 1:length(inspect.these.participants)){
  use.id <- inspect.these.participants[j]
  df.this.participant <- df.smoking.plotdat %>% filter(id==use.id)
  jpeg(filename=file.path(path.pns.output_data, "plots_inferred_QD_part1", paste("Participant_", use.id-3000,".jpg",sep="")), width=600, height=600, units="px")
  source(file.path(path.pns.code, "smoking-plots.R"))
  dev.off()
}

#------------------------------------------------------------------------------
# Select participant data to keep - part 2
#------------------------------------------------------------------------------
df.smoking.plotdat <- df.all %>% filter(id %in% inspect.these.participants2)
df.smoking.plotdat <- left_join(x = df.infer.quitdate2, y = df.smoking.plotdat, by = "id")

df.smoking.plotdat <- df.smoking.plotdat %>%
  mutate(diffdays.postquit.earliest = (time.unixts - as.numeric(postquit.earliest.longformatdate))/(3600*24)) %>%
  mutate(diffdays.postquit.earliest = round(diffdays.postquit.earliest, digits=6))

use.dates <- df.infer.quitdate2

for(j in 1:length(inspect.these.participants2)){
  use.id <- inspect.these.participants2[j]
  df.this.participant <- df.smoking.plotdat %>% filter(id==use.id)
  jpeg(filename=file.path(path.pns.output_data, "plots_inferred_QD_part2", paste("Participant_", use.id-3000,".jpg",sep="")), width=600, height=600, units="px")
  source(file.path(path.pns.code, "smoking-plots.R"))
  dev.off()
}

#------------------------------------------------------------------------------
# Select participant data to keep - part 3
#------------------------------------------------------------------------------
df.smoking.plotdat <- df.all %>% filter(id %in% inspect.these.participants3)
df.smoking.plotdat <- left_join(x = df.infer.quitdate3, y = df.smoking.plotdat, by = "id")

df.smoking.plotdat <- df.smoking.plotdat %>%
  mutate(diffdays.postquit.earliest = (time.unixts - as.numeric(postquit.earliest.longformatdate))/(3600*24)) %>%
  mutate(diffdays.postquit.earliest = round(diffdays.postquit.earliest, digits=6))

use.dates <- df.infer.quitdate3

for(j in 1:length(inspect.these.participants3)){
  use.id <- inspect.these.participants3[j]
  df.this.participant <- df.smoking.plotdat %>% filter(id==use.id)
  jpeg(filename=file.path(path.pns.output_data, "plots_inferred_QD_part3", paste("Participant_", use.id-3000,".jpg",sep="")), width=600, height=600, units="px")
  source(file.path(path.pns.code, "smoking-plots.R"))
  dev.off()
}

# Zoom view
zoom.part3 <- TRUE
zoom.part4 <- FALSE
source(file.path(path.pns.code, "zoomed-smoking-plots.R"))

#------------------------------------------------------------------------------
# Select participant data to keep - part 4
#------------------------------------------------------------------------------
df.smoking.plotdat <- df.all %>% filter(id %in% these.other.participants)
df.smoking.plotdat <- left_join(x = df.other.participants.dates, y = df.smoking.plotdat, by = "id")
df.smoking.plotdat <- df.smoking.plotdat %>%
  mutate(diffdays.postquit.earliest = (time.unixts - as.numeric(postquit.earliest.longformatdate))/(3600*24)) %>%
  mutate(diffdays.postquit.earliest = round(diffdays.postquit.earliest, digits=6))

use.dates <- df.other.participants.dates

for(j in 1:length(these.other.participants)){
  use.id <- these.other.participants[j]
  df.this.participant <- df.smoking.plotdat %>% filter(id==use.id)
  jpeg(filename=file.path(path.pns.output_data, "plots_known_QD", paste("Participant_", use.id-3000,".jpg",sep="")), width=600, height=600, units="px")
  source(file.path(path.pns.code, "smoking-plots.R"))
  dev.off()
}

# Zoom view
zoom.part3 <- FALSE
zoom.part4 <- TRUE
source(file.path(path.pns.code, "zoomed-smoking-plots.R"))

