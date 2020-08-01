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

#------------------------------------------------------------------------------
# Get time variables
#------------------------------------------------------------------------------

# Read in candidate dates
df.alldates <- read.csv(file.path(path.pns.output_data, "alldates.csv"), header = TRUE, stringsAsFactors = FALSE)

# Format dates
df.alldates[["EMA_Qday"]] <- as.POSIXct(strptime(df.alldates[["EMA_Qday"]], format = "%Y-%m-%d", tz = "UTC"))
df.alldates[["quitday"]] <- as.POSIXct(strptime(df.alldates[["quitday"]], format = "%Y-%m-%d", tz = "UTC"))

df.alldates[["prequit.latest.shortformatdate"]] <- as.POSIXct(strptime(df.alldates[["prequit.latest.shortformatdate"]], format = "%Y-%m-%d", tz = "UTC"))
df.alldates[["postquit.earliest.shortformatdate"]] <- as.POSIXct(strptime(df.alldates[["postquit.earliest.shortformatdate"]], format = "%Y-%m-%d", tz = "UTC"))

df.alldates[["prequit.latest.longformatdate"]] <- as.POSIXct(strptime(df.alldates[["prequit.latest.longformatdate"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
df.alldates[["postquit.earliest.longformatdate"]] <- as.POSIXct(strptime(df.alldates[["postquit.earliest.longformatdate"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

#------------------------------------------------------------------------------
# Calculate more time variables
#------------------------------------------------------------------------------

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

#------------------------------------------------------------------------------
# Clean up smoking counts variable
# Apply decision rule for counts of cigarettes smoked utilized to 
# infer quit dates when quit date is ambiguous
#------------------------------------------------------------------------------

source(file.path(path.pns.code, "identify-smoking-vars.R"))

for(i in 1:length(list.all)){
  list.all[[i]]$smoking.qty = NA_real_
}


# note that the rules below to construct smoking.qty differ from the rules
# in the script rules-smoking-quantity.R

# Pre-Quit EMAs ###############################################################

list.all[["Pre-Quit Random"]] <- list.all[["Pre-Quit Random"]] %>% 
  mutate(smoking.qty = case_when(
    assessment.type=="Pre-Quit Random" & rawdata.qty==0 ~ 0,
    assessment.type=="Pre-Quit Random" & rawdata.qty==1 ~ 1,
    assessment.type=="Pre-Quit Random" & rawdata.qty==2 ~ 2,
    assessment.type=="Pre-Quit Random" & rawdata.qty==3 ~ 4,
    assessment.type=="Pre-Quit Random" & rawdata.qty==4 ~ 6,
    assessment.type=="Pre-Quit Random" & rawdata.qty==5 ~ 8,
    assessment.type=="Pre-Quit Random" & rawdata.qty==6 ~ 10,
    assessment.type=="Pre-Quit Random" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  # smoking.qty is missing if participant reported "No" smoking
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

list.all[["Pre-Quit Urge"]] <- list.all[["Pre-Quit Urge"]] %>% 
  mutate(smoking.qty = case_when(
    assessment.type=="Pre-Quit Urge" & rawdata.qty==0 ~ 0,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==1 ~ 1,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==2 ~ 2,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==3 ~ 4,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==4 ~ 6,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==5 ~ 8,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==6 ~ 10,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  # smoking.qty is missing if participant reported "No" smoking
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

list.all[["Pre-Quit Smoking Part Two"]] <- list.all[["Pre-Quit Smoking Part Two"]] %>% 
  mutate(rawdata.qty = case_when(
    # Clean up responses so that smoking.qty for all EMA types below are on the same scale
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==3 ~ 2,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==4 ~ 3,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==5 ~ 4,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==6 ~ 5,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==7 ~ 6,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==8 ~ 7,
    TRUE ~ as.numeric(rawdata.qty))) %>% 
  mutate(smoking.qty = case_when(
    # Fill in smoking.qty
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==0 ~ 0,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==1 ~ 1,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==2 ~ 2,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==3 ~ 4,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==4 ~ 6,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==5 ~ 8,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==6 ~ 10,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty)) 

# Post-Quit EMAs ###############################################################
list.all[["Post-Quit Random"]] <- list.all[["Post-Quit Random"]] %>% 
  mutate(smoking.qty = case_when(
    assessment.type=="Post-Quit Random" & rawdata.qty==0 ~ 0,
    assessment.type=="Post-Quit Random" & rawdata.qty==1 ~ 1,
    assessment.type=="Post-Quit Random" & rawdata.qty==2 ~ 2,
    assessment.type=="Post-Quit Random" & rawdata.qty==3 ~ 4,
    assessment.type=="Post-Quit Random" & rawdata.qty==4 ~ 6,
    assessment.type=="Post-Quit Random" & rawdata.qty==5 ~ 8,
    assessment.type=="Post-Quit Random" & rawdata.qty==6 ~ 10,
    assessment.type=="Post-Quit Random" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  # smoking.qty is missing if participant reported "No" smoking
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

list.all[["Post-Quit Urge"]] <- list.all[["Post-Quit Urge"]] %>% 
  mutate(smoking.qty = case_when(
    assessment.type=="Post-Quit Urge" & rawdata.qty==0 ~ 0,
    assessment.type=="Post-Quit Urge" & rawdata.qty==1 ~ 1,
    assessment.type=="Post-Quit Urge" & rawdata.qty==2 ~ 2,
    assessment.type=="Post-Quit Urge" & rawdata.qty==3 ~ 4,
    assessment.type=="Post-Quit Urge" & rawdata.qty==4 ~ 6,
    assessment.type=="Post-Quit Urge" & rawdata.qty==5 ~ 8,
    assessment.type=="Post-Quit Urge" & rawdata.qty==6 ~ 10,
    assessment.type=="Post-Quit Urge" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  # smoking.qty is missing if participant reported "No" smoking
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

list.all[["Post-Quit About to Slip Part Two"]] <- list.all[["Post-Quit About to Slip Part Two"]] %>% 
  mutate(rawdata.qty = case_when(
    # Clean up responses so that smoking.qty for all EMA types below are on the same scale
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==3 ~ 2,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==4 ~ 3,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==5 ~ 4,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==6 ~ 5,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==7 ~ 6,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==8 ~ 7,
    TRUE ~ as.numeric(rawdata.qty))) %>% 
  mutate(smoking.qty = case_when(
    # Fill in smoking.qty
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==0 ~ 0,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==1 ~ 1,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==2 ~ 2,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==3 ~ 4,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==4 ~ 6,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==5 ~ 8,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==6 ~ 10,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty))

list.all[["Post-Quit Already Slipped"]] <- list.all[["Post-Quit Already Slipped"]] %>% 
  mutate(smoking.qty = case_when(
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==0 ~ 0,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==1 ~ 1,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==2 ~ 2,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==3 ~ 4,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==4 ~ 6,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==5 ~ 8,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==6 ~ 10,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  # smoking.qty is missing if participant reported "No" smoking
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

#------------------------------------------------------------------------------
# Obtain rows & columns to be used to visualize pattern of smoking
#------------------------------------------------------------------------------

list.all <- lapply(list.all, function(dat){
  # Select first few columns
  dat <- dat %>% select(id, record.id, assessment.type, with.any.response,
                        delivered.hrts, begin.hrts, end.hrts, time.hrts,
                        delivered.unixts, begin.unixts, end.unixts, time.unixts,
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

