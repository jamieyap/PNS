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
path.pns.code <- Sys.getenv("path.pns.code")
path.shared.code <- Sys.getenv("path.shared.code")
source(file.path(path.shared.code, "shared-data-manip-utils.R"))
source(file.path(path.pns.code, "data-manip-utils.R"))

df.alldates <- read.csv(file.path(path.pns.output_data, "dates.csv"), header=TRUE, stringsAsFactors = FALSE)
df.alldates$postquit.earliest.shortformatdate <- as.POSIXct(strptime(df.alldates$postquit.earliest.shortformatdate, "%Y-%m-%d"))
df.alldates$prequit.latest.shortformatdate <- as.POSIXct(strptime(df.alldates$prequit.latest.shortformatdate, "%Y-%m-%d"))
df.alldates$postquit.earliest.longformatdate <- as.POSIXct(strptime(df.alldates$postquit.earliest.longformatdate, "%Y-%m-%d %H:%M:%S"))
df.alldates$prequit.latest.longformatdate <- as.POSIXct(strptime(df.alldates$prequit.latest.longformatdate, "%Y-%m-%d %H:%M:%S"))
df.alldates$EMA_Qday <- as.POSIXct(strptime(df.alldates$EMA_Qday, "%Y-%m-%d"))
df.alldates$quitday <- as.POSIXct(strptime(df.alldates$quitday, "%Y-%m-%d"))
df.alldates$diffdays.EMA_Qday <- df.alldates$EMA_Qday - df.alldates$postquit.earliest.shortformatdate  # this will be in seconds
df.alldates$diffdays.quitday <- df.alldates$quitday - df.alldates$postquit.earliest.shortformatdate  # this will be in seconds
df.alldates$diffdays.EMA_Qday <- as.numeric(df.alldates$diffdays.EMA_Qday)/(24*60*60)  # convert to days
df.alldates$diffdays.quitday <- as.numeric(df.alldates$diffdays.quitday)/(24*60*60)  # convert to days

#------------------------------------------------------------------------------
# Determine participants for whom quit date must be inferred
#------------------------------------------------------------------------------
df.alldates <- df.alldates %>% filter(!is.na(is.equal))

# Participants for whom Quit Dates must be inferred
df.infer.quitdate <- df.alldates %>% filter(is.na(quit.hour))
inspect.these.participants <- unique(df.infer.quitdate$id)

# Participants for whom Quit Date is known
these.other.participants <- df.alldates %>% filter(!(id %in% inspect.these.participants)) %>% select(id) %>% unique(.)
these.other.participants <- these.other.participants[["id"]]
df.other.participants.dates <- df.alldates %>% filter(id %in% these.other.participants)

#------------------------------------------------------------------------------
# Read in EMA data
#------------------------------------------------------------------------------

# Post Quit EMAs
df.postquit.random <- read.csv(file.path(path.pns.output_data, "postquit_random.csv"), stringsAsFactors = FALSE)
df.postquit.urge <- read.csv(file.path(path.pns.output_data, "postquit_urge.csv"), stringsAsFactors = FALSE)
df.postquit.abouttoslippartone <- read.csv(file.path(path.pns.output_data, "postquit_abouttoslippartone.csv"), stringsAsFactors = FALSE)
df.postquit.abouttoslipparttwo <- read.csv(file.path(path.pns.output_data, "postquit_abouttoslipparttwo.csv"), stringsAsFactors = FALSE)
df.postquit.alreadyslipped <- read.csv(file.path(path.pns.output_data, "postquit_alreadyslipped.csv"), stringsAsFactors = FALSE)

# Pre Quit EMAs
df.prequit.random <- read.csv(file.path(path.pns.output_data, "prequit_random.csv"), stringsAsFactors = FALSE)
df.prequit.urge <- read.csv(file.path(path.pns.output_data, "prequit_urge.csv"), stringsAsFactors = FALSE)
df.prequit.smokingpartone <- read.csv(file.path(path.pns.output_data, "prequit_smokingpartone.csv"), stringsAsFactors = FALSE)
df.prequit.smokingparttwo <- read.csv(file.path(path.pns.output_data, "prequit_smokingparttwo.csv"), stringsAsFactors = FALSE)

#------------------------------------------------------------------------------
# Identify smoking items across different types of EMAs
# and create common variable names across different types of EMAs
#------------------------------------------------------------------------------

# Post Quit EMAs
df.postquit.random <- df.postquit.random %>% 
  rename(rawdata.indicator = PostQRSmoking1, 
         rawdata.qty = Smoking2_PostQ_Random, 
         rawdata.timing = Smoking3)

df.postquit.urge <- df.postquit.urge %>% 
  rename(rawdata.indicator = SmPostQU1, 
         rawdata.qty = Smoking2_PostQ_Urge, 
         rawdata.timing = Smoking3)

df.postquit.abouttoslippartone <- df.postquit.abouttoslippartone %>% 
  mutate(rawdata.indicator = NA, 
         rawdata.qty = NA, 
         rawdata.timing = NA)

df.postquit.abouttoslipparttwo <- df.postquit.abouttoslipparttwo %>% 
  rename(rawdata.qty = CigJustNow_PostQ_Slip2) %>%
  mutate(rawdata.indicator = NA, 
         rawdata.timing = NA)

df.postquit.alreadyslipped <- df.postquit.alreadyslipped %>% 
  rename(rawdata.qty = HowManyCig, 
         rawdata.timing = LastCig) %>%
  mutate(rawdata.indicator = NA)

# Pre Quit EMAs
df.prequit.random <- df.prequit.random %>% 
  rename(rawdata.indicator = PreQRSmoking1, 
         rawdata.qty = Smoking2_PreQ_Random, 
         rawdata.timing = Smoking3)

df.prequit.urge <- df.prequit.urge %>% 
  rename(rawdata.indicator = SmPQU1, 
         rawdata.qty = Smoking2_PreQ_Urge, 
         rawdata.timing = Smoking3)

df.prequit.smokingpartone <- df.prequit.smokingpartone %>% 
  mutate(rawdata.indicator = NA, 
         rawdata.qty = NA, 
         rawdata.timing = NA)

df.prequit.smokingparttwo <- df.prequit.smokingparttwo %>% 
  rename(rawdata.qty = CigJustNow) %>%
  mutate(rawdata.indicator = NA, 
         rawdata.timing = NA)

#------------------------------------------------------------------------------
# Only retain rows that have data informative of the smoking outcome
#------------------------------------------------------------------------------

# Post Quit EMA
df.postquit.random <- df.postquit.random %>% 
  filter(with.any.response==1) %>% 
  mutate(time.unixts = begin.unixts, time.hrts = begin.hrts)

df.postquit.urge <- df.postquit.urge %>% 
  mutate(time.unixts = if_else(with.any.response==1, begin.unixts, delivered.unixts), 
         time.hrts = if_else(with.any.response==1, begin.hrts, delivered.hrts))

df.postquit.abouttoslippartone <- df.postquit.abouttoslippartone %>% 
  mutate(time.unixts = if_else(with.any.response==1, begin.unixts, delivered.unixts), 
         time.hrts = if_else(with.any.response==1, begin.hrts, delivered.hrts))

df.postquit.abouttoslipparttwo <- df.postquit.abouttoslipparttwo %>% 
  mutate(time.unixts = if_else(with.any.response==1, begin.unixts, delivered.unixts), 
         time.hrts = if_else(with.any.response==1, begin.hrts, delivered.hrts))

df.postquit.alreadyslipped <- df.postquit.alreadyslipped %>% 
  mutate(time.unixts = if_else(with.any.response==1, begin.unixts, delivered.unixts), 
         time.hrts = if_else(with.any.response==1, begin.hrts, delivered.hrts))

# Pre Quit EMA
df.prequit.random <- df.prequit.random %>% 
  filter(with.any.response==1) %>% 
  mutate(time.unixts = begin.unixts, time.hrts = begin.hrts)

df.prequit.urge <- df.prequit.urge %>% 
  mutate(time.unixts = if_else(with.any.response==1, begin.unixts, delivered.unixts), 
         time.hrts = if_else(with.any.response==1, begin.hrts, delivered.hrts))

df.prequit.smokingpartone <- df.prequit.smokingpartone %>% 
  mutate(time.unixts = if_else(with.any.response==1, begin.unixts, delivered.unixts), 
         time.hrts = if_else(with.any.response==1, begin.hrts, delivered.hrts))

df.prequit.smokingparttwo <- df.prequit.smokingparttwo %>% 
  mutate(time.unixts = if_else(with.any.response==1, begin.unixts, delivered.unixts), 
         time.hrts = if_else(with.any.response==1, begin.hrts, delivered.hrts))

#------------------------------------------------------------------------------
# Select columns to keep
#------------------------------------------------------------------------------

# Column names with reference information
keep.cols <- c("id","record.id","assessment.type",
               "time.hrts", "time.unixts", 
               "rawdata.indicator","rawdata.qty","rawdata.timing")

# Post Quit EMA
df.postquit.random <- df.postquit.random %>% select(keep.cols)
df.postquit.urge <- df.postquit.urge %>% select(keep.cols)
df.postquit.abouttoslippartone <- df.postquit.abouttoslippartone %>% select(keep.cols)
df.postquit.abouttoslipparttwo <- df.postquit.abouttoslipparttwo %>% select(keep.cols)
df.postquit.alreadyslipped <- df.postquit.alreadyslipped %>% select(keep.cols)

# Pre Quit EMA
df.prequit.random <- df.prequit.random %>% select(keep.cols)
df.prequit.urge <- df.prequit.urge %>% select(keep.cols)
df.prequit.smokingpartone <- df.prequit.smokingpartone %>% select(keep.cols)
df.prequit.smokingparttwo <- df.prequit.smokingparttwo %>% select(keep.cols)

#------------------------------------------------------------------------------
# Merge data from different types of EMA into one big data frame
#------------------------------------------------------------------------------

df <- rbind(df.postquit.random,
            df.postquit.urge,
            df.postquit.abouttoslippartone,
            df.postquit.abouttoslipparttwo,
            df.postquit.alreadyslipped,
            df.prequit.random,
            df.prequit.urge,
            df.prequit.smokingpartone,
            df.prequit.smokingparttwo)

df <- df %>% 
  arrange(id, time.unixts) %>%
  mutate(ones=1) %>%
  group_by(id) %>%
  mutate(current.order.in.sequence = cumsum(ones)) %>%
  rename(current.record.id = record.id,
         current.assessment.type = assessment.type,
         current.time.hrts = time.hrts,
         current.time.unixts = time.unixts) %>%
  select(id,
         current.record.id, current.assessment.type,
         current.time.hrts, current.time.unixts,
         current.order.in.sequence,
         rawdata.indicator, rawdata.qty)

#------------------------------------------------------------------------------
# Reduce reported ranges of smoking quantities into single numeric values
#------------------------------------------------------------------------------
df$smoking.qty <- NA_real_

# Post Quit EMA
df <- df %>% mutate(smoking.qty = case_when(
  current.assessment.type=="Post-Quit Random" & rawdata.qty==0 ~ 0,
  current.assessment.type=="Post-Quit Random" & rawdata.qty==1 ~ 1,
  current.assessment.type=="Post-Quit Random" & rawdata.qty==2 ~ 2,
  current.assessment.type=="Post-Quit Random" & rawdata.qty==3 ~ 4,
  current.assessment.type=="Post-Quit Random" & rawdata.qty==4 ~ 6,
  current.assessment.type=="Post-Quit Random" & rawdata.qty==5 ~ 8,
  current.assessment.type=="Post-Quit Random" & rawdata.qty==6 ~ 10,
  current.assessment.type=="Post-Quit Random" & rawdata.qty==7 ~ 11,
  TRUE ~ smoking.qty))

df <- df %>% mutate(smoking.qty = case_when(
  current.assessment.type=="Post-Quit Urge" & rawdata.qty==0 ~ 0,
  current.assessment.type=="Post-Quit Urge" & rawdata.qty==1 ~ 1,
  current.assessment.type=="Post-Quit Urge" & rawdata.qty==2 ~ 2,
  current.assessment.type=="Post-Quit Urge" & rawdata.qty==3 ~ 4,
  current.assessment.type=="Post-Quit Urge" & rawdata.qty==4 ~ 6,
  current.assessment.type=="Post-Quit Urge" & rawdata.qty==5 ~ 8,
  current.assessment.type=="Post-Quit Urge" & rawdata.qty==6 ~ 10,
  current.assessment.type=="Post-Quit Urge" & rawdata.qty==7 ~ 11,
  TRUE ~ smoking.qty))

df <- df %>% 
  mutate(rawdata.qty = case_when(
    # Clean up responses so that smoking.qty for all EMA types below are on the same scale
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==3 ~ 2,
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==4 ~ 3,
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==5 ~ 4,
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==6 ~ 5,
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==7 ~ 6,
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==8 ~ 7,
    TRUE ~ as.numeric(rawdata.qty))) %>% 
  mutate(smoking.qty = case_when(
    # Fill in smoking.qty
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==0 ~ 0,
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==1 ~ 1,
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==2 ~ 2,
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==3 ~ 4,
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==4 ~ 6,
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==5 ~ 8,
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==6 ~ 10,
    current.assessment.type=="Post-Quit About to Slip - Part2" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty))

df <- df %>% mutate(smoking.qty = case_when(
  current.assessment.type=="Post-Quit Already Slipped" & rawdata.qty==0 ~ 0,
  current.assessment.type=="Post-Quit Already Slipped" & rawdata.qty==1 ~ 1,
  current.assessment.type=="Post-Quit Already Slipped" & rawdata.qty==2 ~ 2,
  current.assessment.type=="Post-Quit Already Slipped" & rawdata.qty==3 ~ 4,
  current.assessment.type=="Post-Quit Already Slipped" & rawdata.qty==4 ~ 6,
  current.assessment.type=="Post-Quit Already Slipped" & rawdata.qty==5 ~ 8,
  current.assessment.type=="Post-Quit Already Slipped" & rawdata.qty==6 ~ 10,
  current.assessment.type=="Post-Quit Already Slipped" & rawdata.qty==7 ~ 11,
  TRUE ~ smoking.qty))

# Pre Quit EMA
df <- df %>% mutate(smoking.qty = case_when(
  current.assessment.type=="Pre-Quit Random" & rawdata.qty==0 ~ 0,
  current.assessment.type=="Pre-Quit Random" & rawdata.qty==1 ~ 1,
  current.assessment.type=="Pre-Quit Random" & rawdata.qty==2 ~ 2,
  current.assessment.type=="Pre-Quit Random" & rawdata.qty==3 ~ 4,
  current.assessment.type=="Pre-Quit Random" & rawdata.qty==4 ~ 6,
  current.assessment.type=="Pre-Quit Random" & rawdata.qty==5 ~ 8,
  current.assessment.type=="Pre-Quit Random" & rawdata.qty==6 ~ 10,
  current.assessment.type=="Pre-Quit Random" & rawdata.qty==7 ~ 11,
  TRUE ~ smoking.qty))

df <- df %>% mutate(smoking.qty = case_when(
  current.assessment.type=="Pre-Quit Urge" & rawdata.qty==0 ~ 0,
  current.assessment.type=="Pre-Quit Urge" & rawdata.qty==1 ~ 1,
  current.assessment.type=="Pre-Quit Urge" & rawdata.qty==2 ~ 2,
  current.assessment.type=="Pre-Quit Urge" & rawdata.qty==3 ~ 4,
  current.assessment.type=="Pre-Quit Urge" & rawdata.qty==4 ~ 6,
  current.assessment.type=="Pre-Quit Urge" & rawdata.qty==5 ~ 8,
  current.assessment.type=="Pre-Quit Urge" & rawdata.qty==6 ~ 10,
  current.assessment.type=="Pre-Quit Urge" & rawdata.qty==7 ~ 11,
  TRUE ~ smoking.qty))

df <- df %>%
  mutate(rawdata.qty = case_when(
    # Clean up responses so that smoking.qty for all EMA types below are on the same scale
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==3 ~ 2,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==4 ~ 3,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==5 ~ 4,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==6 ~ 5,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==7 ~ 6,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==8 ~ 7,
    TRUE ~ as.numeric(rawdata.qty))) %>% 
  mutate(smoking.qty = case_when(
    # Fill in smoking.qty
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==0 ~ 0,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==1 ~ 1,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==2 ~ 2,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==3 ~ 4,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==4 ~ 6,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==5 ~ 8,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==6 ~ 10,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty))

# Finally, use info from rawdata.indicator
df <- df %>% 
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty)) %>%
  select(-rawdata.indicator, -rawdata.qty)

#------------------------------------------------------------------------------
# Select participant data to keep
#------------------------------------------------------------------------------
df.smoking.plotdat <- df %>% filter(id %in% inspect.these.participants)
df.smoking.plotdat <- left_join(x = df.infer.quitdate, y = df.smoking.plotdat, by = "id")

df.smoking.plotdat <- df.smoking.plotdat %>%
  mutate(diffdays.postquit.earliest = (current.time.unixts - as.numeric(postquit.earliest.longformatdate))/(3600*24)) %>%
  mutate(diffdays.postquit.earliest = round(diffdays.postquit.earliest, digits=6))

use.dates <- df.infer.quitdate

for(j in 1:length(inspect.these.participants)){
  use.id <- inspect.these.participants[j]
  df.this.participant <- df.smoking.plotdat %>% filter(id==use.id)
  jpeg(filename=file.path(path.pns.output_data, "plots_inferred_QD", paste("Participant# ", use.id-3000,".jpg",sep="")), width=600, height=600, units="px")
  source(file.path(path.pns.code, "smoking-plots.R"))
  dev.off()
}

#------------------------------------------------------------------------------
# Select participant data to keep
#------------------------------------------------------------------------------
df.smoking.plotdat <- df %>% filter(id %in% these.other.participants)
df.smoking.plotdat <- left_join(x = df.other.participants.dates, y = df.smoking.plotdat, by = "id")
df.smoking.plotdat <- df.smoking.plotdat %>%
  mutate(diffdays.postquit.earliest = (current.time.unixts - as.numeric(postquit.earliest.longformatdate))/(3600*24)) %>%
  mutate(diffdays.postquit.earliest = round(diffdays.postquit.earliest, digits=6))

use.dates <- df.other.participants.dates

for(j in 1:length(these.other.participants)){
  use.id <- these.other.participants[j]
  df.this.participant <- df.smoking.plotdat %>% filter(id==use.id)
  jpeg(filename=file.path(path.pns.output_data, "plots_known_QD", paste("Participant_", use.id-3000,".jpg",sep="")), width=600, height=600, units="px")
  source(file.path(path.pns.code, "smoking-plots.R"))
  dev.off()
}

