###############################################################################
# ABOUT:
# * Creates the smoking outcome from the output csv files of all the
#   following scripts: get-postquit-ema.R and get-prequit-ema.R
###############################################################################

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------
library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
path.pns.code <- Sys.getenv("path.pns.code")
path.shared.code <- Sys.getenv("path.shared.code")
source(file.path(path.shared.code, "shared-data-manip-utils.R"))
source(file.path(path.pns.code, "data-manip-utils.R"))

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
  group_by(id) %>%
  do(GetPastRecords(df.this.group = ., 
                    cols.today = c("record.id","assessment.type","time.unixts","time.hrts"), 
                    h = c(1,1,1,1), 
                    this.numeric = c(FALSE,FALSE,TRUE,FALSE)))

df <- df %>% 
  arrange(id, time.unixts) %>%
  group_by(id) %>%
  mutate(ones=1) %>%
  mutate(current.order.in.sequence = cumsum(ones)) %>%
  select(-ones) %>%
  mutate(time.between.hours = (time.unixts - time.unixts_shift.minus.1)/(60*60)) %>%
  rename(current.record.id = record.id,
         current.assessment.type = assessment.type,
         current.time.hrts = time.hrts,
         current.time.unixts = time.unixts,
         previous.record.id = record.id_shift.minus.1,
         previous.assessment.type = assessment.type_shift.minus.1,
         previous.time.hrts = time.hrts_shift.minus.1,
         previous.time.unixts = time.unixts_shift.minus.1) %>%
  select(id, 
         previous.record.id, previous.assessment.type, 
         previous.time.hrts, previous.time.unixts,
         current.record.id, current.assessment.type, 
         current.order.in.sequence,
         current.time.hrts, current.time.unixts,
         time.between.hours, everything())

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
    # Fill in num.cigs.smoked
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
    # Fill in num.cigs.smoked
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==0 ~ 0,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==1 ~ 1,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==2 ~ 2,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==3 ~ 4,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==4 ~ 6,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==5 ~ 8,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==6 ~ 10,
    current.assessment.type=="Pre-Quit Smoking - Part2" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty))

#------------------------------------------------------------------------------
# Reduce reported ranges of most recent time when an individual smoked into 
# single numeric values
#------------------------------------------------------------------------------
df$smoking.delta.minutes <- NA_real_

# Responses to timing of smoking in Post-Quit Already slipped (0-8) is on a 
# different scale than Post-Quit Random and Post-Quit Urge, both of 
# which are on the same scale (1-9). We shift responses to Post-Quit Random and
# Post-Quit Urge so that they are on a scale of 0-8
df <- df %>% 
  mutate(rawdata.timing = as.double(rawdata.timing)) %>%
  mutate(rawdata.timing = if_else(current.assessment.type == "Post-Quit Random", rawdata.timing-1, rawdata.timing)) %>%
  mutate(rawdata.timing = if_else(current.assessment.type == "Post-Quit Urge", rawdata.timing-1, rawdata.timing))

# Analogously implement for Pre-Quit data
df <- df %>% 
  mutate(rawdata.timing = as.double(rawdata.timing)) %>%
  mutate(rawdata.timing = if_else(current.assessment.type == "Pre-Quit Random", rawdata.timing-1, rawdata.timing)) %>%
  mutate(rawdata.timing = if_else(current.assessment.type == "Pre-Quit Urge", rawdata.timing-1, rawdata.timing))

# Now that all responses to timing of smoking are on the same scale, we proceed ro perform the next step
df <- df %>% 
  mutate(smoking.delta.minutes = (15*rawdata.timing +15* (rawdata.timing+1))/2) %>%
  mutate(smoking.delta.hours = smoking.delta.minutes/60)

#------------------------------------------------------------------------------
# When two consecutive EMAs used to construct the smoking outcome are more than
# CUTOFF time apart, then consider smoking information reported in the latter 
# EMA as missing information
#------------------------------------------------------------------------------
CUTOFF <- 48

df <- df %>% 
  mutate(smoking.qty = if_else(time.between.hours>CUTOFF & current.assessment.type!="Post-Quit Already Slipped", NA_real_, smoking.qty)) %>%
  mutate(smoking.delta.minutes = if_else(time.between.hours>CUTOFF & current.assessment.type!="Post-Quit Already Slipped", NA_real_, smoking.delta.minutes)) %>%
  mutate(smoking.delta.hours = if_else(time.between.hours>CUTOFF & current.assessment.type!="Post-Quit Already Slipped", NA_real_, smoking.delta.hours))

#------------------------------------------------------------------------------
# Check whether reported timing of smoking is not consistent with length of 
# time interval between two consecutive EMAs used to construct smoking outcome
#------------------------------------------------------------------------------
df <- df %>% mutate(is.greaterthan.timebetween = if_else((smoking.delta.hours > time.between.hours), 1, 0))

#------------------------------------------------------------------------------
# Calculate minimum possible time and maximum possible time
#------------------------------------------------------------------------------
df$earliest.possible.time.hrts <- as.POSIXct(NA_character_)
df$latest.possible.time.hrts <- as.POSIXct(NA_character_)

df <- df %>% mutate(previous.time.unixts = as.numeric(previous.time.unixts), current.time.unixts = as.numeric(current.time.unixts)) 
df$earliest.possible.time.unixts <- NA_real_
df$latest.possible.time.unixts <- NA_real_

df <- df %>% 
  mutate(earliest.possible.time.unixts = case_when(
    # Post-Quit
    (!is.na(smoking.qty)) & current.assessment.type=="Post-Quit Random" ~ previous.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Post-Quit Urge" ~ previous.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Post-Quit About to Slip" ~ previous.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Post-Quit About to Slip - Part2" ~ previous.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Post-Quit Already Slipped" ~ min(previous.time.unixts,current.time.unixts-24*60*60),
    # Pre-Quit
    (!is.na(smoking.qty)) & current.assessment.type=="Pre-Quit Random" ~ previous.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Pre-Quit Urge" ~ previous.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Pre-Quit Smoking" ~ previous.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Pre-Quit Smoking - Part2" ~ previous.time.unixts,
    TRUE ~ earliest.possible.time.unixts
  )) %>% 
  mutate(latest.possible.time.unixts = case_when(
    # Post-Quit
    (!is.na(smoking.qty)) & current.assessment.type=="Post-Quit Random" ~ current.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Post-Quit Urge" ~ current.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Post-Quit About to Slip" ~ current.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Post-Quit About to Slip - Part2" ~ current.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Post-Quit Already Slipped" ~ current.time.unixts,
    # Post-Quit
    (!is.na(smoking.qty)) & !is.na(smoking.delta.minutes) & is.greaterthan.timebetween==0 & current.assessment.type=="Post-Quit Random" ~ current.time.unixts-60*smoking.delta.minutes,
    (!is.na(smoking.qty)) & !is.na(smoking.delta.minutes) & is.greaterthan.timebetween==0 & current.assessment.type=="Post-Quit Urge" ~ current.time.unixts-60*smoking.delta.minutes,
    (!is.na(smoking.qty)) & !is.na(smoking.delta.minutes) & is.greaterthan.timebetween==0 & current.assessment.type=="Post-Quit About to Slip" ~ current.time.unixts-60*smoking.delta.minutes,
    (!is.na(smoking.qty)) & !is.na(smoking.delta.minutes) & is.greaterthan.timebetween==0 & current.assessment.type=="Post-Quit About to Slip - Part2" ~ current.time.unixts-60*smoking.delta.minutes,
    (!is.na(smoking.qty)) & !is.na(smoking.delta.minutes) & current.assessment.type=="Post-Quit Already Slipped" ~ current.time.unixts-60*smoking.delta.minutes,
    # Pre-Quit
    (!is.na(smoking.qty)) & current.assessment.type=="Pre-Quit Random" ~ current.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Pre-Quit Urge" ~ current.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Pre-Quit Smoking" ~ current.time.unixts,
    (!is.na(smoking.qty)) & current.assessment.type=="Pre-Quit Smoking - Part2" ~ current.time.unixts,
    # Pre-Quit
    (!is.na(smoking.qty)) & !is.na(smoking.delta.minutes) & is.greaterthan.timebetween==0 & current.assessment.type=="Pre-Quit Random" ~ current.time.unixts-60*smoking.delta.minutes,
    (!is.na(smoking.qty)) & !is.na(smoking.delta.minutes) & is.greaterthan.timebetween==0 & current.assessment.type=="Pre-Quit Urge" ~ current.time.unixts-60*smoking.delta.minutes,
    (!is.na(smoking.qty)) & !is.na(smoking.delta.minutes) & is.greaterthan.timebetween==0 & current.assessment.type=="Pre-Quit Smoking" ~ current.time.unixts-60*smoking.delta.minutes,
    (!is.na(smoking.qty)) & !is.na(smoking.delta.minutes) & is.greaterthan.timebetween==0 & current.assessment.type=="Pre-Quit Smoking - Part2" ~ current.time.unixts-60*smoking.delta.minutes,
    TRUE ~ earliest.possible.time.unixts
  )) 


# Create human-readable timestamps
df[["earliest.possible.time.hrts"]] <- as.POSIXct(df[["earliest.possible.time.unixts"]], origin="1970-01-01")
df[["latest.possible.time.hrts"]] <- as.POSIXct(df[["latest.possible.time.unixts"]], origin="1970-01-01")

#------------------------------------------------------------------------------
# Save data
#------------------------------------------------------------------------------
write.csv(df, file.path(path.pns.output_data, "smoking.csv"), row.names=FALSE, na="")


