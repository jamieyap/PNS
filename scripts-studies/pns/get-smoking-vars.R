###############################################################################
# ABOUT:
# * Complete preparaton of smoking-related variables
# * Prior to running this script, run get-ema-item-responses.R
###############################################################################

library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

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

df.quit.dates <- read.csv(file.path(path.pns.output_data, "quit_dates_final.csv"), stringsAsFactors = FALSE)
df.quit.dates <- df.quit.dates %>% rename(start.study.hrts = start.study.date, end.study.hrts = end.study.date, quit.hrts = quit.date) 
df.quit.dates[["start.study.hrts"]] <- as.POSIXct(strptime(df.quit.dates[["start.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
df.quit.dates[["end.study.hrts"]] <- as.POSIXct(strptime(df.quit.dates[["end.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
df.quit.dates[["quit.hrts"]] <- as.POSIXct(strptime(df.quit.dates[["quit.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

# Convert human-readable timestamps to UNIX timestamps
df.quit.dates <- df.quit.dates %>%
  mutate(start.study.unixts = as.numeric(start.study.hrts),
         end.study.unixts = as.numeric(end.study.hrts),
         quit.unixts = as.numeric(quit.hrts)) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts,
         start.study.unixts, quit.unixts, end.study.unixts, everything())

#------------------------------------------------------------------------------
# Obtain rows & columns to be used in constructing the smoking outcome
#------------------------------------------------------------------------------

load(file = file.path(path.pns.staged_data, "all_ema_processed.RData"))

list.all <- lapply(list.all, function(dat){
  # Select first few columns
  dat <- dat %>% 
    select(id, record.id, assessment.type, with.any.response,
           delivered.hrts, begin.hrts, end.hrts, time.hrts,
           delivered.unixts, begin.unixts, end.unixts, time.unixts,
           rawdata.indicator, rawdata.qty, rawdata.timing,
           smoking.qty) %>%
    mutate(smoking.indicator = NA_real_, smoking.delta.minutes = NA_real_)
  
  # Exclude assessments that meet the criteria below from creating the smoking outcome
  dat <- dat %>% 
    filter(!(with.any.response==0 & assessment.type=="Pre-Quit Random")) %>%
    filter(!(with.any.response==0 & assessment.type=="Post-Quit Random")) 
  
  return(dat)
})

df.all <- bind_rows(list.all)

#------------------------------------------------------------------------------
# Implement inclusion/exclusion criteria
#------------------------------------------------------------------------------

df.all <- left_join(x = df.quit.dates, y = df.all, by = "id")
df.all <- df.all %>% filter((delivered.unixts>=start.study.unixts) & (delivered.unixts<=end.study.unixts))

#------------------------------------------------------------------------------
# Construct id for each smoking interval
#------------------------------------------------------------------------------

df.all <- df.all %>% 
  mutate(ones = 1) %>%
  arrange(id, time.unixts) %>%
  group_by(id) %>%
  mutate(ema.order = cumsum(ones))

#------------------------------------------------------------------------------
# Determine time between two consecutive EMAs
#------------------------------------------------------------------------------

df.all <- df.all %>%   
  arrange(id, time.unixts) %>%
  group_by(id) %>%
  do(GetPastRecords(df.this.group = ., cols.today = c("time.unixts","assessment.type"), h = c(1,1), this.numeric = c(TRUE,FALSE))) 


df.all <- df.all %>%
  mutate(time.unixts_shift.minus.1 = if_else(ema.order==1, start.study.unixts, time.unixts_shift.minus.1)) %>%
  mutate(time.between.hours = (time.unixts - time.unixts_shift.minus.1)/(60*60))

#------------------------------------------------------------------------------
# Clean up smoking time variable
#------------------------------------------------------------------------------

# Responses to timing of smoking in Post-Quit Already slipped (0-8) is on a 
# different scale than Post-Quit Random and Post-Quit Urge, both of 
# which are on the same scale (1-9). We shift responses to Post-Quit Random and
# Post-Quit Urge so that they are on a scale of 0-8.
#
# Similarly, Pre-Quit Random and Pre-Quit Urge are both on a scale of 1-9
# while the remaining types of Pre-Quit assessments are on a scale of 0-8
# Hence, we shift the scale of Pre-Quit Random and Pre-Quit Urge to be 0-8

df.all <- df.all %>%
  mutate(rawdata.timing = as.numeric(rawdata.timing)) %>%
  mutate(rawdata.timing = case_when(
    # Post-Quit assessments
    assessment.type == "Post-Quit Random" ~ rawdata.timing-1,
    assessment.type == "Post-Quit Urge" ~ rawdata.timing-1,
    # Pre-Quit assessments
    assessment.type == "Pre-Quit Random" ~ rawdata.timing-1,
    assessment.type == "Pre-Quit Urge" ~ rawdata.timing-1,
    # Else
    TRUE ~ rawdata.timing
  ))

df.all <- df.all %>%
  mutate(smoking.delta.minutes = case_when(
    # Post-Quit assessments
    assessment.type == "Post-Quit Random" & rawdata.timing!=8 ~ (15*rawdata.timing +15* (rawdata.timing+1))/2,
    assessment.type == "Post-Quit Urge" & rawdata.timing!=8 ~ (15*rawdata.timing +15* (rawdata.timing+1))/2,
    assessment.type == "Post-Quit Already Slipped" & rawdata.timing!=8 ~ (15*rawdata.timing +15* (rawdata.timing+1))/2,
    # Pre-Quit assessments
    assessment.type == "Pre-Quit Random" & rawdata.timing!=8 ~ (15*rawdata.timing +15* (rawdata.timing+1))/2,
    assessment.type == "Pre-Quit Urge" & rawdata.timing!=8 ~ (15*rawdata.timing +15* (rawdata.timing+1))/2,
    # Else
    TRUE ~ smoking.delta.minutes
  ))

df.all <- df.all %>%
  mutate(smoking.delta.minutes = case_when(
    # Post-Quit assessments
    assessment.type == "Post-Quit Random" & rawdata.timing==8 ~ 2*60 + (1/60)*((time.unixts-2*60*60) - time.unixts_shift.minus.1)/2,
    assessment.type == "Post-Quit Urge" & rawdata.timing==8 ~ 2*60 + (1/60)*((time.unixts-2*60*60) - time.unixts_shift.minus.1)/2,
    assessment.type == "Post-Quit Already Slipped" & rawdata.timing==8 ~ 2*60 + (1/60)*((time.unixts-2*60*60) - time.unixts_shift.minus.1)/2,
    # Pre-Quit assessments
    assessment.type == "Pre-Quit Random" & rawdata.timing==8 ~ 2*60 + (1/60)*((time.unixts-2*60*60) - time.unixts_shift.minus.1)/2,
    assessment.type == "Pre-Quit Urge" & rawdata.timing==8 ~ 2*60 + (1/60)*((time.unixts-2*60*60) - time.unixts_shift.minus.1)/2,
    # Else
    TRUE ~ smoking.delta.minutes
  ))

df.all <- df.all %>% 
  mutate(smoking.delta.minutes = if_else(smoking.delta.minutes > time.between.hours*60, time.between.hours*60/2, smoking.delta.minutes))

df.all <- df.all %>%
  mutate(smoking.delta.minutes = case_when(
    # Post-Quit assessments
    assessment.type == "Post-Quit About to Slip Part Two" & assessment.type_shift.minus.1 == "Post-Quit About to Slip Part One" ~ time.between.hours*60/2,
    # Pre-Quit assessments
    assessment.type == "Pre-Quit Smoking Part Two" & assessment.type_shift.minus.1 == "Pre-Quit Smoking Part One" ~ time.between.hours*60/2,
    # Else
    TRUE ~ smoking.delta.minutes
  ))

df.all <- df.all %>%
  mutate(smoking.delta.minutes = case_when(
    # Pre-Quit assessments
    assessment.type == "Pre-Quit Smoking Part Two" & assessment.type_shift.minus.1 != "Pre-Quit Smoking Part One" ~ time.between.hours*60/2,
    # Else
    TRUE ~ smoking.delta.minutes
  ))

df.all <- df.all %>%
  mutate(smoking.delta.minutes = replace(smoking.delta.minutes, smoking.qty==0, NA_real_)) %>%
  mutate(smoking.delta.minutes = replace(smoking.delta.minutes, is.na(smoking.qty), NA_real_))

# Select columns to retain
df.all <- df.all %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts,
         start.study.unixts, quit.unixts, end.study.unixts, 
         sensitivity,
         record.id, ema.order, 
         assessment.type, 
         with.any.response,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         time.between.hours,
         rawdata.indicator, rawdata.qty, rawdata.timing,
         assessment.type.past.1=assessment.type_shift.minus.1,
         time.unixts.past.1=time.unixts_shift.minus.1,
         smoking.qty, smoking.delta.minutes)

# Format dates prior to writing to csv file
# Use argument tz="UTC" or else %H:%M:%S will not be displayed as 00:00:00 for start.study.hrts
# and end.study.hrts, and 04:00:00 will not be displayed for quit.hrts
# This trick prevents R from performing an automatic adjustment of these time variables
# to local time of machine in the output file
df.all[["start.study.hrts"]] <- strftime(df.all[["start.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
df.all[["end.study.hrts"]] <- strftime(df.all[["end.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
df.all[["quit.hrts"]] <- strftime(df.all[["quit.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)

# Save output
write.csv(df.all, file.path(path.pns.output_data, "smoking_outcome.csv"), row.names=FALSE, na = "")

