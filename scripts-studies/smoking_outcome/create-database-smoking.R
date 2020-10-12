###############################################################################
# ABOUT:
# * Create database of smoking information using EMA data
# * Run clean-ema.R prior to running this script
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
# Read in output of clean-ema.R and store each data frame in list.all
#------------------------------------------------------------------------------

list.all <- list()
list.all[["Pre-Quit Random"]] <- read.csv(file.path(path.pns.output_data, "pre_quit_random_ema.csv"), stringsAsFactors = FALSE)
list.all[["Post-Quit Random"]] <- read.csv(file.path(path.pns.output_data, "post_quit_random_ema.csv"), stringsAsFactors = FALSE)
list.all[["Pre-Quit Urge"]] <- read.csv(file.path(path.pns.output_data, "pre_quit_urge_ema.csv"), stringsAsFactors = FALSE)
list.all[["Post-Quit Urge"]] <- read.csv(file.path(path.pns.output_data, "post_quit_urge_ema.csv"), stringsAsFactors = FALSE)
list.all[["Pre-Quit Smoking Part One"]] <- read.csv(file.path(path.pns.output_data, "pre_quit_smoking_part_one_ema.csv"), stringsAsFactors = FALSE)
list.all[["Post-Quit About to Slip Part One"]] <- read.csv(file.path(path.pns.output_data, "post_quit_about_to_slip_part_one_ema.csv"), stringsAsFactors = FALSE)
list.all[["Pre-Quit Smoking Part Two"]] <- read.csv(file.path(path.pns.output_data, "pre_quit_smoking_part_two_ema.csv"), stringsAsFactors = FALSE)
list.all[["Post-Quit About to Slip Part Two"]] <- read.csv(file.path(path.pns.output_data, "post_quit_about_to_slip_part_two_ema.csv"), stringsAsFactors = FALSE)
list.all[["Post-Quit Already Slipped"]] <- read.csv(file.path(path.pns.output_data, "post_quit_already_slipped_ema.csv"), stringsAsFactors = FALSE)

#------------------------------------------------------------------------------
# Exclude assessments meeting the criteria below from creating a curated
# data base of smoking information based on EMAs
#------------------------------------------------------------------------------

list.all <- lapply(list.all, function(dat){
  dat <- dat %>% 
    filter(!(with_any_response==0 & assessment_type=="Pre-Quit Random")) %>%
    filter(!(with_any_response==0 & assessment_type=="Post-Quit Random")) 
  
  return(dat)
})

#------------------------------------------------------------------------------
# Implement decision rules for constructing curated database of smoking
# information based on EMAs; these are decision rules on
#   - identifying variables containing smoking infromation across
#     various types of EMA
#   - rules on constructing a count variable on number of cigarettes smoked
#     within a specified time period
#   - rules on constructing a binary variable on whether any cigarettes
#     have been smoked within a specified time period
#------------------------------------------------------------------------------

source(file.path(path.pns.code, "smoking_outcome", "identify-smoking-vars.R"))
source(file.path(path.pns.code, "smoking_outcome", "rules-smoking-quantity.R"))
source(file.path(path.pns.code, "smoking_outcome", "rules-smoking-indicator.R"))

#------------------------------------------------------------------------------
# Set up data frame prior to implementing decision rules on constructing
# a variable on timing of cigarette smoking
#------------------------------------------------------------------------------

list.all <- lapply(list.all, function(dat){
  dat <- dat %>% 
    select(id, 
           callnumr, 
           start_study_hrts, quit_hrts, end_study_hrts, 
           start_study_unixts, quit_unixts, end_study_unixts,
           sensitivity,
           record_id, assessment_type, 
           use_as_postquit,
           with_any_response,
           delivered_hrts, begin_hrts, end_hrts, time_hrts,
           delivered_unixts, begin_unixts, end_unixts, time_unixts,
           rawdata_indicator, rawdata_qty, rawdata_timing,
           smoking_qty, smoking_indicator) %>%
    mutate(smoking_delta_minutes = NA_real_)
  
  return(dat)
})

df.all <- bind_rows(list.all)

# Construct id for each smoking interval
df.all <- df.all %>% 
  mutate(ones = 1) %>%
  arrange(id, time_unixts) %>%
  group_by(id) %>%
  mutate(ema_order = cumsum(ones)) %>%
  select(-ones)

# For each row, get time variables, assessment type, and record id of earlier row
df.all <- df.all %>%   
  arrange(id, time_unixts) %>%
  group_by(id) %>%
  do(GetPastRecords(df.this.group = ., 
                    cols.today = c("time_hrts","time_unixts","assessment_type","record_id"), 
                    h = c(1,1,1,1), 
                    this.numeric = c(FALSE,TRUE,FALSE,FALSE))) 

# Calculate number of hours elapsed between current EMA and earlier EMA
df.all <- df.all %>%
  mutate(time_unixts_shift_minus_1 = if_else(ema_order==1, as.double(start_study_unixts), time_unixts_shift_minus_1)) %>%
  mutate(hours_between_past_and_present = (time_unixts - time_unixts_shift_minus_1)/(60*60))

source(file.path(path.pns.code, "smoking_outcome", "rules-smoking-timing.R"))

#------------------------------------------------------------------------------
# Select columns
#------------------------------------------------------------------------------

write.csv(df.all, file.path(path.pns.output_data, "curated_database_smoking.csv"), na="", row.names = FALSE)


