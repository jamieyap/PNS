###############################################################################
# ABOUT:
# * Create database of smoking information using EMA data
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

#------------------------------------------------------------------------------
# Read in output of clean-ema.R and store each data frame in list.all
#------------------------------------------------------------------------------
list.all <- list()

# Random EMA
list.all[["Post-Quit Random"]] <- readRDS(file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", "delivered_no_issues_post_quit_random_ema.RData"))
list.all[["Pre-Quit Random"]] <- readRDS(file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", "delivered_no_issues_pre_quit_random_ema.RData"))

# Urge EMA
list.all[["Post-Quit Urge"]] <- readRDS(file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", "delivered_no_issues_post_quit_urge_ema.RData"))
list.all[["Pre-Quit Urge"]] <- readRDS(file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", "delivered_no_issues_pre_quit_urge_ema.RData"))

# Smoking Part One EMA
list.all[["Post-Quit About to Slip Part One"]] <- readRDS(file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", "delivered_no_issues_post_quit_about_to_slip_part_one_ema.RData"))
list.all[["Pre-Quit Smoking Part One"]] <- readRDS(file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", "delivered_no_issues_pre_quit_smoking_part_one_ema.RData"))

# Smoking Part Two EMA
list.all[["Post-Quit About to Slip Part Two"]] <- readRDS(file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", "delivered_no_issues_post_quit_about_to_slip_part_two_ema.RData"))
list.all[["Pre-Quit Smoking Part Two"]] <- readRDS(file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", "delivered_no_issues_pre_quit_smoking_part_two_ema.RData"))

# Already Slipped EMA
list.all[["Post-Quit Already Slipped"]] <- readRDS(file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", "delivered_no_issues_post_quit_already_slipped_ema.RData"))

#------------------------------------------------------------------------------
# Exclude assessments meeting the criteria below from creating a curated
# data base of smoking information based on EMAs
#------------------------------------------------------------------------------

list.all <- lapply(list.all, function(dat){
  dat <- dat %>% filter(with.any.response==1)
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
           start.study.hrts, quit.hrts, end.study.hrts, 
           start.study.unixts, quit.unixts, end.study.unixts,
           sensitivity,
           record.id, assessment.type, 
           use.as.postquit,
           with.any.response,
           delivered.hrts, begin.hrts, end.hrts, time.hrts,
           delivered.unixts, begin.unixts, end.unixts, time.unixts,
           rawdata.indicator, rawdata.qty, rawdata.timing,
           smoking.qty, smoking.indicator) %>%
    mutate(smoking.delta.minutes = NA_real_)
  
  return(dat)
})

df.all <- bind_rows(list.all)

# Construct id for each smoking interval
df.all <- df.all %>% 
  mutate(ones = 1) %>%
  arrange(id, time.unixts) %>%
  group_by(id) %>%
  mutate(ema.order = cumsum(ones)) %>%
  select(-ones)

# For each row, get time variables, assessment type, and record id of earlier row
df.all <- df.all %>%   
  arrange(id, time.unixts) %>%
  group_by(id) %>%
  do(GetPastRecords(df.this.group = ., 
                    cols.today = c("time.hrts","time.unixts","assessment.type","record.id"), 
                    h = c(1,1,1,1), 
                    this.numeric = c(FALSE,TRUE,FALSE,FALSE))) 

# Calculate number of hours elapsed between current EMA and earlier EMA
df.all <- df.all %>%
  mutate(time.unixts_shift_minus_1 = if_else(ema.order==1, as.double(start.study.unixts), time.unixts_shift_minus_1)) %>%
  mutate(hours.between.past.and.present = (time.unixts - time.unixts_shift_minus_1)/(60*60))

source(file.path(path.pns.code, "smoking_outcome", "rules-smoking-timing.R"))

#------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------
saveRDS(df.all, file.path(path.pns.staged_data, "curated_smoking_database.RData"))

