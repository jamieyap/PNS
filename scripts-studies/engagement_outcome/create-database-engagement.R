###############################################################################
# ABOUT:
# Creates a new dataset beginning with the files
# pre_quit_random_ema.csv and post_quit_random_ema.csv
# both of which are outputs of clean-ema.R
#
# This new dataset
# - selects a subset of EMA items from pre_quit_random_ema.csv and 
#   post_quit_random_ema.csv
# - constructs new variables specific to an analysis having response to next
#   EMA as outcome
# - merges select baseline variables into this new dataset
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

data.baseline <- read.csv(file.path(path.pns.output_data, "pns_curated_baseline_data.csv"), stringsAsFactors = FALSE)
data.prequit.random <- read.csv(file.path(path.pns.output_data, "pre_quit_random_ema.csv"), stringsAsFactors = FALSE)
data.postquit.random <- read.csv(file.path(path.pns.output_data, "post_quit_random_ema.csv"), stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# Pick out columns from baseline, Pre-Quit EMA, and Post-Quit EMA datasets
# and combine the three data frames into one file
# -----------------------------------------------------------------------------
subset.data.baseline <- data.baseline %>% 
  select(id, callnumr, gender, income, hourpart, hourfull, wsws3, wsws12, wsws23)

subset.data.prequit.random <- data.prequit.random %>%
  select(id, callnumr,
         start_study_hrts, end_study_hrts, quit_hrts,
         start_study_unixts, end_study_unixts, quit_unixts,
         record_id, assessment_type, use_as_postquit, sensitivity,
         delivered_hrts, begin_hrts, end_hrts, time_hrts, 
         delivered_unixts, begin_unixts, end_unixts, time_unixts,
         record_status, with_any_response,
         Affect1, Affect2, Affect3, Affect4, Affect5, 
         Affect6, Affect7, Affect8, Affect9, Affect10)

subset.data.postquit.random <- data.postquit.random %>%
  select(id, callnumr,
         start_study_hrts, end_study_hrts, quit_hrts,
         start_study_unixts, end_study_unixts, quit_unixts,
         record_id, assessment_type, use_as_postquit, sensitivity,
         delivered_hrts, begin_hrts, end_hrts, time_hrts, 
         delivered_unixts, begin_unixts, end_unixts, time_unixts,
         record_status, with_any_response,
         Affect1, Affect2, Affect3, Affect4, Affect5, 
         Affect6, Affect7, Affect8, Affect9, Affect10)

subset.data.random <- rbind(subset.data.prequit.random, subset.data.postquit.random)
data.for.analysis <- left_join(x = subset.data.random, y = subset.data.baseline, by = c("id", "callnumr"))

# -----------------------------------------------------------------------------
# Create outcome variable
# -----------------------------------------------------------------------------

# with_any_response_shift_plus_1: does next EMA in the FUTURE have a response to at least one item?
data.for.analysis <- data.for.analysis %>% 
  group_by(id) %>%
  do(GetFutureRecords(df.this.group=., cols.today=c("with_any_response"), h=c(1), this.numeric=c(TRUE)))

# Very very important: before proceeding, order rows according to increasing 
# time_unixts within each person
data.for.analysis <- data.for.analysis %>% arrange(id, time_unixts)

# -----------------------------------------------------------------------------
# Calculate additional time variables related to the current EMA
# -----------------------------------------------------------------------------

# Create variable: day within the study associated with time_unixts
data.for.analysis <- data.for.analysis %>%
  mutate(study_day = 1+floor((time_unixts - start_study_unixts)/(60*60*24)))

# time_unixts_shift_plus_1: what is time_unixts for the next EMA in the FUTURE?
data.for.analysis <- data.for.analysis %>% 
  group_by(id) %>%
  do(GetFutureRecords(df.this.group=., cols.today=c("time_unixts"), h=c(1), this.numeric=c(TRUE)))

# Now, let's create a new variable for time between two consecutive EMAs in hours
# These two consecutive EMAs are: current EMA (kth EMA) and next EMA (k+1 th EMA)
data.for.analysis <- data.for.analysis %>% 
  mutate(hours_between = (time_unixts_shift_plus_1 - time_unixts)/(60*60)) %>%
  select(-time_unixts_shift_plus_1)  # don't need this variable anymore

# -----------------------------------------------------------------------------
# Create features in terms of past history:
# use pattern of response/non-response over time
# -----------------------------------------------------------------------------

# Calculate number of EMAs delivered thus far and
# proportion of EMAs having any response among all EMAs delivered thus far
data.for.analysis <- data.for.analysis %>%
  mutate(ones = 1) %>%
  group_by(id) %>%
  mutate(current_total_delivered = cumsum(ones),
         current_total_responded = cumsum(with_any_response)) %>%
  select(-ones)

# Rearrange columns so that baseline variables appear first
data.for.analysis <- data.for.analysis %>%
  select(id, callnumr, gender, income, hourpart, hourfull, wsws3, wsws12, wsws23,
         start_study_hrts, end_study_hrts, quit_hrts,
         start_study_unixts, end_study_unixts, quit_unixts,
         record_id, assessment_type, use_as_postquit, sensitivity,
         delivered_hrts, begin_hrts, end_hrts, time_hrts, 
         delivered_unixts, begin_unixts, end_unixts, time_unixts, study_day,
         record_status, with_any_response, with_any_response_shift_plus_1, hours_between,
         current_total_delivered, current_total_responded,
         Affect1, Affect2, Affect3, Affect4, Affect5, 
         Affect6, Affect7, Affect8, Affect9, Affect10,
         everything())

# -----------------------------------------------------------------------------
# Write out into new file
# -----------------------------------------------------------------------------

write.csv(data.for.analysis, file.path(path.pns.output_data, "curated_database_engagement.csv"), na="", row.names = FALSE)


