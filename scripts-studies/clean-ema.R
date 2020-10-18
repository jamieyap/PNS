###############################################################################
# ABOUT:
# * Complete preparaton by type of EMA
#
# * Focus of this script is not on individual items within an EMA but on
#   identifying which rows which are likely to result from one of the following
#   circumstances:
#      - EMAs (any type) which the software chose to launch and were 
#        successfully delivered/initiated but had indication of 
#        issues relating to the software
#      - EMAs (any type) which the software chose to launch and were 
#        successfully delivered/initiated with no indication of 
#        issues relating to the software
#      - self-initiated EMA button press where the software chose to NOT
#        launch an EMA
#
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

#------------------------------------------------------------------------------
# Create time variables
#------------------------------------------------------------------------------
df.quit.dates <- readRDS(file.path(path.pns.staged_data, "quit_dates_final.RData"))
df.quit.dates <- df.quit.dates %>% rename(start.study.hrts = start.study.date, end.study.hrts = end.study.date, quit.hrts = quit.date) 

# Convert human-readable timestamps to UNIX timestamps
df.quit.dates <- df.quit.dates %>%
  mutate(start.study.unixts = as.numeric(start.study.hrts),
         end.study.unixts = as.numeric(end.study.hrts),
         quit.unixts = as.numeric(quit.hrts)) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts,
         start.study.unixts, quit.unixts, end.study.unixts, everything())

#------------------------------------------------------------------------------
# Read in raw data (already contains time variables at the EMA-level)
# Loading this RData file will add list.all to the global environment
#------------------------------------------------------------------------------
load(file = file.path(path.pns.staged_data, "all_ema_processed.RData"))

#------------------------------------------------------------------------------
# Implement inclusion/exclusion criteria applicable to all datasets
# and create variables to use as filtering criteria in all datasets
#------------------------------------------------------------------------------
list.all.subset <- lapply(list.all, function(this.df, use.quit.dates = df.quit.dates){
  # Merge quit date data with raw data; quit date data contains time variables
  # at the person-level
  this.df <- left_join(x = use.quit.dates, y = this.df, by = "id")
  
  this.df <- this.df %>%
    # Exclude all of a participant's data if they are tagged as exclude==1
    filter(exclude==0) %>% 
    # Exclude EMAs delivered before start study time and EMAs delivered after
    # end study time
    filter((delivered.unixts>=start.study.unixts) & (delivered.unixts<=end.study.unixts))
  
  this.df <- this.df %>%
    # Tag each EMA as being viewed as delivered during the Pre-Quit period
    # or delivered during the Post-Quit Period
    mutate(use.as.postquit = if_else(delivered.unixts>=quit.unixts, 1, 0))
  
  # Clean up
  this.df <- this.df %>% 
    select(-exclude) %>%
    select(id, callnumr, 
           start.study.hrts, quit.hrts, end.study.hrts, 
           start.study.unixts, quit.unixts, end.study.unixts,
           record.id, assessment.type, 
           use.as.postquit, sensitivity,
           delivered.hrts, begin.hrts, end.hrts, time.hrts,
           delivered.unixts, begin.unixts, end.unixts, time.unixts,
           record.status, with.any.response,
           everything())
  
  return(this.df)
})

#------------------------------------------------------------------------------
# Identify EMAs (any type) which the software chose to launch and were 
# successfully delivered/initiated but had some indication of 
# issues relating to the software
#------------------------------------------------------------------------------

# POST-QUIT MODE EMAs #################

list.all.subset[["Post-Quit Random"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_with_issues_rdata_files", paste("delivered_with_issues_", "post_quit_random_ema.RData", sep="")))
  
list.all.subset[["Post-Quit Urge"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_with_issues_rdata_files", paste("delivered_with_issues_", "post_quit_urge_ema.RData", sep="")))

list.all.subset[["Post-Quit About to Slip Part One"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_with_issues_rdata_files", paste("delivered_with_issues_", "post_quit_about_to_slip_part_one_ema.RData", sep="")))

list.all.subset[["Post-Quit About to Slip Part Two"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_with_issues_rdata_files", paste("delivered_with_issues_", "post_quit_about_to_slip_part_two_ema.RData", sep="")))

list.all.subset[["Post-Quit Already Slipped"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_with_issues_rdata_files", paste("delivered_with_issues_", "post_quit_already_slipped_ema.RData", sep="")))

# PRE-QUIT MODE EMAs ##################

list.all.subset[["Pre-Quit Random"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_with_issues_rdata_files", paste("delivered_with_issues_", "pre_quit_random_ema.RData", sep="")))

list.all.subset[["Pre-Quit Urge"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_with_issues_rdata_files", paste("delivered_with_issues_", "pre_quit_urge_ema.RData", sep="")))

list.all.subset[["Pre-Quit Smoking Part One"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_with_issues_rdata_files", paste("delivered_with_issues_", "pre_quit_smoking_part_one_ema.RData", sep="")))

list.all.subset[["Pre-Quit Smoking Part Two"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_with_issues_rdata_files", paste("delivered_with_issues_", "pre_quit_smoking_part_two_ema.RData", sep="")))

#------------------------------------------------------------------------------
# Identify EMAs (any type) which the software chose to launch and were 
# successfully delivered/initiated and had no indication of 
# issues relating to the software
#------------------------------------------------------------------------------

# POST-QUIT MODE EMAs #################

list.all.subset[["Post-Quit Random"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", paste("delivered_no_issues_", "post_quit_random_ema.RData", sep="")))

list.all.subset[["Post-Quit Urge"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", paste("delivered_no_issues_", "post_quit_urge_ema.RData", sep="")))

list.all.subset[["Post-Quit About to Slip Part One"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", paste("delivered_no_issues_", "post_quit_about_to_slip_part_one_ema.RData", sep="")))

list.all.subset[["Post-Quit About to Slip Part Two"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", paste("delivered_no_issues_", "post_quit_about_to_slip_part_two_ema.RData", sep="")))

list.all.subset[["Post-Quit Already Slipped"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", paste("delivered_no_issues_", "post_quit_already_slipped_ema.RData", sep="")))

# PRE-QUIT MODE EMAs

list.all.subset[["Pre-Quit Random"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", paste("delivered_no_issues_", "pre_quit_random_ema.RData", sep="")))

list.all.subset[["Pre-Quit Urge"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", paste("delivered_no_issues_", "pre_quit_urge_ema.RData", sep="")))

list.all.subset[["Pre-Quit Smoking Part One"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", paste("delivered_no_issues_", "pre_quit_smoking_part_one_ema.RData", sep="")))

list.all.subset[["Pre-Quit Smoking Part Two"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) %>%
  saveRDS(., file.path(path.pns.staged_data, "delivered_no_issues_rdata_files", paste("delivered_no_issues_", "pre_quit_smoking_part_two_ema.RData", sep="")))

#------------------------------------------------------------------------------
# Identify self-initiated EMA (any type) button press where the software chose 
# to NOT launch an EMA
#------------------------------------------------------------------------------

# POST-QUIT MODE EMAs #################
list.all.subset[["Post-Quit Urge"]] %>% 
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  saveRDS(., file.path(path.pns.staged_data, "button_press_rdata_files", paste("button_press_", "post_quit_urge_ema.RData", sep="")))

list.all.subset[["Post-Quit About to Slip Part One"]] %>% 
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  saveRDS(., file.path(path.pns.staged_data, "button_press_rdata_files", paste("button_press_", "post_quit_about_to_slip_part_one_ema.RData", sep="")))

list.all.subset[["Post-Quit About to Slip Part Two"]] %>% 
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  saveRDS(., file.path(path.pns.staged_data, "button_press_rdata_files", paste("button_press_", "post_quit_about_to_slip_part_two_ema.RData", sep="")))

list.all.subset[["Post-Quit Already Slipped"]] %>% 
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  saveRDS(., file.path(path.pns.staged_data, "button_press_rdata_files", paste("button_press_", "post_quit_already_slipped_ema.RData", sep="")))

# PRE-QUIT MODE EMAs ##################
list.all.subset[["Pre-Quit Urge"]] %>% 
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  saveRDS(., file.path(path.pns.staged_data, "button_press_rdata_files", paste("button_press_", "pre_quit_urge_ema.RData", sep="")))

list.all.subset[["Pre-Quit Smoking Part One"]] %>% 
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  saveRDS(., file.path(path.pns.staged_data, "button_press_rdata_files", paste("button_press_", "pre_quit_smoking_part_one_ema.RData", sep="")))

list.all.subset[["Pre-Quit Smoking Part Two"]] %>% 
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  saveRDS(., file.path(path.pns.staged_data, "button_press_rdata_files", paste("button_press_", "pre_quit_smoking_part_two_ema.RData", sep="")))




