###############################################################################
# ABOUT:
# * Complete preparaton by type of EMA
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

df.quit.dates <- readRDS(file = file.path(path.pns.staged_data, "quit_dates_final.RData"))
df.quit.dates <- df.quit.dates %>% 
  rename(start.study.hrts = start.study.date, 
         end.study.hrts = end.study.date, 
         quit.hrts = quit.date) 

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
#------------------------------------------------------------------------------
list.all <- readRDS(file = file.path(path.pns.staged_data, "all_ema_processed.RData"))

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
# successfully delivered/initiated and had no indication of 
# issues relating to the software
#------------------------------------------------------------------------------

list.clean.launched <- list()

# POST-QUIT MODE EMAs #################

list.clean.launched[["Post-Quit Random"]] <- list.all.subset[["Post-Quit Random"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out"))

list.clean.launched[["Post-Quit Urge"]] <- list.all.subset[["Post-Quit Urge"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out"))

list.clean.launched[["Post-Quit About to Slip Part One"]] <- list.all.subset[["Post-Quit About to Slip Part One"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) 

list.clean.launched[["Post-Quit About to Slip Part Two"]] <- list.all.subset[["Post-Quit About to Slip Part Two"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) 

list.clean.launched[["Post-Quit Already Slipped"]] <- list.all.subset[["Post-Quit Already Slipped"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) 

# PRE-QUIT MODE EMAs ##################

list.clean.launched[["Pre-Quit Random"]] <- list.all.subset[["Pre-Quit Random"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) 

list.clean.launched[["Pre-Quit Urge"]] <- list.all.subset[["Pre-Quit Urge"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) 

list.clean.launched[["Pre-Quit Smoking Part One"]] <- list.all.subset[["Pre-Quit Smoking Part One"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out")) 

list.clean.launched[["Pre-Quit Smoking Part Two"]] <- list.all.subset[["Pre-Quit Smoking Part Two"]] %>% 
  filter(with.any.response==1 | (with.any.response==0 & record.status=="Incomplete/Timed Out"))

saveRDS(list.clean.launched, file.path(path.pns.staged_data, "clean_launched.RData"))

#------------------------------------------------------------------------------
# Identify EMAs (any type) which the software chose to launch and were 
# successfully delivered/initiated but had some indication of 
# issues relating to the software
#------------------------------------------------------------------------------

list.dirty.launched <- list()

# POST-QUIT MODE EMAs #################

list.dirty.launched[["Post-Quit Random"]] <- list.all.subset[["Post-Quit Random"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

list.dirty.launched[["Post-Quit Urge"]] <- list.all.subset[["Post-Quit Urge"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

list.dirty.launched[["Post-Quit About to Slip Part One"]] <- list.all.subset[["Post-Quit About to Slip Part One"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

list.dirty.launched[["Post-Quit About to Slip Part Two"]] <- list.all.subset[["Post-Quit About to Slip Part Two"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

list.dirty.launched[["Post-Quit Already Slipped"]] <- list.all.subset[["Post-Quit Already Slipped"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

# PRE-QUIT MODE EMAs ##################

list.dirty.launched[["Pre-Quit Random"]] <- list.all.subset[["Pre-Quit Random"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

list.dirty.launched[["Pre-Quit Urge"]] <- list.all.subset[["Pre-Quit Urge"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

list.dirty.launched[["Pre-Quit Smoking Part One"]] <- list.all.subset[["Pre-Quit Smoking Part One"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

list.dirty.launched[["Pre-Quit Smoking Part Two"]] <- list.all.subset[["Pre-Quit Smoking Part Two"]] %>% 
  filter((with.any.response==0 & record.status=="Completed") | (with.any.response==0 & record.status=="FRAGMENT RECORD")) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

df.dirty.launched <- bind_rows(list.dirty.launched)
saveRDS(df.dirty.launched, file.path(path.pns.staged_data, "dirty_launched.RData"))

#------------------------------------------------------------------------------
# Identify self-initiated EMA (any type) button press where the software chose 
# to NOT launch an EMA
#------------------------------------------------------------------------------

list.bp <- list()

# POST-QUIT MODE EMAs #################
list.bp[["Post-Quit Urge"]] <- list.all.subset[["Post-Quit Urge"]] %>% 
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

list.bp[["Post-Quit About to Slip Part One"]] <- list.all.subset[["Post-Quit About to Slip Part One"]] %>% 
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

list.bp[["Post-Quit Already Slipped"]] <- list.all.subset[["Post-Quit Already Slipped"]] %>% 
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

# PRE-QUIT MODE EMAs ##################
list.bp[["Pre-Quit Urge"]] <- list.all.subset[["Pre-Quit Urge"]] %>% 
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

list.bp[["Pre-Quit Smoking Part One"]] <- list.all.subset[["Pre-Quit Smoking Part One"]] %>% 
  filter(with.any.response==0 & record.status=="CANCELLED") %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts, 
         start.study.unixts, quit.unixts, end.study.unixts,
         record.id, assessment.type, 
         use.as.postquit, sensitivity,
         delivered.hrts, begin.hrts, end.hrts, time.hrts,
         delivered.unixts, begin.unixts, end.unixts, time.unixts,
         record.status, with.any.response)

df.bp <- bind_rows(list.bp)
saveRDS(df.bp, file.path(path.pns.staged_data, "buttonpress.RData"))





