###############################################################################
# ABOUT:
# * Complete preparaton for each type of EMA
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
# Implement inclusion/exclusion criteria
#------------------------------------------------------------------------------

load(file = file.path(path.pns.staged_data, "all_ema_processed.RData"))

list.all <- lapply(list.all, function(this.df, use.quit.dates = df.quit.dates){
  this.df <- left_join(x = use.quit.dates, y = this.df, by = "id")
  this.df <- this.df %>% filter((delivered.unixts>=start.study.unixts) & (delivered.unixts<=end.study.unixts))
  this.df <- this.df %>% mutate(use.as.postquit = if_else(delivered.unixts>=quit.unixts, 1, 0))
  this.df <- this.df %>% 
    select(-rawdata.indicator, -rawdata.qty, -rawdata.timing,
           -smoking.qty) %>% 
    select(id, callnumr, 
           start.study.hrts, quit.hrts, end.study.hrts, 
           start.study.unixts, quit.unixts, end.study.unixts,
           sensitivity,
           record.id, assessment.type, 
           use.as.postquit,
           with.any.response,
           everything())
  
  return(this.df)
})

#------------------------------------------------------------------------------
# Save individual files to output
#------------------------------------------------------------------------------

for(i in 1:length(list.all)){
  
  this.df <- list.all[[i]]
  
  # Format dates prior to writing to csv file
  # Use argument tz="UTC" or else %H:%M:%S will not be displayed as 00:00:00 for start.study.hrts
  # and end.study.hrts, and 04:00:00 will not be displayed for quit.hrts
  # This trick prevents R from performing an automatic adjustment of these time variables
  # to local time of machine in the output file
  this.df[["start.study.hrts"]] <- strftime(this.df[["start.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  this.df[["end.study.hrts"]] <- strftime(this.df[["end.study.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  this.df[["quit.hrts"]] <- strftime(this.df[["quit.hrts"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
  
  list.all[[i]] <- this.df
}

write.csv(list.all[["Pre-Quit Random"]], file.path(path.pns.output_data, "pre_quit_random_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Pre-Quit Urge"]], file.path(path.pns.output_data, "pre_quit_urge_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Pre-Quit Smoking Part One"]], file.path(path.pns.output_data, "pre_quit_smoking_part_one_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Pre-Quit Smoking Part Two"]], file.path(path.pns.output_data, "pre_quit_smoking_part_two_ema.csv"), na="", row.names = FALSE)

write.csv(list.all[["Post-Quit Random"]], file.path(path.pns.output_data, "post_quit_random_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Post-Quit Urge"]], file.path(path.pns.output_data, "post_quit_urge_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Post-Quit About to Slip Part One"]], file.path(path.pns.output_data, "post_quit_about_to_slip_part_one_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Post-Quit About to Slip Part Two"]], file.path(path.pns.output_data, "post_quit_about_to_slip_part_two_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Post-Quit Already Slipped"]], file.path(path.pns.output_data, "post_quit_already_slipped_ema.csv"), na="", row.names = FALSE)


