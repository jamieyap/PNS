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
df.quit.dates <- read.csv(file.path(path.pns.output_data, "quit_dates_final.csv"), stringsAsFactors = FALSE)
load(file = file.path(path.pns.staged_data, "all_ema_processed.RData"))

#------------------------------------------------------------------------------
# Implement inclusion/exclusion criteria
#------------------------------------------------------------------------------

df.quit.dates <- df.quit.dates %>%
  rename(start.study.hrts = start.study.date, end.study.hrts = end.study.date, quit.hrts = quit.date) %>%
  mutate(start.study.hrts = as.POSIXct(strptime(start.study.hrts, format = "%Y-%m-%d %H:%M:%S")),
         end.study.hrts = as.POSIXct(strptime(end.study.hrts, format = "%Y-%m-%d %H:%M:%S")),
         quit.hrts = as.POSIXct(strptime(quit.hrts, format = "%Y-%m-%d %H:%M:%S"))) %>%
  mutate(start.study.unixts = as.numeric(start.study.hrts),
         end.study.unixts = as.numeric(end.study.hrts),
         quit.unixts = as.numeric(quit.hrts)) %>%
  select(id, callnumr, 
         start.study.hrts, quit.hrts, end.study.hrts,
         start.study.unixts, quit.unixts, end.study.unixts, everything())

list.all <- lapply(list.all, function(this.df, use.quit.dates = df.quit.dates){
  this.df <- left_join(x = use.quit.dates, y = this.df, by = "id")
  this.df <- this.df %>% filter((time.unixts>=start.study.unixts) & (time.unixts<=end.study.unixts))
  this.df <- this.df %>% 
    select(-rawdata.indicator, -rawdata.qty, -rawdata.timing,
           -smoking.qty, -smoking.delta.minutes)
  return(this.df)
})

#------------------------------------------------------------------------------
# Save individual files to output
#------------------------------------------------------------------------------

write.csv(list.all[["Pre-Quit Random"]], file.path(path.pns.output_data, "pre_quit_random_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Pre-Quit Urge"]], file.path(path.pns.output_data, "pre_quit_urge_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Pre-Quit Smoking Part One"]], file.path(path.pns.output_data, "pre_quit_smoking_part_one_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Pre-Quit Smoking Part Two"]], file.path(path.pns.output_data, "pre_quit_smoking_part_two_ema.csv"), na="", row.names = FALSE)

write.csv(list.all[["Post-Quit Random"]], file.path(path.pns.output_data, "post_quit_random_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Post-Quit Urge"]], file.path(path.pns.output_data, "post_quit_urge_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Post-Quit About to Slip Part One"]], file.path(path.pns.output_data, "post_quit_about_to_slip_part_one_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Post-Quit About to Slip Part Two"]], file.path(path.pns.output_data, "post_quit_about_to_slip_part_two_ema.csv"), na="", row.names = FALSE)
write.csv(list.all[["Post-Quit Already Slipped"]], file.path(path.pns.output_data, "post_quit_already_slipped_ema.csv"), na="", row.names = FALSE)

#------------------------------------------------------------------------------
# Transform list.all from a list into a data frame
#------------------------------------------------------------------------------

ema.item.names <- read.csv(file.path(path.pns.output_data, "ema_item_names.csv"), header = TRUE, stringsAsFactors = FALSE)
collect.reshaped.df <- list()
all.names <- ema.item.names$name.new

for(i in 1:length(list.all)){
  current.df <- list.all[[i]]
  
  # Which names in all.names are not current.df column names
  add.these.colnames <- setdiff(all.names, colnames(current.df))
  current.df[,add.these.colnames] <- NA
  
  # Reorder columns
  current.df <- current.df %>% 
    select(id, callnumr, 
           start.study.hrts, quit.hrts, end.study.hrts, 
           start.study.unixts, quit.unixts, end.study.unixts,
           sensitivity, record.id, assessment.type, with.any.response,
           delivered.hrts, begin.hrts, end.hrts, time.hrts,
           delivered.unixts, begin.unixts, end.unixts, time.unixts,
           all.names)
  
  # Collect reshaped data frame
  collect.reshaped.df <- append(collect.reshaped.df , list(current.df))
}

reshaped.df <- do.call(rbind, collect.reshaped.df)

#------------------------------------------------------------------------------
# Merge Smoking Outcome data frame with EMA data frame
#------------------------------------------------------------------------------
df.smoking <- read.csv(file.path(path.pns.output_data, "smoking_outcome.csv"), stringsAsFactors = FALSE)
colnames.merged <- c(colnames(df.smoking), colnames(reshaped.df))
colnames.merged <- unique(colnames.merged)

# Append column names to df.smoking
df.smoking.add.these.colnames <- setdiff(colnames.merged, colnames(df.smoking))
df.smoking[,df.smoking.add.these.colnames] <- NA

# Append column names to df.merged
reshaped.df.add.these.colnames <- setdiff(colnames.merged, colnames(reshaped.df))
reshaped.df[,reshaped.df.add.these.colnames] <- NA

# Merge both data frames
df.smoking <- df.smoking[, colnames.merged]
reshaped.df <- reshaped.df[, colnames.merged]
BIG.df <- rbind(df.smoking, reshaped.df)

#------------------------------------------------------------------------------
# Save merged dataset to csv file
#------------------------------------------------------------------------------
write.csv(reshaped.df, file.path(path.pns.output_data, "merged.csv"), row.names = FALSE, na="")

