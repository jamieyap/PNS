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

# all_ema_processed.RData is the output of the script get-ema-item-responses.R
load(file = file.path(path.pns.staged_data, "all_ema_processed.RData"))

list.all <- lapply(list.all, function(this.df, use.quit.dates = df.quit.dates){
  # call left_join so that only individuals whose ID's are in quit_dates_final.csv
  # will be retained
  this.df <- left_join(x = use.quit.dates, y = this.df, by = "id")
  this.df <- this.df %>% 
    select(id, callnumr, 
           start.study.hrts, quit.hrts, end.study.hrts, 
           start.study.unixts, quit.unixts, end.study.unixts,
           sensitivity,
           record.id, assessment.type, 
           with.any.response,
           everything()) %>%
    # participants not having a particular EMA type will still have 
    # a row in this.df; this row will contain their ID but have a missing value
    # for record.id
    # note: in the file get-ema-data-frames-by-type.R, these columns were
    # taken care of (removed) upon execution of
    # the step filter((delivered.unixts>=start.study.unixts) & (delivered.unixts<=end.study.unixts))
    # since these rows will have missing values in delivered.unixts,
    # this criterion will return FALSE for these rows and hence will be dropped
    filter(!is.na(record.id)) 
  
  return(this.df)
})

#------------------------------------------------------------------------------
# Calculate time elapsed between begin.unixts and delivered.unixts
#------------------------------------------------------------------------------

list.all <- lapply(list.all, function(this.df){
  this.df <- this.df %>% 
    mutate(delay = begin.unixts - delivered.unixts) %>%
    select(id, record.id, assessment.type, delivered.unixts, begin.unixts, end.unixts, delay)
  return(this.df)
})


df.all <- bind_rows(list.all)

#------------------------------------------------------------------------------
# Calculate summary statistics in time variables
#------------------------------------------------------------------------------

df.summary <- df.all %>% 
  group_by(assessment.type) %>%
  summarise(count.geq.than.0 = sum(1*(delay >= 0), na.rm=TRUE),
            count.less.than.0 = sum(1*(delay < 0), na.rm=TRUE),
            q0 = quantile(delay, probs=0, na.rm=TRUE),
            q50 = quantile(delay, probs=.5, na.rm=TRUE),
            q99 = quantile(delay, probs=.95, na.rm=TRUE),
            q100 = quantile(delay, probs=1, na.rm=TRUE))

#------------------------------------------------------------------------------
# Write results to a csv file
#------------------------------------------------------------------------------

write.csv(df.summary, file.path(path.pns.output_data, "checks_output/summarise_delay.csv"), row.names = FALSE)

