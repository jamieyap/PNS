###############################################################################
# ABOUT:
#   * Calculate number of EMAs in each possible combination of values of
#     with.any.response and record.status
#   * with.any.response can take one of two possible values: 0 and 1
#   * record.status can take on one of four possible values:
#       - 'Completed', 'FRAGMENT RECORD', 'CANCELLED', 'Incomplete/Timed Out'
#   * These summary statistics are calculated twice: before and after applying
#     the following exclusion rules:
#       - excluding EMAs falling prior to beginning of the study and after
#         end of study
#       - excluding all EMAs from individuals who will not be included in 
#         all data analysis (e.g., individuals who do not have any
#         EMAs which were delivered in Post-Quit mode)
###############################################################################

library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.staged_data <- Sys.getenv("path.pns.staged_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
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
  
  return(this.df)
})

names.ema.types <- names(list.all)

#------------------------------------------------------------------------------
# Calculate summary statistics for each type of EMA before applying exclusion 
# criteria and after applying exclusion criteria
#------------------------------------------------------------------------------

list.summarise <- list()

for(i in 1:length(names.ema.types)){
  current.ema.type <- names.ema.types[i]
  df.raw <- list.all[[current.ema.type]]
  df.out <- list.all.subset[[current.ema.type]]
  
  df.tabulate.raw <- df.raw %>% 
    group_by(with.any.response) %>% 
    summarise(attempted = n(), 
              completed.raw = sum(record.status=="Completed"),
              cancelled.raw = sum(record.status=="CANCELLED"), 
              timedout.raw = sum(record.status=="Incomplete/Timed Out"), 
              fragment.raw = sum(record.status=="FRAGMENT RECORD"))
  
  df.tabulate.out <- df.out %>% 
    group_by(with.any.response) %>% 
    summarise(success = n(), 
              completed.out = sum(record.status=="Completed"),
              cancelled.out = sum(record.status=="CANCELLED"), 
              timedout.out = sum(record.status=="Incomplete/Timed Out"), 
              fragment.out = sum(record.status=="FRAGMENT RECORD"))
  
  df.tabulate <- cbind(df.tabulate.raw, df.tabulate.out[,2:6])
  df.tabulate[["percent.success"]] <- df.tabulate.out[["success"]]/df.tabulate.raw[["attempted"]]
  df.tabulate[["percent.success"]] <- round(100*df.tabulate[["percent.success"]], digits=1)
  df.tabulate[["assessment.type"]] <- current.ema.type
  df.tabulate <- df.tabulate %>% select(assessment.type, everything())
  
  list.summarise <- append(list.summarise, list(df.tabulate))
}

df.summarise <- bind_rows(list.summarise)

View(df.summarise)

write.csv(df.summarise, file.path(path.pns.output_data, "summary_stats_completion_status.csv"), row.names = FALSE, na="")


