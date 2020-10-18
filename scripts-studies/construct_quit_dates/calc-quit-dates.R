###############################################################################
# ABOUT:
#   * alldates_annotated.xlsx contains information on each individual's
#     'Working Quit Date' and whether an individual will be included in
#     the overall analytic datasets (via the variable 'exclude') and
#     whether an individual will be included in sensitivity analysis
#     (via the variable 'sensitivity')
#   
#   * The first few columns of alldates_annotated.xlsx were created
#     using calc-candidate-dates.R and the following scripts were used
#     to determine 'Working Quit Date' (the value of the variable 'quit.date')
# 
#   * From 'Working Quit Date', the dates viewed as an individual's 
#     beginning and end times in the study were calculated; these are then
#     named as 'start.study.date' and 'end.study.date', respectively
###############################################################################

library(readxl)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.staged_data <- Sys.getenv("path.pns.staged_data")

df.alldates <- read_xlsx(path = file.path(path.pns.input_data, "alldates_annotated.xlsx"), sheet = "alldates_annotated")

df.quit.dates <- data.frame(id = df.alldates$id, 
                            callnumr = df.alldates$callnumr, 
                            start.study.date = as.POSIXct(NA), 
                            quit.date = df.alldates$final.quit.date, 
                            end.study.date = as.POSIXct(NA),
                            exclude = df.alldates$exclude,
                            sensitivity = df.alldates$sensitivity,
                            stringsAsFactors = FALSE)

# Time of day in start.study.date and end.study.date are set to 12AM
df.quit.dates[["start.study.date"]] <- df.quit.dates[["quit.date"]] - 7*24*60*60  # seven-day Pre-Quit period
df.quit.dates[["end.study.date"]] <- df.quit.dates[["quit.date"]] + 21*24*60*60   # twenty-one-day Post-Quit Period
# time of day in quit.date is set to 4am
df.quit.dates[["quit.date"]] <- df.quit.dates[["quit.date"]] + 4*60*60

# Save dates
saveRDS(df.quit.dates, file.path(path.pns.staged_data, "quit_dates_final.RData"))

