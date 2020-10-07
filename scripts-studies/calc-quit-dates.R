library(readxl)

path.pns.output_data <- Sys.getenv("path.pns.output_data")

df.alldates <- read_xlsx(path = file.path(path.pns.output_data, "alldates_annotated.xlsx"), sheet = "alldates_annotated")

df.quit.dates <- data.frame(id = df.alldates$id, 
                            callnumr = df.alldates$callnumr, 
                            start.study.date = as.POSIXct(NA), 
                            quit.date = df.alldates$final.quit.date, 
                            end.study.date = as.POSIXct(NA),
                            exclude = df.alldates$exclude,
                            sensitivity = df.alldates$sensitivity)

# Time of day in start.study.date and end.study.date are set to 12AM
df.quit.dates[["start.study.date"]] <- df.quit.dates[["quit.date"]] - 7*24*60*60
df.quit.dates[["end.study.date"]] <- df.quit.dates[["quit.date"]] + 21*24*60*60
# time of day in quit.date is set to 4am
df.quit.dates[["quit.date"]] <- df.quit.dates[["quit.date"]] + 4*60*60

# Format dates prior to writing to csv file
# Use argument tz="UTC" or else %H:%M:%S will not be displayed as 00:00:00 for start.study.date
# and end.study.date, and 04:00:00 will not be displayed for quit.date in quit_dates_final.csv
# This trick prevents R from performing an automatic adjustment of these time variables
# to local time of machine in the output file quit_dates_final.csv
df.quit.dates[["start.study.date"]] <- strftime(df.quit.dates[["start.study.date"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
df.quit.dates[["end.study.date"]] <- strftime(df.quit.dates[["end.study.date"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)
df.quit.dates[["quit.date"]] <- strftime(df.quit.dates[["quit.date"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = FALSE)

write.csv(df.quit.dates, file.path(path.pns.output_data, "quit_dates_final.csv"), row.names=FALSE, na="")

