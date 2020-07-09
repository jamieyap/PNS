library(readxl)
path.pns.output_data <- Sys.getenv("path.pns.output_data")

df.alldates <- read_xlsx(path = file.path(path.pns.output_data, "alldates_annotated.xlsx"), sheet = "alldates_annotated")
df.alldates <- df.alldates[which(df.alldates$exclude==0),]

df.quit.dates <- data.frame(id = df.alldates$id, 
                            callnumr = df.alldates$callnumr, 
                            start.study.date = as.POSIXct(NA), 
                            quit.date = df.alldates$final.quit.date, 
                            end.study.date = as.POSIXct(NA),
                            sensitivity = df.alldates$sensitivity)

# add 10e-6 to start.study.date and end.study.date so that HH:MM:SS=00:00:00 will be displayed
# time of day in start.study.date and end.study.date are set to 12AM
df.quit.dates[["start.study.date"]] <- df.quit.dates[["quit.date"]] - 7*24*60*60 + 1/1000000  
df.quit.dates[["end.study.date"]] <- df.quit.dates[["quit.date"]] + 21*24*60*60 + 1/1000000
# time of day in quit.date is set to 4am
df.quit.dates[["quit.date"]] <- df.quit.dates[["quit.date"]] + 4*60*60

write.csv(df.quit.dates, file.path(path.pns.output_data, "quit_dates_final.csv"), row.names=FALSE)

