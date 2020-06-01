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
path.pns.code <- Sys.getenv("path.pns.code")
path.shared.code <- Sys.getenv("path.shared.code")
source(file.path(path.shared.code, "shared-data-manip-utils.R"))
source(file.path(path.pns.code, "data-manip-utils.R"))

###############################################################################
# Checks on dates and reported smoking
###############################################################################

#------------------------------------------------------------------------------
# Get dates from Post Quit
#------------------------------------------------------------------------------
postquit.files <- c("Post_Quit_Random.csv",
                    "Post_Quit_Urge.csv",
                    "Post_Quit_About_to_Slip.csv",
                    "Post_Quit_About_to_Slip_Part2.csv",
                    "Post_Quit_Already_Slipped.csv")

list.collect.postquit <- list()

for(i in 1:length(postquit.files)){
  this.file <- postquit.files[i]
  
  df.raw <- read.csv(file.path(path.pns.input_data, this.file), stringsAsFactors = FALSE)
  df.raw.quit.dates <- df.raw %>% 
    select(id = Part_ID, initiated=Initiated, assessment.type=Asse_Name) %>%
    # Note: Need as.POSIXct() as mutate does not support POSIXlt objects
    mutate(initiated = as.POSIXct(strptime(initiated, "%m/%d/%Y %I:%M:%S %p"))) %>%
    arrange(id, initiated) %>%
    group_by(id, assessment.type) %>%
    # earliest.date is of class POSIXct
    summarise(earliest.date = min(initiated))
  
  list.collect.postquit <- append(list.collect.postquit, list(df.raw.quit.dates))
  
  # Remove used variables from environment
  remove(df.raw, df.raw.quit.dates, this.file)
}

collect.postquit <- bind_rows(list.collect.postquit)
collect.postquit <- collect.postquit %>% group_by(id) %>% summarise(postquit.earliest.date=min(earliest.date))

#------------------------------------------------------------------------------
# Get dates from Pre Quit
#------------------------------------------------------------------------------
prequit.files <- c("Pre_Quit_Random.csv",
                   "Pre_Quit_Urge.csv",
                   "Pre_Quit_Smoking.csv",
                   "Pre_Quit_Smoking_Part2.csv")

list.collect.prequit <- list()

for(i in 1:length(prequit.files)){
  this.file <- prequit.files[i]
  
  df.raw <- read.csv(file.path(path.pns.input_data, this.file), stringsAsFactors = FALSE)
  df.raw.quit.dates <- df.raw %>% 
    select(id = Part_ID, initiated=Initiated, assessment.type=Asse_Name) %>%
    # Note: Need as.POSIXct() as mutate does not support POSIXlt objects
    mutate(initiated = as.POSIXct(strptime(initiated, "%m/%d/%Y %I:%M:%S %p"))) %>%
    arrange(id, initiated) %>%
    group_by(id, assessment.type) %>%
    # latest.date is of class POSIXct
    summarise(latest.date = max(initiated))
  
  list.collect.prequit <- append(list.collect.prequit, list(df.raw.quit.dates))
  
  # Remove used variables from environment
  remove(df.raw, df.raw.quit.dates, this.file)
}

collect.prequit <- bind_rows(list.collect.prequit)
collect.prequit <- collect.prequit %>% group_by(id) %>% summarise(prequit.latest.date=max(latest.date))

#------------------------------------------------------------------------------
# Dates in records from study staff
#------------------------------------------------------------------------------
staff.recorded.dates <- read.csv(file.path(path.pns.input_data, "staff_recorded_dates.csv"), stringsAsFactors = FALSE)
staff.recorded.dates <- staff.recorded.dates %>% mutate(EMA_Qday = as.POSIXct(strptime(EMA_Qday, "%m/%d/%Y")))

#------------------------------------------------------------------------------
# Dates in baseline raw data file
#------------------------------------------------------------------------------
df.baseline <- read.csv(file.path(path.pns.input_data, "PNSBaseline.csv"), stringsAsFactors = FALSE)
df.baseline.dates <- df.baseline %>% 
  select(callnumr, quitday) %>%
  mutate(quitday = as.POSIXct(strptime(quitday, "%m/%d/%Y")))

# Remove from environment
remove(df.baseline)

#------------------------------------------------------------------------------
# Merge several streams of data into one data frame
#------------------------------------------------------------------------------
df.alldates <- left_join(x = staff.recorded.dates, y = df.baseline.dates, by = "callnumr")
df.alldates <- left_join(x = df.alldates, y = collect.prequit, by = "id")
df.alldates <- left_join(x = df.alldates, y = collect.postquit, by = "id")

df.alldates <- df.alldates %>%
  mutate(prequit.latest.longformatdate = prequit.latest.date,
         postquit.earliest.longformatdate = postquit.earliest.date) %>%
  rename(prequit.latest.shortformatdate = prequit.latest.date,
         postquit.earliest.shortformatdate = postquit.earliest.date) %>%
  mutate(prequit.latest.shortformatdate = as.POSIXct(strftime(prequit.latest.shortformatdate, "%Y-%m-%d")),
         postquit.earliest.shortformatdate = as.POSIXct(strftime(postquit.earliest.shortformatdate, "%Y-%m-%d")))

df.alldates <- df.alldates %>% filter(!is.na(postquit.earliest.longformatdate))

remove(staff.recorded.dates, df.baseline.dates)

#------------------------------------------------------------------------------
# Plot dates
#------------------------------------------------------------------------------

df.plot.quithour <- df.alldates %>% 
  filter(prequit.latest.shortformatdate == postquit.earliest.shortformatdate) %>%
  mutate(prequit.hour = as.numeric(strftime(prequit.latest.longformatdate, "%H")) + (1/60)*as.numeric(strftime(prequit.latest.longformatdate, "%M")),
         postquit.hour = as.numeric(strftime(postquit.earliest.longformatdate, "%H")) + (1/60)*as.numeric(strftime(postquit.earliest.longformatdate, "%M"))) %>%
  mutate(plotid = 1:nrow(.))

df.plot.quithour %>%
  summarise(after.4am = sum(prequit.hour>4),
            time.between.mean = mean(postquit.hour - prequit.hour),
            time.between.min = min(postquit.hour - prequit.hour),
            time.between.max = max(postquit.hour - prequit.hour),
            prequit.hour.mean = mean(prequit.hour),
            prequit.hour.min = min(prequit.hour),
            prequit.hour.max = max(prequit.hour),
            postquit.hour.mean = mean(postquit.hour),
            postquit.hour.min = min(postquit.hour),
            postquit.hour.max = max(postquit.hour))

jpeg(filename=file.path(path.pns.output_data, "plots_known_QD", "inspect_for_quit_hour.jpg"), width=700, height=700, units="px")
plot(-1, type='n', 
     xlim = c(0,25), ylim = c(1,50), xaxt="n", yaxt="n",
     xlab = "Hour (24-hour clock)", ylab="Participant #")
axis(1, at = seq(0,24,4))
axis(2, at = seq(1,50))

for(i in 1:nrow(df.plot.quithour)){
  segments(x0 = df.plot.quithour[i,"prequit.hour"], y0 = df.plot.quithour[i, "plotid"],
           x1 = df.plot.quithour[i,"postquit.hour"], y1 = df.plot.quithour[i, "plotid"])
}
abline(v = 4, col="blue", lty=2)
text(3,50, "4am", col="blue", cex=0.70)
dev.off()

