# About this script: In this script, we perform a series of data curation tasks 
# specific to data from the PNS study

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------

library(assertthat)
library(dplyr)
path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
source(file.path(path.code,"data-manip-utils.R"))
use.study <- "PNS"

#------------------------------------------------------------------------------
# Setup PNS dataset. These are common data manipulation tasks for all
# PNS datasets for analyses
#------------------------------------------------------------------------------
source(file.path(path.code,"pns-run-curation/pns-setup.R"))

#------------------------------------------------------------------------------
# Specify relevant questions for each analysis dataset below
#------------------------------------------------------------------------------

# Specify columns to select from different datasets
these.cols.post.quit.about.to.slip <- c("id", 
                                        "with.any.response",
                                        "engaged.yes",
                                        "record.status",
                                        "assessment.type", 
                                        "delivered.hrts",
                                        "delivered.unixts",
                                        "assessment.hrts",
                                        "assessment.unixts",
                                        "start.clock", 
                                        "end.clock")

these.cols.post.quit.about.to.slip.part2 <- c("id", 
                                              "with.any.response",
                                              "engaged.yes",
                                              "record.status", 
                                              "assessment.type", 
                                              "delivered.hrts",
                                              "delivered.unixts", 
                                              "assessment.hrts",
                                              "assessment.unixts",
                                              "start.clock", 
                                              "end.clock",
                                              "CigJustNow_PostQ_Slip2") # How many cigarettes did you just smoke?

these.cols.post.quit.already.slipped <- c("id", 
                                          "with.any.response", 
                                          "engaged.yes",
                                          "record.status", 
                                          "assessment.type",  
                                          "delivered.hrts",
                                          "delivered.unixts",
                                          "assessment.hrts",
                                          "assessment.unixts",
                                          "start.clock", 
                                          "end.clock",
                                          "HowManyCig", # How many cigarettes did you smoke during this slip?
                                          "LastCig") # How long ago did you smoke your last cigarette?

these.cols.post.quit.random <- c("id", 
                                 "with.any.response", 
                                 "engaged.yes",
                                 "record.status", 
                                 "assessment.type",  
                                 "delivered.hrts",
                                 "delivered.unixts",
                                 "assessment.hrts",
                                 "assessment.unixts",
                                 "start.clock", 
                                 "end.clock",
                                 "PostQRSmoking1",  # Since the last computer recording, have you smoked any cigarettes that you did not record in the computer?
                                 "Smoking2_PostQ_Random",  # How many cigarettes did you smoke that you did not record?
                                 "Smoking3") #  How long ago did you smoke the most recent cigarette that you did not record? 

these.cols.post.quit.urge <- c("id", 
                               "with.any.response", 
                               "engaged.yes",
                               "record.status", 
                               "assessment.type",  
                               "delivered.hrts",
                               "delivered.unixts",
                               "assessment.hrts",
                               "assessment.unixts",
                               "start.clock", 
                               "end.clock",
                               "SmPostQU1",  # Since the last computer recording, have you smoked any cigarettes that you did not record in the computer?
                               "Smoking2_PostQ_Urge", # How many cigarettes did you smoke that you did not record?
                               "Smoking3") # How long ago did you smoke the most recent cigarette that you did not record? 

#------------------------------------------------------------------------------
# Merge observations from different types of EMAs into one dataset
#------------------------------------------------------------------------------

# Select subset of columns from raw data and clean these columns up
df.post.quit.about.to.slip <- post.quit.about.to.slip %>% 
  select(these.cols.post.quit.about.to.slip) %>%
  mutate(smoking.indicator = NA, smoking.qty = NA, smoking.timing = NA)

df.post.quit.about.to.slip.part2 <- post.quit.about.to.slip.part2 %>% 
  select(these.cols.post.quit.about.to.slip.part2) %>%
  rename(smoking.qty = CigJustNow_PostQ_Slip2) %>%
  mutate(smoking.indicator = NA, smoking.timing = NA)

df.post.quit.already.slipped <- post.quit.already.slipped %>% 
  select(these.cols.post.quit.already.slipped) %>%
  rename(smoking.qty = HowManyCig, smoking.timing = LastCig) %>%
  mutate(smoking.indicator = NA)

df.post.quit.random <- post.quit.random %>% 
  select(these.cols.post.quit.random) %>%
  rename(smoking.indicator = PostQRSmoking1, smoking.qty = Smoking2_PostQ_Random, smoking.timing = Smoking3)

df.post.quit.urge <- post.quit.urge %>% 
  select(these.cols.post.quit.urge) %>%
  rename(smoking.indicator = SmPostQU1, smoking.qty = Smoking2_PostQ_Urge, smoking.timing = Smoking3)

# Merge all datasets
df.post.quit.about.to.slip <- df.post.quit.about.to.slip[, order(colnames(df.post.quit.about.to.slip))]
df.post.quit.about.to.slip.part2 <- df.post.quit.about.to.slip.part2[, order(colnames(df.post.quit.about.to.slip.part2))]
df.post.quit.already.slipped <- df.post.quit.already.slipped[, order(colnames(df.post.quit.already.slipped))]
df.post.quit.random <- df.post.quit.random[, order(colnames(df.post.quit.random))]
df.post.quit.urge <- df.post.quit.urge[, order(colnames(df.post.quit.urge))]

df.all <- rbind(df.post.quit.about.to.slip,
                df.post.quit.about.to.slip.part2,
                df.post.quit.already.slipped,
                df.post.quit.random,
                df.post.quit.urge)

df.all <- df.all %>% arrange(id, assessment.unixts) %>% 
  mutate(assessment.type = as.character(assessment.type), 
         delivered.hrts = as.character(delivered.hrts),
         assessment.hrts = as.character(assessment.hrts)) %>%
  select(id = id,
         start.clock = start.clock,
         end.clock = end.clock,
         engaged.yes = engaged.yes,
         with.any.response = with.any.response,
         record.status = record.status,
         delivered.hrts = delivered.hrts,
         delivered.unixts = delivered.unixts,
         assessment.hrts = assessment.hrts,
         assessment.unixts = assessment.unixts,
         assessment.type = assessment.type,
         smoking.indicator = smoking.indicator,
         smoking.qty = smoking.qty,
         smoking.timing = smoking.timing) # Rearrange columns

# Create IDs for each person-EMA delivered
df.all <- df.all %>% arrange(id, delivered.unixts)
df.all[,"delivered.assessment.id"] <- CreateID(dat = df.all, 
                                               sequence.along = "delivered.unixts", 
                                               by.var = "id", 
                                               id.name = "delivered.assessment.id")

#------------------------------------------------------------------------------
# Exclude EMAs delivered but where there is no indication that participant 
# completed any question
#------------------------------------------------------------------------------

# Create data for analysis: Get subset of df.all corresponding to
# (1) EMAs with any recorded response
# (2) EMAs where participant completed at least one question but due to
# technical issues, none of their responses were recorded

df.analysis <- df.all %>% filter(engaged.yes == 1) 
df.analysis <- df.analysis %>% arrange(id, assessment.unixts)
df.analysis[,"assessment.id"] <- CreateID(dat = df.analysis, 
                                          sequence.along = "delivered.assessment.id", 
                                          by.var = "id", 
                                          id.name = "assessment.id")

#------------------------------------------------------------------------------
# Create end points of time intervals
#------------------------------------------------------------------------------

# Count unique participant ID's
ids.participants <- unique(df.analysis$id)
n.participants <- length(ids.participants)

# Create new columns by id
tmp.list <- list()
for(i in 1:n.participants){
  # Obtain subset of rows corresponding to participant i
  this.group <- which(df.analysis$id==ids.participants[i])
  df.this.group <- df.analysis[this.group,]
  df.this.group <- df.this.group %>% arrange(id, assessment.unixts)
  
  # Define new columns
  df.this.group <- GetPastRecords(df.this.group = df.this.group, cols.today = c("assessment.type", "assessment.unixts"), h = c(1,1))
  df.this.group[,"interval.duration.secs"] <- df.this.group[,"assessment.unixts"] - df.this.group[,"assessment.unixts_lag1"]
  df.this.group[,"interval.duration.secs"] <- if_else(df.this.group[,"assessment.id"]==1, 
                                                      df.this.group[,"assessment.unixts"] - df.this.group[,"start.clock"], 
                                                      df.this.group[,"interval.duration.secs"])
  df.this.group[,"interval.duration.hours"] <- df.this.group[,"interval.duration.secs"]/(60*60) 
  
  # Append new info to current info
  df.this.group <- list(df.this.group)
  tmp.list <- append(tmp.list, df.this.group)
}

# bind_rows is dplyr's efficient implementation of do.call(rbind, my_list)
df.analysis.tmp <- bind_rows(tmp.list) %>% arrange(id, assessment.unixts)

#------------------------------------------------------------------------------
# Individually inspect observations with interval.duration.hours=0 and
# resolve issues
#------------------------------------------------------------------------------

df.analysis.tmp <- df.analysis.tmp %>% 
  # Two consecutive Post-Quit About to Slip Part2 assessments delivered
  mutate(smoking.qty = replace(smoking.qty, row.names(.)==2303, df.analysis.tmp[2304,"smoking.qty"]),
         record.status = replace(record.status, row.names(.)==2303, df.analysis.tmp[2304,"record.status"])) %>%
  # Two consecutive Post-Quit About to Slip Part2 assessments delivered
  mutate(smoking.qty = replace(smoking.qty, row.names(.)==2938, df.analysis.tmp[2939,"smoking.qty"]),
         record.status = replace(record.status, row.names(.)==2938, df.analysis.tmp[2939,"record.status"])) %>%
  # Two consecutive Post-Quit About to Slip Part2 assessments delivered
  mutate(smoking.qty = replace(smoking.qty, row.names(.)==2961, df.analysis.tmp[2962,"smoking.qty"]),
         record.status = replace(record.status, row.names(.)==2961, df.analysis.tmp[2962,"record.status"])) %>%
  # Two consecutive Post-Quit About to Slip Part2 assessments delivered
  mutate(smoking.qty = replace(smoking.qty, row.names(.)==2974, df.analysis.tmp[2975,"smoking.qty"]),
         record.status = replace(record.status, row.names(.)==2974, df.analysis.tmp[2975,"record.status"])) %>%
  # Two consecutive Post-Quit About to Slip Part2 assessments delivered
  mutate(smoking.qty = replace(smoking.qty, row.names(.)==3999, df.analysis.tmp[4000,"smoking.qty"]),
         record.status = replace(record.status, row.names(.)==3999, df.analysis.tmp[4000,"record.status"])) %>%
  # Two consecutive Post-Quit Already Slipped assessments delivered
  mutate(smoking.qty = replace(smoking.qty, row.names(.)==4075, df.analysis.tmp[4076,"smoking.qty"]),
         smoking.timing = replace(smoking.timing, row.names(.)==4075, df.analysis.tmp[4076,"smoking.timing"]),
         record.status = replace(record.status, row.names(.)==4075, df.analysis.tmp[4076,"record.status"]))  %>%
  # Two consecutive Post-Quit About to Slip Part2 assessments delivered
  mutate(smoking.qty = replace(smoking.qty, row.names(.)==6526, df.analysis.tmp[6527,"smoking.qty"]),
         record.status = replace(record.status, row.names(.)==6526, df.analysis.tmp[6527,"record.status"])) %>%
  # Two consecutive Post-Quit About to Slip Part2 assessments delivered
  mutate(smoking.qty = replace(smoking.qty, row.names(.)==6583, df.analysis.tmp[6584,"smoking.qty"]),
         record.status = replace(record.status, row.names(.)==6583, df.analysis.tmp[6584,"record.status"])) %>%
  # Two consecutive Post-Quit Urge assessments delivered
  mutate(smoking.indicator = replace(smoking.indicator, row.names(.)==7422, df.analysis.tmp[7423,"smoking.indicator"]),
         smoking.qty = replace(smoking.qty, row.names(.)==7422, df.analysis.tmp[7423,"smoking.qty"]),
         smoking.timing = replace(smoking.timing, row.names(.)==7422, df.analysis.tmp[7423,"smoking.timing"]),
         record.status = replace(record.status, row.names(.)==7422, df.analysis.tmp[7423,"record.status"])) 
  
df.analysis <- df.analysis.tmp %>% filter(!(row.names(.)%in%c(2304,2939,2962,2975,4000,4076,6527,6584,7423)))

# Finally, of the remaining observations, create IDs for each 
# person-EMA delivered AND with an indication of any response
df.analysis[,"assessment.id"] <- CreateID(dat = df.analysis, 
                                          sequence.along = "delivered.assessment.id", 
                                          by.var = "id", 
                                          id.name = "assessment.id")

#------------------------------------------------------------------------------
# Provide smoking labels to time intervals according to Method 01
# Begin data manipulation with df.analysis
#------------------------------------------------------------------------------

# Save to csv file
# UB.seconds and LB.seconds are in seconds since start.clock
df.analysis.01 <- df.analysis %>% SmokingLabelPNS01(df = .) %>%
  mutate(UB.seconds = assessment.unixts - start.clock) %>%
  mutate(LB.seconds = assessment.unixts_lag1 - start.clock) %>%
  mutate(LB.seconds = if_else(assessment.id==1, 0, LB.seconds)) %>%
  select(id, interval.id=assessment.id, 
         start.clock, end.clock,
         #assessment.type, interval.duration.hours, record.status, 
         LB.seconds, UB.seconds, smoking.label, num.cigs.smoked)

df.analysis.01 <- CreateLastTimeInterval(df.analysis.01)

write.csv(df.analysis.01, file.path(path.output_data,"PNS/df.analysis.01.csv"), row.names = FALSE)

#------------------------------------------------------------------------------
# Provide smoking labels to time intervals according to Method 02a
# Begin data manipulation with df.analysis
#------------------------------------------------------------------------------

df.analysis.02a <- df.analysis %>% SmokingLabelPNS01(df = .) %>% SmokingTimingPNS(df = .)
# test-file-01.R uses out_smoking_timing.csv to perform a sanity check
write.csv(df.analysis.02a, file.path(path.output_data, "PNS/out_smoking_timing.csv"), row.names=FALSE)

# Create data for analysis by participant id
tmp.list <- list()
for(i in 1:n.participants){
  # Obtain subset of rows corresponding to participant i
  this.group <- which(df.analysis.02a$id==ids.participants[i])
  df.this.group <- df.analysis.02a[this.group,]
  
  # Create data for analysis for a given participant id
  newdf.this.group <- SmokingRelabel(df = df.this.group, FUN_RESOLVE_CONFLICT = NULL)
  
  # Append new info to current info
  newdf.this.group <- list(newdf.this.group)
  tmp.list <- append(tmp.list, newdf.this.group)
}

# bind_rows is dplyr's efficient implementation of do.call(rbind, my_list)
df.analysis.02a <- bind_rows(tmp.list) %>% arrange(id, interval.id)

# Save to csv file
# UB.seconds and LB.seconds are in seconds since start.clock
df.analysis.02a <- df.analysis.02a %>% 
  mutate(UB.seconds = UB - start.clock) %>%
  mutate(LB.seconds = LB - start.clock) %>%
  mutate(LB.seconds = if_else(interval.id==1, 0, LB.seconds)) %>%
  select(id, interval.id, 
         start.clock, end.clock,
         LB.seconds, UB.seconds, smoking.label, num.cigs.smoked)

df.analysis.02a <- CreateLastTimeInterval(df.analysis.02a)

write.csv(df.analysis.02a, file.path(path.output_data,"PNS/df.analysis.02a.csv"), row.names = FALSE)

#------------------------------------------------------------------------------
# Provide smoking labels to time intervals according to Method 02b
# Begin data manipulation with df.analysis
#------------------------------------------------------------------------------

df.analysis.02b <- df.analysis %>% SmokingLabelPNS01(df = .) %>% SmokingTimingPNS(df = .)
# test-file-01.R uses out_smoking_timing.csv to perform a sanity check
write.csv(df.analysis.02b, file.path(path.output_data, "PNS/out_smoking_timing.csv"), row.names=FALSE)

# Create data for analysis by participant id
tmp.list <- list()
for(i in 1:n.participants){
  # Obtain subset of rows corresponding to participant i
  this.group <- which(df.analysis.02b$id==ids.participants[i])
  df.this.group <- df.analysis.02b[this.group,]
  
  # Create data for analysis for a given participant id
  newdf.this.group <- SmokingRelabel(df = df.this.group, FUN_RESOLVE_CONFLICT = ResolveConflict)
  
  # Append new info to current info
  newdf.this.group <- list(newdf.this.group)
  tmp.list <- append(tmp.list, newdf.this.group)
}

# bind_rows is dplyr's efficient implementation of do.call(rbind, my_list)
df.analysis.02b <- bind_rows(tmp.list) %>% arrange(id, interval.id)

# Save to csv file
# UB.seconds and LB.seconds are in seconds since start.clock
df.analysis.02b <- df.analysis.02b %>% 
  mutate(UB.seconds = UB - start.clock) %>%
  mutate(LB.seconds = LB - start.clock) %>%
  mutate(LB.seconds = if_else(interval.id==1, 0, LB.seconds)) %>%
  select(id, interval.id, 
         start.clock, end.clock,
         LB.seconds, UB.seconds, smoking.label, num.cigs.smoked)

df.analysis.02b <- CreateLastTimeInterval(df.analysis.02b)

write.csv(df.analysis.02b, file.path(path.output_data,"PNS/df.analysis.02b.csv"), row.names = FALSE)

#------------------------------------------------------------------------------
# Add control covariates to each dataset for analysis
#------------------------------------------------------------------------------

source(file.path(path.code, "construct-variables-intervals.R"))

write.csv(df.analysis.01, file.path(path.output_data,"PNS/df.analysis.01.csv"), row.names = FALSE)
write.csv(df.analysis.02a, file.path(path.output_data,"PNS/df.analysis.02a.csv"), row.names = FALSE)
write.csv(df.analysis.02b, file.path(path.output_data,"PNS/df.analysis.02b.csv"), row.names = FALSE)
