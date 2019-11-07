# About this script: In this script, we perform a series of data curation tasks 
# specific to data from the Break Free study

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------

library(assertthat)
library(dplyr)
path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
source(file.path(path.code,"breakfree-run-curation/breakfree-setup.R"))

#------------------------------------------------------------------------------
# Specify columns to use in creating data for analyses
#------------------------------------------------------------------------------

these.cols <- c("id", "delivered.assessment.id", "record.status",
                "start.clock", "end.clock",
                # Assessment type (eg. random EMA, smoking EMA, stress EMA)
                "assessment.type", 
                # Timestamp: when EMA was delivered
                "delivered.unixts",
                # Timestamp: when individual began completing EMA
                "assessment.unixts",
                # Indicator of whether there is a record of participant completing at least one question
                "with.any.response",
                # Smoking related questions and their timestamps
                "Smoked.Cig.","Time.39",
                "Howmany.Cigs.", "Time.40",
                "How.long.ago.", "Time.41",
                "Recent.cig.how.long.ago.","Time.42",
                "How.long.ago.smoke.first.cig","Time.43")

df.all <- subset(breakfree.ema, select = these.cols)
write.csv(df.all, file.path(path.output_data,"BreakFree/df.all.csv"), row.names = FALSE)

#------------------------------------------------------------------------------
# Only retain rows with a record of at least one question completed
#------------------------------------------------------------------------------

df.analysis <- df.all %>% filter(with.any.response == 1) %>% arrange(id, assessment.unixts)
df.analysis[,"assessment.id"] <- CreateID(dat = df.analysis, sequence.along = "assessment.unixts", by.var = "id", id.name = "assessment.id")
tmp.list <- colnames(df.analysis)
df.analysis <- df.analysis %>% select(tmp.list[1:2],tmp.list[length(tmp.list)],tmp.list[3:length(tmp.list)-1])
remove(tmp.list)

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
df.analysis <- bind_rows(tmp.list) %>% arrange(id, assessment.unixts)

#------------------------------------------------------------------------------
# Provide smoking labels to time intervals according to Method 01
# Begin data manipulation with df.analysis
#------------------------------------------------------------------------------

# Save to csv file
# UB.seconds and LB.seconds are in seconds since start.clock
df.analysis.01 <- df.analysis %>% SmokingLabelBreakFree01(df = .) %>%
  mutate(UB.seconds = assessment.unixts - start.clock) %>%
  mutate(LB.seconds = assessment.unixts_lag1 - start.clock) %>%
  mutate(LB.seconds = if_else(assessment.id==1, 0, LB.seconds)) %>%
  select(id, interval.id=assessment.id, 
         start.clock, end.clock,
         #assessment.type, interval.duration.hours, record.status, 
         LB.seconds, UB.seconds, smoking.label, num.cigs.smoked)

df.analysis.01 <- CreateLastTimeInterval(df.analysis.01)

write.csv(df.analysis.01, file.path(path.output_data,"BreakFree/df.analysis.01.csv"), row.names = FALSE)

#------------------------------------------------------------------------------
# Provide smoking labels to time intervals according to Method 02a
# Begin data manipulation with df.analysis
#------------------------------------------------------------------------------

df.analysis.02a <- df.analysis %>% SmokingLabelBreakFree01(df = .) %>% SmokingTimingBreakFree(df = .)
# For sanity check
write.csv(df.analysis.02a, file.path(path.output_data, "BreakFree/out_smoking_timing.csv"), row.names = FALSE)

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

write.csv(df.analysis.02a, file.path(path.output_data,"BreakFree/df.analysis.02a.csv"), row.names = FALSE)

#------------------------------------------------------------------------------
# Provide smoking labels to time intervals according to Method 02b
# Begin data manipulation with df.analysis
#------------------------------------------------------------------------------

df.analysis.02b <- df.analysis %>% SmokingLabelBreakFree01(df = .) %>% SmokingTimingBreakFree(df = .)
# For sanity check
write.csv(df.analysis.02b, file.path(path.output_data, "BreakFree/out_smoking_timing.csv"), row.names = FALSE)

# Create data for analysis by participant id
tmp.list <- list()
for(i in 1:n.participants){
  # Obtain subset of rows corresponding to participant i
  this.group <- which(df.analysis.02b$id==ids.participants[i])
  df.this.group <- df.analysis.02b[this.group,]
  
  # Create data for analysis for a given participant id
  newdf.this.group <- SmokingRelabel(df = df.this.group, FUN_RESOLVE_CONFLICT =  ResolveConflict)
  
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

write.csv(df.analysis.02b, file.path(path.output_data,"BreakFree/df.analysis.02b.csv"), row.names = FALSE)

#------------------------------------------------------------------------------
# Provide smoking labels to time intervals according to Method 03, 04a, 04b
# Begin data manipulation with df.analysis.01, df.analysis.02a, df.analysis.02b
#------------------------------------------------------------------------------

df.analysis.03 <- IntegratePuffmarkerEpisodes(df.ema = df.analysis.01, df.puffmarker = breakfree.puffmarker.episodes)
write.csv(df.analysis.03, file.path(path.output_data,"BreakFree/df.analysis.03.csv"), row.names = FALSE)

df.analysis.04a <- IntegratePuffmarkerEpisodes(df.ema = df.analysis.02a, df.puffmarker = breakfree.puffmarker.episodes)
write.csv(df.analysis.04a, file.path(path.output_data,"BreakFree/df.analysis.04a.csv"), row.names = FALSE)

df.analysis.04b <- IntegratePuffmarkerEpisodes(df.ema = df.analysis.02b, df.puffmarker = breakfree.puffmarker.episodes)
write.csv(df.analysis.04b, file.path(path.output_data,"BreakFree/df.analysis.04b.csv"), row.names = FALSE)


