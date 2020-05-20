#------------------------------------------------------------------------------
# Tasks common to all analyses using PNS study data
#------------------------------------------------------------------------------
# Implement decision rules to include only "valid EMAs"
df.processed <- SetUpPostQuit(df.raw = df.raw)

#------------------------------------------------------------------------------
# Tasks specific to this dataset for analysis
#------------------------------------------------------------------------------
# Create a new variable so that for each row in the raw data is checked: the 
# new variable with.any.response equals 1 if response to any question was 
# recorded and equals 0 if no response to any question was recorded
df.processed <- CheckAnyResponse(df = df.processed, keep.cols = ema.item.names[["name.codebook"]])

#------------------------------------------------------------------------------
# Format date and time variables
#------------------------------------------------------------------------------
df.processed[["delivered.hrts"]] <- strftime(df.processed[["delivered.hrts"]], format = "%Y-%m-%d %H:%M:%S")
df.processed[["begin.hrts"]] <- strftime(df.processed[["begin.hrts"]], format = "%Y-%m-%d %H:%M:%S")
df.processed[["end.hrts"]] <- strftime(df.processed[["end.hrts"]], format = "%Y-%m-%d %H:%M:%S")

#------------------------------------------------------------------------------
# Check whether there are any missing begin.unixts/begin.hrts timestamps
#------------------------------------------------------------------------------
df.processed[["begin.hrts"]] <- if_else(df.processed[["with.any.response"]]==1 & is.na(df.processed[["begin.hrts"]]),
                                        df.processed[["delivered.hrts"]], 
                                        df.processed[["begin.hrts"]])

#------------------------------------------------------------------------------
# Exclude rows whose timestamps occur before the beginning of the study or 
# after the end of study
#------------------------------------------------------------------------------
df.usedates <- df.dates %>% 
  select(id, callnumr, 
         use.begin.date.value, use.end.date.value,
         use.begin.time.value, use.end.time.value) %>%
  mutate(begin.date.hrts = paste(use.begin.date.value, use.begin.time.value, sep=" "),
         end.date.hrts = paste(use.end.date.value, use.end.time.value, sep=" ")) %>%
  mutate(begin.date.hrts = if_else(begin.date.hrts=="", NA_character_, begin.date.hrts),
         end.date.hrts = if_else(end.date.hrts=="", NA_character_, end.date.hrts)) %>%
  mutate(begin.date.hrts = as.POSIXct(strptime(begin.date.hrts, format = "%Y-%m-%d %H:%M:%S")),
         end.date.hrts = as.POSIXct(strptime(end.date.hrts, format = "%Y-%m-%d %H:%M:%S"))) %>%
  mutate(begin.date.unixts = as.numeric(begin.date.hrts),
         end.date.unixts = as.numeric(end.date.hrts)) %>%
  filter(!is.na(begin.date.unixts)) %>%
  select(-use.begin.date.value, -use.end.date.value,
         -use.begin.time.value, -use.end.time.value)

df.processed <- left_join(x = df.usedates, y = df.processed, by="id")
df.processed <- df.processed %>% filter(delivered.unixts>=begin.date.unixts & delivered.unixts<=end.date.unixts)

#------------------------------------------------------------------------------
# Clean up data frame and write out processed data
#------------------------------------------------------------------------------
df.processed <- df.processed %>%
  select(id, callnumr,
         begin.date.hrts, begin.date.unixts,
         end.date.hrts, end.date.unixts,
         record.id, assessment.type,
         delivered.hrts, begin.hrts, end.hrts,
         delivered.unixts, begin.unixts, end.unixts,
         with.any.response,
         ema.item.names$name.codebook)

