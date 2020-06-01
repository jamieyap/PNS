#------------------------------------------------------------------------------
# Tasks common to all analyses using PNS study data
#------------------------------------------------------------------------------
# Implement decision rules to include only "valid EMAs"
df.processed <- SetUpPreQuit(df.raw = df.raw)

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

df.processed <- df.processed %>% 
  select(id, 
         record.id, assessment.type,
         delivered.hrts, begin.hrts, end.hrts,
         delivered.unixts, begin.unixts, end.unixts,
         with.any.response,
         ema.item.names$name.codebook)
