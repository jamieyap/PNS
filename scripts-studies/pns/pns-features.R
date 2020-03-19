#------------------------------------------------------------------------------
# Create new features
#------------------------------------------------------------------------------
# Which affect variable?
all.use.vars <- c("Affect1","Affect2","Affect3","Affect4","Affect5",
                  "Affect6","Affect7","Affect8","Affect9","Affect10")
# What time frame to consider?
# all.use.H = 48: past 2 days until present timestamp
# all.use.H = 24*22: since quit day until present timestamp (since 
#                   entire post quit period in the study is 21 days)
all.use.H <- c(48, 24*22) 

# More data processing steps...
df.post.quit.random[,"ones"] <- 1  # Create a new column with ones

for(i in 1:length(all.use.H)){
  # Loop over time frames
  use.H <- all.use.H[i]
  
  for(j in 1:length(all.use.vars)){
    # Loop over EMA items
    use.this.var <- all.use.vars[j]
    
    # Code below could optionally be placed inside a loop
    # for other possible values of use.this.var and use.H
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    # =============================================================================
    # Mean of variables among prompts delivered within a given time frame
    # =============================================================================
    tmp.var.name <- paste(use.this.var,"_mean.since.","past",use.H, "hours", sep="")
    
    df.post.quit.random[,tmp.var.name] <- df.post.quit.random %>% 
      group_by(id) %>%
      do(MeanWithin(df.this.group = ., 
                    ones.col = "ones", 
                    current.ts = "time.unixts.scaled", 
                    H=use.H,
                    this.var=use.this.var)
      ) %>%
      as.data.frame(.) %>%
      select("V1")
    
    # =============================================================================
    # Variance of variables among prompts delivered within a given time frame
    # =============================================================================
    tmp.var.name <- paste(use.this.var,"_var.since.","past",use.H, "hours", sep="")
    
    df.post.quit.random[,tmp.var.name] <- df.post.quit.random %>% 
      group_by(id) %>%
      do(VarianceWithin(df.this.group = ., 
                        ones.col = "ones", 
                        current.ts = "time.unixts.scaled", 
                        H=use.H,
                        this.var=use.this.var)
      ) %>%
      as.data.frame(.) %>%
      select("V1")
    
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  } # End looping over variables
} # End looping over time frames

