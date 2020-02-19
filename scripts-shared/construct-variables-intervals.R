df.all <- df.all %>% mutate(delivered.time.since.start.clock = delivered.unixts - start.clock)

# Create data for analysis by participant id
tmp.01.list <- list()
tmp.02a.list <- list()
tmp.02b.list <- list()

for(i in 1:n.participants){
  # Obtain subset of rows corresponding to participant i
  this.group <- ids.participants[i]
  
  df.this.group <- df.all %>% filter(id==this.group)
  
  # Data frame: df.analysis.01 ------------------------------------------------
  df.analysis.01.this.group <- df.analysis.01 %>% filter(id==this.group)
  
  # Create data for analysis for a given participant id
  newdf.01.this.group <- CheckWithin(ref.df = df.this.group, 
                                     ref.this.var = "delivered.time.since.start.clock",
                                     df = df.analysis.01.this.group,
                                     study = use.study)
  
  # Append new info to current info
  newdf.01.this.group <- list(newdf.01.this.group)
  tmp.01.list <- append(tmp.01.list, newdf.01.this.group)
  
  # Data frame: df.analysis.02a -----------------------------------------------
  df.analysis.02a.this.group <- df.analysis.02a  %>% filter(id==this.group)
  
  # Create data for analysis for a given participant id
  newdf.02a.this.group <- CheckWithin(ref.df = df.this.group, 
                                      ref.this.var = "delivered.time.since.start.clock",
                                      df = df.analysis.02a.this.group,
                                      study = use.study)
  
  # Append new info to current info
  newdf.02a.this.group <- list(newdf.02a.this.group)
  tmp.02a.list <- append(tmp.02a.list, newdf.02a.this.group)
  
  # Data frame: df.analysis.02b -----------------------------------------------
  df.analysis.02b.this.group <- df.analysis.02b  %>% filter(id==this.group)
  
  # Create data for analysis for a given participant id
  newdf.02b.this.group <- CheckWithin(ref.df = df.this.group, 
                                      ref.this.var = "delivered.time.since.start.clock",
                                      df = df.analysis.02b.this.group,
                                      study = use.study)
  
  # Append new info to current info
  newdf.02b.this.group <- list(newdf.02b.this.group)
  tmp.02b.list <- append(tmp.02b.list, newdf.02b.this.group)
  
}

df.analysis.01 <- bind_rows(tmp.01.list) %>% arrange(id, interval.id)
df.analysis.02a <- bind_rows(tmp.02a.list) %>% arrange(id, interval.id)
df.analysis.02b <- bind_rows(tmp.02b.list) %>% arrange(id, interval.id) 
