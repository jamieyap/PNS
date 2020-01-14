
#------------------------------------------------------------------------------
# Clean up multiple choice items
#------------------------------------------------------------------------------
df$Location <- apply(df, 1, function(this.row){
  Location_1 <- as.numeric(this.row["Location_1"])
  Location_2 <- as.numeric(this.row["Location_2"])
  Location_3 <- as.numeric(this.row["Location_3"])
  Location_4 <- as.numeric(this.row["Location_4"])
  Location_5 <- as.numeric(this.row["Location_5"])
  Location_6 <- as.numeric(this.row["Location_6"])
  Location_7 <- as.numeric(this.row["Location_7"])
  
  vals <- unique(c(Location_1, Location_2,
                   Location_3, Location_4,
                   Location_5, Location_6,
                   Location_7))
  vals <- vals[!is.na(vals)]
  
  if(length(vals)==0){
    out <- NA_character_
  }else{
    out <- list(vals)
    out <- as.character(out)
  }
  
  return(out)
})

df$Consume3 <- apply(df, 1, function(this.row){
  Consume3_1 <- as.numeric(this.row["Consume3_1"])
  Consume3_2 <- as.numeric(this.row["Consume3_2"])
  Consume3_3 <- as.numeric(this.row["Consume3_3"])
  Consume3_4 <- as.numeric(this.row["Consume3_4"])
  Consume3_5 <- as.numeric(this.row["Consume3_5"])
  
  vals <- unique(c(Consume3_1, Consume3_2,
                   Consume3_3, Consume3_4,
                   Consume3_5))
  vals <- vals[!is.na(vals)]
  
  if(length(vals)==0){
    out <- NA_character_
  }else{
    out <- list(vals)
    out <- as.character(out)
  }
  
  return(out)
})

df <- df %>% 
  select(-Location_1, -Location_2,
         -Location_3, -Location_4,
         -Location_5, -Location_6,
         -Location_7) %>%
  select(-Consume3_1, -Consume3_2,
         -Consume3_3, -Consume3_4,
         -Consume3_5)