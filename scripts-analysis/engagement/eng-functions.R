library(dplyr)
library(assertthat)

CountWithin <- function(df.this.group, check.ts = "time_unixts", current.ts =  "time_unixts", H, newvar.name){
  # Args: 
  #   df.this.group: a data frame corresponding groups in the
  #   data for which this counting operation is to be performed
  #   (e.g. each group may be 1 participant)
  #   current.ts: seconds since Jan 1, 1970 in UNIX time format
  #   H: number of hours prior to current.ts at which counting
  #   should begin
  #   check.ts: seconds since Jan 1, 1970 in UNIX time format
  # Output:
  #   An array of means with number of rows equal to 
  #   number of check.ts entries in df.this.group
  
  df.this.group[,"pastH.ts"] <- df.this.group[,current.ts] - H*60*60
  list.quantity <- list()
  
  # For each row of df.this.group
  for(i in 1:nrow(df.this.group)){
    LB <- as.numeric(df.this.group[i,"pastH.ts"])
    UB <- as.numeric(df.this.group[i,current.ts])
    
    df.subset <- df.this.group %>% 
      filter((.data[[check.ts]] >= LB) & (.data[[check.ts]] <= UB))
    
    quantity <- nrow(df.subset)
    list.quantity <- append(list.quantity, quantity)
  }
  vec.quantity <- do.call(rbind, list.quantity)
  vec.quantity <- as.data.frame(vec.quantity)
  
  df.this.group[,newvar.name] <- vec.quantity
  df.this.group <- df.this.group %>% select(-pastH.ts)
  
  return(df.this.group)
}

MeanWithin <- function(df.this.group, check.ts = "time_unixts", current.ts = "time_unixts", H, this.var, newvar.name){
  # Args: 
  #   df.this.group: a data frame corresponding groups in the
  #   data for which this counting operation is to be performed
  #   (e.g. each group may be 1 participant)
  #   current.ts: seconds since Jan 1, 1970 in UNIX time format
  #   H: number of hours prior to current.ts at which counting
  #   should begin
  #   check.ts: seconds since Jan 1, 1970 in UNIX time format
  # Output:
  #   An array of means with number of rows equal to 
  #   number of check.ts entries in df.this.group
  
  df.this.group[,"pastH.ts"] <- df.this.group[,current.ts] - H*60*60
  list.quantity <- list()
  
  # For each row of df.this.group
  for(i in 1:nrow(df.this.group)){
    LB <- as.numeric(df.this.group[i,"pastH.ts"])
    UB <- as.numeric(df.this.group[i,current.ts])
    
    df.subset <- df.this.group %>% 
      filter((.data[[check.ts]] >= LB) & (.data[[check.ts]] <= UB))
    
    check.missing <- sum(is.na(df.subset[,this.var]))
    if(check.missing == nrow(df.subset)){
      quantity <- NA_real_
    }else{
      these.vals <- df.subset[,this.var]
      these.vals <- as.matrix(these.vals)
      quantity <- mean(these.vals, na.rm=TRUE)
    }
    
    list.quantity <- append(list.quantity, quantity)
  }
  vec.quantity <- do.call(rbind, list.quantity)
  vec.quantity <- as.numeric(vec.quantity)
  
  df.this.group[,newvar.name] <- vec.quantity
  df.this.group <- df.this.group %>% select(-pastH.ts)
  
  return(df.this.group)
}

VarianceWithin <- function(df.this.group, check.ts = "time_unixts", current.ts = "time_unixts", H, this.var, newvar.name){
  # Args: 
  #   df.this.group: a data frame corresponding groups in the
  #   data for which this counting operation is to be performed
  #   (e.g. each group may be 1 participant)
  #   current.ts: seconds since Jan 1, 1970 in UNIX time format
  #   H: number of hours prior to current.ts at which counting
  #   should begin
  #   check.ts: seconds since Jan 1, 1970 in UNIX time format
  # Output:
  #   An array of variances with number of rows equal to 
  #   number of check.ts entries in df.this.group
  
  df.this.group[,"pastH.ts"] <- df.this.group[,current.ts] - H*60*60
  list.quantity <- list()
  
  # For each row of df.this.group
  for(i in 1:nrow(df.this.group)){
    LB <- as.numeric(df.this.group[i,"pastH.ts"])
    UB <- as.numeric(df.this.group[i,current.ts])
    
    df.subset <- df.this.group %>% 
      filter((.data[[check.ts]] >= LB) & (.data[[check.ts]] <= UB))
    
    check.missing <- sum(is.na(df.subset[,this.var]))
    if(check.missing == nrow(df.subset)){
      quantity <- NA_real_
    }else{
      these.vals <- df.subset[,this.var]
      these.vals <- as.matrix(these.vals)
      quantity <- var(these.vals, na.rm=TRUE)
    }
    
    list.quantity <- append(list.quantity, quantity)
  }
  vec.quantity <- do.call(rbind, list.quantity)
  vec.quantity <- as.numeric(vec.quantity)
  
  df.this.group[,newvar.name] <- vec.quantity
  df.this.group <- df.this.group %>% select(-pastH.ts)
  
  return(df.this.group)
}


