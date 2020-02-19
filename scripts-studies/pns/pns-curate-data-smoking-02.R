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
source(file.path(path.code,"pns-run-curation/pns-get-items.R"))

#------------------------------------------------------------------------------
# Read in smoking datasets
#------------------------------------------------------------------------------

df.analysis.01 <- read.csv(file.path(path.output_data,"PNS/df.analysis.01.csv"))
df.analysis.02a <- read.csv(file.path(path.output_data,"PNS/df.analysis.02a.csv"))
df.analysis.02b <- read.csv(file.path(path.output_data,"PNS/df.analysis.02b.csv"))

list.df.analysis <- list(df.analysis.01=df.analysis.01,
                         df.analysis.02a=df.analysis.02a,
                         df.analysis.02b=df.analysis.02b)

#------------------------------------------------------------------------------
# Merge smoking datasets with responses from random EMAs
#------------------------------------------------------------------------------

for(k in 1:length(list.df.analysis)){
  
  smoking.df.name <- names(list.df.analysis)[k]
  smoking.df <- list.df.analysis[[k]]
  ids <- unique(smoking.df$id)
  list.smoking.df.with.vars <- list()
  
  for(i in 1:length(ids)){
    this.participant.smoking.df <- smoking.df %>% filter(id==ids[i])
    this.participant.variables.df <- variables.df %>% filter(id==ids[i])
    tot.col <- ncol(this.participant.variables.df)
    
    past.colnames.this.participant.variables.df <- c(colnames(this.participant.variables.df)[1],
                                                     paste("past",
                                                           colnames(this.participant.variables.df)[2:tot.col],
                                                           sep="."))
    future.colnames.this.participant.variables.df <- c(colnames(this.participant.variables.df)[1],
                                                       paste("future",
                                                             colnames(this.participant.variables.df)[2:tot.col],
                                                             sep="."))
    
    past.this.participant.variables.df <- this.participant.variables.df
    future.this.participant.variables.df <- this.participant.variables.df
    colnames(past.this.participant.variables.df) <- past.colnames.this.participant.variables.df
    colnames(future.this.participant.variables.df) <- future.colnames.this.participant.variables.df
    remove(this.participant.variables.df)
    
    for(j in 1:nrow(this.participant.smoking.df)){
      this.row.begin.time <- as.numeric(this.participant.smoking.df[j,"LB.seconds"])
      this.row.end.time <- as.numeric(this.participant.smoking.df[j,"UB.seconds"])
      
      check.past <- which(this.row.begin.time >= past.this.participant.variables.df$past.time.unixts.scaled)
      check.future <- which(this.row.end.time <= future.this.participant.variables.df$future.time.unixts.scaled)
      
      if(length(check.past)>0){
        idx.past <- max(check.past)
        dat.past <- past.this.participant.variables.df[idx.past,!(colnames(past.this.participant.variables.df)%in%c("id"))]
      }else{
        dat.past <- rep(NA, length(colnames(past.this.participant.variables.df)))
        dat.past <- as.data.frame(t(dat.past))
        colnames(dat.past) <- colnames(past.this.participant.variables.df)
        dat.past <- dat.past[, !(colnames(dat.past) %in% c("id"))]
      }
      
      if(length(check.future)>0){
        idx.future <- min(check.future)
        dat.future <- future.this.participant.variables.df[idx.future,!(colnames(future.this.participant.variables.df)%in%c("id"))]
      }else{
        dat.future <- rep(NA, length(colnames(future.this.participant.variables.df)))
        dat.future <- as.data.frame(t(dat.future))
        colnames(dat.future) <- colnames(future.this.participant.variables.df)
        dat.future <- dat.future[, !(colnames(dat.future) %in% c("id"))]
      }
      
      tmpdf <- cbind(this.participant.smoking.df[j,], dat.past, dat.future)
      tmpdf <- as.data.frame(tmpdf)
      list.smoking.df.with.vars <- append(list.smoking.df.with.vars, list(tmpdf))
    }
  }
  
  smoking.df.with.vars <- do.call(rbind, list.smoking.df.with.vars)
  
  write.csv(smoking.df.with.vars, 
            file.path(path.output_data, paste("PNS/",smoking.df.name,".with.vars.csv", sep="")), row.names=FALSE)
  
}


