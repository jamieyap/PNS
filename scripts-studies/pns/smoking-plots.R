if(min(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE) >= -7){
  plot(x=-100, main=paste("Participant# ",use.id-3000, sep=""),
       xlim = c(-7, 3+max(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE)),
       ylim = c(-3, max(df.this.participant$smoking.qty, na.rm=TRUE)),
       xlab = "#Days before (negative) or after (positive) first Post-Quit EMA (zero)",
       ylab = "#Cigarettes",
       xaxt="n",
       yaxt="n")
  
  axis(1, at = seq(-7, ceiling(max(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE))))
  axis(2, at = seq(0, max(df.this.participant$smoking.qty, na.rm=TRUE)))
}else{
  plot(x=-100, main=paste("Participant# ",use.id-3000, sep=""),
       xlim = c(min(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE), 3+max(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE)),
       ylim = c(-3, max(df.this.participant$smoking.qty, na.rm=TRUE)),
       xlab = "#Days before (negative) or after (positive) first Post-Quit EMA (zero)",
       ylab = "#Cigarettes",
       xaxt="n",
       yaxt="n")
  
  axis(1, at = seq(floor(min(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE)), ceiling(max(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE))))
  axis(2, at = seq(0, max(df.this.participant$smoking.qty, na.rm=TRUE)))
}

subset.df.this.participant <- df.this.participant %>% filter(!is.na(smoking.qty))
if(nrow(subset.df.this.participant)>0){
  for(i in 1:nrow(subset.df.this.participant)){
    segments(x0 = subset.df.this.participant[i,"diffdays.postquit.earliest"], y0 = 0,
             x1 = subset.df.this.participant[i,"diffdays.postquit.earliest"], y1 = subset.df.this.participant[i, "smoking.qty"])
    
    if(subset.df.this.participant[i, "smoking.qty"]==0){
      points(subset.df.this.participant[i,"diffdays.postquit.earliest"], -0.15, col="cornflowerblue", pch=19)
      points(subset.df.this.participant[i,"diffdays.postquit.earliest"], -0.15, col="cadetblue3")
    }else{
      points(subset.df.this.participant[i,"diffdays.postquit.earliest"], -0.15, col="coral1", pch=19)
      points(subset.df.this.participant[i,"diffdays.postquit.earliest"], -0.15, col="brown3")
    }
  }
}

subset.df.this.participant <- df.this.participant %>% filter(is.na(smoking.qty))
if(nrow(subset.df.this.participant)>0){
  for(i in 1:nrow(subset.df.this.participant)){
    if(subset.df.this.participant[i, "current.assessment.type"]=="Pre-Quit Random"){
      next
    }else if(subset.df.this.participant[i, "current.assessment.type"]=="Post-Quit Random"){
      next
    }else{
      points(subset.df.this.participant[i,"diffdays.postquit.earliest"], -0.30, col="darkgoldenrod1", pch=19)
      points(subset.df.this.participant[i,"diffdays.postquit.earliest"], -0.30, col="darkgoldenrod4")
    }
  }
}


EMA_QDay <- use.dates[use.dates$id==use.id,"diffdays.EMA_Qday"]
EMA_QDay <- round(EMA_QDay, digits=1)
abline(v = EMA_QDay, col="blue", lty=2)

quitday <- use.dates[use.dates$id==use.id,"diffdays.quitday"]
quitday <- round(quitday, digits=1)
abline(v = quitday, col="blue", lty=2)

if(!is.na(EMA_QDay)){
  if(EMA_QDay>1+max(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE)){
    text(floor(max(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE))-1,-0.7, paste("EMA_QDay=",EMA_QDay,sep=""), col="blue", cex=0.70, pos=1)
  }else if(EMA_QDay < min(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE)){
    text(ceiling(min(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE))-1,-0.7, paste("EMA_QDay=",EMA_QDay,sep=""), col="blue", cex=0.70, pos=1)
  }else{
    text(EMA_QDay-1,-0.7, paste("EMA_QDay=",EMA_QDay,sep=""), col="blue", cex=0.70, pos=1)
  }
}else{
  text(0,-0.7, paste("EMA_QDay is missing"), col="blue", cex=0.70, pos=3)
}


if(!is.na(quitday)){
  if(quitday>1+max(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE)){
    text(floor(max(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE))+1,-1, paste("quitday=",quitday,sep=""), col="blue", cex=0.70, pos=3)
  }else if(quitday< min(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE)){
    text(ceiling(min(df.this.participant$diffdays.postquit.earliest, na.rm=TRUE))+1,-1, paste("quitday=",quitday,sep=""), col="blue", cex=0.70, pos=3)
  }else{
    text(quitday+1,-1, paste("quitday=",quitday,sep=""), col="blue", cex=0.70, pos=3)
  }
}else{
  text(0,-1, paste("quitday is missing"), col="blue", cex=0.70, pos=3)
}

legend("bottomleft", 
       c("with reported #cigarettes smoked (=0)",
         "with reported #cigarettes smoked (>0)",
         "missing #cigarettes smoked (self-initiated EMA)"),
       cex=1,
       pch = c(19, 19, 19),
       col =c("cornflowerblue","coral1","darkgoldenrod1"))

