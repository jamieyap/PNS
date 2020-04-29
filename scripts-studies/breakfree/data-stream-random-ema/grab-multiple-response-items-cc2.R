# List to collect item responses
list.resp.cc2 <- list()
N <- length(list.df.raw.random.DATA.cc2)

for(i in 1:N){
  df.raw <- list.df.raw.random.DATA.cc2[[i]]
  
  all.col.names <- colnames(df.raw)
  check <- (("questions_0_response_0") %in% all.col.names)
  
  if(isTRUE(check)){
    list.items <- list()
    # There are 64 items in a Random EMA for CC2 participants
    # The last item is a thank you message
    # Thus, idx ranges from 1 to 63
    for(idx in collect.idx){  
      these.col.names <- grepl(paste("_",idx,"_",sep=""), all.col.names)
      these.col.names <- all.col.names[these.col.names]
      df.item <- df.raw %>% select(these.col.names) 
      
      # This step allows us to determine the total number of options
      # available to the participant to select
      idx.options <- grep(pattern = paste("questions_",idx,"_response_option_", sep=""), x = these.col.names)
      # number.options is a count of the total number of options available to the participant to select
      number.options <- length(these.col.names[idx.options])
      all.possible.columns <- paste("questions_",idx,"_response_",0:(number.options-1), sep="")
      in.data <- all.possible.columns[all.possible.columns %in% these.col.names]
      
      if(length(in.data)>0){
        list.collect.chosen.options <- list()
        
        for(j in 1:length(in.data)){
          idx.resp <- which(these.col.names==in.data[j])
          question.resp <- df.item[,idx.resp]
          question.resp <- as.character(question.resp)
          question.resp <- replace(question.resp, question.resp=="", NA_character_)
          list.collect.chosen.options <- append(list.collect.chosen.options, list(question.resp))
        }
        
        collect.chosen.options <- bind_cols(list.collect.chosen.options)
        collect.chosen.options <- as.matrix(collect.chosen.options)
        chosen.options <- apply(collect.chosen.options, 1, function(this.row){
          
          picked <- this.row[!is.na(this.row)]
          
          if(length(picked)==0){
            combined <- NA_character_
          }else if(length(picked)==1){
            combined <- paste("{",picked,"}",sep="")
          }else{
            combined <- paste("{",picked[1],"}",sep="")
            for(k in 2:length(picked)){
              combined <- paste("{",picked[k],"}",sep="")
            }
          }
          
          return(combined)
        })
        
        question.resp <- as.matrix(chosen.options)
        colnames(question.resp) <- paste("item.",idx, sep="")
        list.items <- append(list.items,list(question.resp))
      }else{
        question.resp <- rep(NA_character_, times = nrow(df.item))
        question.resp <- as.matrix(question.resp)
        colnames(question.resp) <- paste("item.",idx, sep="")
        list.items <- append(list.items,list(question.resp))
      }
    }
    
    df.resp <- do.call(cbind, list.items)
    df.resp <- as.data.frame(df.resp)
    df <- df.raw %>% select(user.id, merge.id)
    df <- cbind(df, df.resp)
    list.resp.cc2 <- append(list.resp.cc2, list(df))
  }else{
    df.resp <- rep(NA, nrow(df.raw)*length(collect.idx))
    df.resp <- matrix(df.resp, ncol = length(collect.idx))
    df.resp <- as.data.frame(df.resp)
    colnames(df.resp) <- paste("item.",collect.idx, sep="")
    df <- df.raw %>% select(user.id, merge.id)
    df <- cbind(df, df.resp) 
    list.resp.cc2 <- append(list.resp.cc2, list(df))
  }
}

df.resp.cc2 <- do.call(rbind, list.resp.cc2)

