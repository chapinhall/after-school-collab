##### MISC CODE


# Output program lists
  setwd("H:/Integrated Evaluation Project for YSS Providers/data/preprocessed-data/")
  for (org in c("ASM", "CHASI", "UCAN", "YMCA")){
    load(paste0("./", org, "/", org, "programs.Rda"))
    prog <- get(paste0(org, "programs"))
    rownames(prog) <- NULL
    #prog <- prog[order(prog[, c("year", "program")]),] ... doesn't work. Not all program files have these headings
    write.csv(prog, file = paste0("./", org, "/", org, "programs.csv"))
  }
  
