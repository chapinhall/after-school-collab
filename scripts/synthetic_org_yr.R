#---------------------------------------------#
#---------------------------------------------#
# MODIFYING SCRAMBLED DATA FOR ANALYSIS       # 
#                 DEMOS                       #
# Author: Emily Wiegand		                    #  
#---------------------------------------------#
#---------------------------------------------#

# This script takes the output of scramble_data and creates artificial variation in orgs and years
# for demonstration purposes.


##Set up workspace and designate/update file locations

library(plyr)

rm(list=ls())
#myDir <- "/projects/Integrated_Evaluation_Youth_Support_Services/"
myDir <- "H:/Integrated Evaluation Project for YSS Providers"
setwd(myDir)
dataPath <- "./data/preprocessed-data/" # File path to save synthetic data

load(file = paste0(dataPath,"Scram.Rda"))

org2sites <- c(LETTERS[14:26])

Scram$Org[Scram$fAnyYss=='Org Alpha'] <- "Alpha"
Scram$Org[Scram$Org==''] <- "None"
Scram$Org[Scram$cYssSite %in% org2sites] <- "Beta"

Scram$Year <- "2008"
Scram$Year[sample(nrow(Scram), 200000)] <- "2009"
Scram$Year[sample(nrow(Scram), 170000)] <- "2010"
Scram$Year[sample(nrow(Scram), 130000)] <- "2011"
Scram$Year[sample(nrow(Scram), 100000)] <- "2012"

todrop <- c("fAnyYss", "fYssSite", "fShortSite", "fYssType")
Scram2 <- Scram[,!(names(Scram) %in% todrop)]


## Save data to file
save(Scram2, file = paste0(dataPath,"ScramPlusYrOrg.Rda"))
