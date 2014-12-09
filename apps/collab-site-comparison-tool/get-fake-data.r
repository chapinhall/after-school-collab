# Generate data for program comparison utility

# Want to generate a fake table of sites, where each record has information about the organization, average youth characteristics, and some
#   narrative information. We will randomly sample lists of data for categorical values, and generate random numbers for continuous values.

  rm(list = ls())
#  setwd("C:/Users/nmader/Documents/GitHub/after-school-collab/apps/collab-site-comparison-tool/")

# Bring in categorical lists
  planets    <- read.csv("./materials-for-generating-fake-data/planets.csv",           stringsAsFactors = FALSE)
  countries  <- read.csv("./materials-for-generating-fake-data/countries.csv",         stringsAsFactors = FALSE)
  gePassages <- read.csv("./materials-for-generating-fake-data/GreatExpectations.csv", stringsAsFactors = FALSE)
  games      <- read.csv("./materials-for-generating-fake-data/board_games.csv",       stringsAsFactors = FALSE)
  composers  <- read.csv("./materials-for-generating-fake-data/composers.csv",         stringsAsFactors = FALSE)
  
  # Subset some lists to be a little smaller -- we want them to be categorical, but not too large
    Games     <- games[1:15, ]
    Composers <- composers[1:15, ]
    passLen <- sapply(gePassages, nchar)
    gePassages <- gePassages[100 < passLen & passLen < 300, 1]

# Sample categorical values
  mySample <- function(myList, n, repl) {
    m <- switch(class(myList),
                "character"  = myList[sample(1:length(myList), n, replace = repl)],
                "data.frame" = myList[sample(1:nrow(myList),   n, replace = repl), 1])
  }
  
  n <- nrow(countries)
  mydata <- data.frame(org  = mySample(planets,   n, T),
                       site = mySample(countries, n, F),
                       pct_this  = round(runif(n), 2),
                       pct_that  = round(runif(n), 2),
                       pct_other = round(runif(n), 2),
                       num_things = round(rnorm(n, mean = 250, sd = 50), 0),
                       narrative = mySample(gePassages, n, F))
  mydata <- within(mydata, {
    programType <-  sample(c("Games", "Composers"), n, replace = T)
    program <- sapply(programType, function(x) sample(get(x), 1))
  })

write.csv(mydata, file = "./progdata.csv", row.names = F)

