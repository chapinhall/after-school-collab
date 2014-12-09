#
# Server-side operations for site comparison tool
#

library(shiny)
#setwd("C:/Users/nmader/Documents/GitHub/after-school-collab/apps/collab-site-comparison-tool/")
inInt <- function(x, int) return((int[1] <= x & x <= int[2]))

progdata <- read.csv("./progdata.csv", stringsAsFactors = F)
o.u <- unique(progdata$org)
p.u <- unique(progdata$program)

shinyServer(function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    
    # Included Orgs
    myOrgIncl    <- (input$inclMy    == T) & (progdata$org == input$myOrg)
    otherOrgIncl <- (input$inclOther == T) & (progdata$org != input$myOrg)
    orgIncl <- myOrgIncl | otherOrgIncl
    
    print(input$programType)
    print(input$program)
    
    # Included program types
      if (input$programType == "All") {
        ptIncl <- 1 == 1
      } else {
        ptIncl <- (progdata$programType == input$programType)
      }
    # Included program
#       if (input$program == "All") {
#         pIncl <- 1 == 1
#       } else {
#         pIncl <- (progdata$program == input$program)
#       }
    #print(cbind(progdata[, c("org", "programType", "program")], orgIncl, ptIncl, pIncl))
    #print(input$program)
    
    mySub <- subset(progdata, inInt(pct_this,  input$this)  &
                              inInt(pct_that,  input$that)  &
                              inInt(pct_other, input$other) &
                              orgIncl & ptIncl) #  & pIncl
    dim(mySub)
    mySub <- mySub[, c("org", "site", "programType", "program", "pct_this", "pct_that", "pct_other", "num_things", "narrative")]
    colnames(mySub) <- c("Org", "Site", "Program Type", "Program", "% This", "% That", "% Other", "# things", "Narrative of Practice")
    return(mySub)
  })
  
  # Create the program list based on the program type selected
  output$programList <- renderUI({
#     print(input$programType)
#     x <- switch(input$programType,
#            "All"       = c("All", c.u, g.u),
#            "Composers" = c("All", c.u),
#            "Games"     = c("All", g.u)
#          )
#     print(x)
    return("All")
  })
  
  # Generate a summary of the dataset
  output$view <- renderDataTable({
    datasetInput()
    }, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5, bSortClasses = TRUE, bAutoWidth = FALSE,
                      aoColumn = list(list(sWidth = "30px", sWidth = "30px", sWidth = "30px", sWidth = "30px",
                                           sWidth = "20px", sWidth = "20px", sWidth = "20px", sWidth = "20px",
                                           sWidth = "150px"))
               )
  )

})