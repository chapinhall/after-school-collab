#
# UI for the site comparison tool
#

# Stolen from the Movie Explore under the R Shiny Gallery -- http://shiny.rstudio.com/gallery/movie-explorer.html

library(ggvis)
library(shiny)
library(ggplot2)
#setwd("C:/Users/nmader/Documents/GitHub/after-school-collab/apps/collab-site-comparison-tool/")
progdata <- read.csv("./progdata.csv", stringsAsFactors = F)
o.u  <- unique(progdata$org)
pt.u <- c("All", unique(progdata$programType))
g.u  <- with(progdata, c("All", unique(program[programType == "Games"])))
c.u  <- with(progdata, c("All", unique(program[programType == "Composers"])))

shinyUI(fluidPage(
  titlePanel("Site Comparison Tool"),
  fluidRow(
    column(3,
      fluidRow(
        wellPanel(
          h4("Filter Org and Programs"),
          selectInput("myOrg",       "My Organization", o.u),
          selectInput("programType", "Program Type",    pt.u),
          selectInput("program",     "Program",         c("All")),
          checkboxInput("inclMy",    "Include my sites",          value = T),
          checkboxInput("inclOther", "Include other org's sites", value = T)
        )
      ),
      fluidRow(
        wellPanel(
          h4("Filter Characteristics"),
          sliderInput(inputId = "this",  label = "% This",  min = 0, max = 1, value = c(0, 1), ticks = T, format = "#0%"),
          sliderInput(inputId = "that",  label = "% That",  min = 0, max = 1, value = c(0, 1), ticks = T, format = "#0%"),
          sliderInput(inputId = "other", label = "% Other", min = 0, max = 1, value = c(0, 1), ticks = T, format = "#0%")
        )
      )
    ),
    column(9,
       h4("Sites meeting criteria"),
      #tableOutput("view")
      dataTableOutput(outputId="view")
    )
  )
))