setwd("C:/Users/nmader/Documents/GitHub/after-school-collab/write-ups")
library("knitr")
library("ggplot2")

# Read a sample data set
myData <- read.csv("C:/Users/nmader/Documents/GitHub/after-school-collab/apps/collab-site-comparison-tool/progdata.csv")

colnames(myData)
for (o in unique(myData$org)){
  subData <- myData[myData$org == o,]
  
  ggplot(subData, aes(x = site, y = pct_this, fill = programType)) + geom_bar(stat = "identity") + 
    ggtitle("% This by Site") + xlab("Site") + ylab("% This") +
    theme(axis.text.x = element_text(angle = 90))
  #myFileName <- paste0("./test-reports/fig_pctThis_", o, ".png")
  ggsave("./test-reports/site_graph.png")
  
  knit2pdf("test-knitr-doc-template.Rnw", output = paste0("./test-reports/report_", o, ".tex"))
}