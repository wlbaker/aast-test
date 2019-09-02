library(shinydashboard)

source( "setup.R")

labs <- data.frame(
  names=c( "Creatinine", "Troponin", "Myoglobin",  "Lactate", "pH"),
  y_min=c( 0, 0, 0, 0, 7.0 ),
  y_max=c( 3.7, 25, 2000, 16, 8.0 )
)

computeOneAnalysis <- function( labName, y_min, y_max, grps ) {
  # read the data
  xls <- "../data/labs.xlsx"
  h <- read_xlsx( xls, sheet=labName )
  
  # convert the group to factors, ordering the factors in chronological order
  h$group <- factor( h$group, levels=grps )
  
  dd <- createGroupSubset( h, grps )
  analysis <- createAggregateAnalysis( dd, grps )
}  


function(input, output, session) {


  output$labPlot <- renderPlot({
    
    grps <- input$checkGroup
    labName <- input$lab
    y_min <- input$y_min
    y_max <- input$y_max
    myAnalysis <- computeOneAnalysis( labName, 0, 3.7, grps)

    tabbedMeans <- tapply(myAnalysis$mean, list(myAnalysis$grp,
                                                myAnalysis$TP),
                          function(x) c(x = x))
    
    barCenters <- barplot(height = tabbedMeans,
                          beside = TRUE, las = 1,
                          ylim = c(y_min, y_max), xpd=FALSE,
                          xaxt = "n",
                          main = "",
                          ylab = labName,
                          xlab = "",
                          border = "black", axes = TRUE,
                          legend.text = TRUE,
                          font=2, font.lab=2,
                          cex.lab=1.5,  # font expansion factor
                          args.legend = list(title = "Group", 
                                             x = "topright"
                          ))
    # add standard-error-of-the-mean indicators to the graph
    
    tapply(myAnalysis$mean, list(myAnalysis$grp, myAnalysis$TP),
           function(x) c(x = x))    
    
    tabbedSE <- tapply(myAnalysis$se, list(myAnalysis$grp,
                                           myAnalysis$TP),
                       function(x) c(x = x))
    
    dy = (y_max - y_min) * 0.02
    text(x = barCenters, y = par("usr")[3] - dy, srt = 30, 
         adj = 1, labels = myAnalysis$names, xpd = TRUE, font=2, cex=1.3)
    
    segments(barCenters, tabbedMeans - tabbedSE, barCenters,
             tabbedMeans + tabbedSE, lwd = 1.5)
    
    arrows(barCenters, tabbedMeans - tabbedSE, barCenters,
           tabbedMeans + tabbedSE, lwd = 1.5, angle = 90,
           code = 3, length = 0.05)
    
    # tidy up the graph with a line on the bottom row
    abline(a = y_min, b = 0)
    
  })
}
