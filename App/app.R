library(shiny)
library(igraph)

#get the data
pair.causality.CSD.data <- dget("pairwise.causality.regional.CSD.data.RData")

pair.causality.CSD.data$to <- pair.causality.CSD.data$to-1
pair.causality.CSD.data$from <- pair.causality.CSD.data$from - 1

ui <- fluidPage(
  
  #slider that controls the threshold to show
  sliderInput(inputId = "threshold", label="Choose a minimum causality threshold",
    value = 0.0, min = 0.0, max = 1.0),
  #slider that controls which data frame to show
  sliderInput(inputId = "time", label="Choose a time segment",
    value = range(start.times)[1], min = range(start.times)[1], max = range(start.times)[2],
    step=2000, animate=animationOptions(interval=250)),
  #the main pannel where the graph plot is
  mainPanel(plotOutput(outputId = "main.plot")),
  #a sidebar with descriptions of the nodes and their meanings
  sidebarPanel(
    h4("1. ss forelimn"),
    h4("2. ss disgranular"),
    h4("3. parietal, postdorsa"),
    h4("4. 1st visual"),
    h4("5. ss hindlimb"),
    h4("6. ss trunk"),
    h4("7. lateral parietal assoc"),
    h4("8. 2nd visual"),
    h4("9. 1st motor"),
    h4("10. medial parietal assoc"),
    h4("11. 2nd motor"),
    h4("12. retrosplenial disgran"),
    plotOutput(outputId = "side.plot"))
)


server <- function(input, output) {
  
  #draw the image of the threshold legend
  output$side.plot <- renderPlot({
    emptyplot(main = "colorlegend")
    colorlegend(rbPal(10), zlim=c(0,1), dz=0.1, digit=2)
  })
  
  #draw the graph with the specific timepoint and thershold wanted
  output$main.plot <- renderPlot({
    edge.list <- subset(as.data.frame(pair.causality.CSD.data), rho >= input$threshold &
        start.time==input$time, drop=T)
    
    g <- (make_empty_graph() %>%
        add_vertices(12, color = "orange"))
    
    if(length(edge.list$to) > 0)
    {
      edges <- unlist(lapply(1:length(edge.list$to),
        function(i){c(edge.list[i,1], edge.list[i,2])}))
      g <- add_edges(g, edges)
    }
    
    print(paste("ploting threshold",input$threshold))
    
    #restructure the graph to make it look better
    E(g)$width <- edge.list$rho*2
    E(g)$curved <- 0.2
    l = layout.grid(g, width=8)
    l[1,] <- c(0.5, 0) #ss forelimb
    l[2,] <- c(2, 0) #ss disgran
    l[3,] <- c(3, 0) #parietal, postdorsal
    l[4,] <- c(7, 0.5) #1st visual
    l[5,] <- c(0.5, 1) #ss hindlimb
    l[6,] <- c(2, 1) #ss trunk
    l[7,] <- c(3, 1) #lateral parietal assoc
    l[8,] <- c(5.5, 1.5) # 2nd vis
    l[9,] <- c(1,2) #1st motor
    l[10,] <- c(3,2) #medial parietal assoc
    l[11,] <- c(0.5, 3) #2nd motor
    l[12,] <- c(4, 3) #retrosplenial disgran
    
    #create a pallete for the edge colors
    rbPal <- colorRampPalette(c("yellow", "green", "blue", "red"))
#     save_file <- "~/Desktop/SIP/Code/rEDM/region\ threshold\ graphs/"
#     file_name <- paste(save_file,strt.tim,"-threshold-graph.pdf", sep="")
    # pdf(file=file_name)
    #plot the graph with the appropriate edge colors and layout and aspect ratio
    plot(g, edge.arrow.size=.8, vertex.size = 7, vertex.label.cex=.7, edge.color = rbPal(101)[as.numeric(ceiling(edge.list$rho*100+1))],  layout=l, asp=0.4)
      # dev.off()
  })
}

shinyApp(ui=ui, server = server)