library(shiny)
library(igraph)
library(shape)

#get the data
# setwd("~/Desktop/SIP/Code/App")


#get the data and determine the time.step between analysis
# pair.causality.CSD.data <- dget("pairwise.causality.regional.CSD.data.RData")
start.times <- unique(pair.causality.CSD.data$start.time)
time.windows <- unique(pair.causality.CSD.data$time.window)
time.steps <- unique(pair.causality.CSD.data$time.step)
animation.speed <- 1000

#create a pallete for the edge colors
rbPal <- colorRampPalette(c("yellow", "green", "blue", "red"))



#create the UI for the app
ui <- fluidPage(
  
  #title
  title="Sugihara Causality Graph",
  
  #the main graph output placeholder
  plotOutput(outputId = "main.plot"),
  
  
  #the settings and options portion
  fluidRow(
    
    #first column
    column(3,
      #slider that controls the threshold to show
      sliderInput(inputId = "threshold", label="Choose a minimum causality threshold",
        value = 0.0, min = 0.0, max = 1.0),
      #slider that controls which data frame to show
      sliderInput(inputId = "time", label="Choose a time segment",
        value = range(start.times)[1], min = range(start.times)[1], max = range(start.times)[2],
        step=time.steps[1], animate=animationOptions(animation.speed))
    ),
    
    #second column
    column(3,
      #selector to select the time window desired
      selectInput(inputId = "time.window", label="Choose a time window size", selected= time.windows[1],
        choices=time.windows),
      #selector to select the time step desired
      selectInput(inputId = "time.step", label="Choose a time step size",  selected = time.steps[1],
        choices=time.steps)
    ),
    
    #third column
    column(3,
      #a sidebar with descriptions of the nodes and their meanings
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
          h4("12. retrosplenial disgran")
    ),
    
    #4th column
    column(3,
      plotOutput(outputId = "side.plot")
    )

  )
)


#server processing
server <- function(input, output, session) {
  
  #changing the data frame selected depending on what time step and time window are selected
  d <- reactive({
    print("changind d")
    subset((pair.causality.CSD.data), rho >= input$threshold &
        time.window==input$time.window & time.step==input$time.step, drop=T)
  })
  
  #update the slider options when the user selects different time step
  observe({
    val <- input$time
    updateSliderInput(session, "time", label="Choose a time segment",
      min = range(start.times)[1], max = range(start.times)[2],
      step=input$time.step)
  })
  
  # draw the image of the threshold legend
  output$side.plot <- renderPlot({
    emptyplot(main = "Causality colors")
    colorlegend(rbPal(10), zlim=c(0,1), dz=0.1, digit=2, posx = c(0.475, 0.525))
  })
  
  #draw the graph with the specific timepoint and thershold wanted
  output$main.plot <- renderPlot({
    
    edge.list <- subset(d(), start.time==input$time, drop=T)
    
    #make an empty graph
    g <- (make_empty_graph() %>%
        add_vertices(12, color = "orange"))
    
    print(input$time)
    #then add the edges if there are any
    if(length(edge.list$to) > 0)
    {
      edges <- unlist(lapply(1:length(edge.list$to),
        function(i){c(edge.list[i,1], edge.list[i,2])}))
      g <- add_edges(g, edges)
    }
    
    
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
    
    #finally plot the graph
    plot(g, edge.arrow.size=.8, vertex.size = 7, vertex.label.cex=.7, edge.color = rbPal(101)[as.numeric(ceiling(edge.list$rho*100+1))],  layout=l, asp=0.4)
  })
}

shinyApp(ui=ui, server = server)