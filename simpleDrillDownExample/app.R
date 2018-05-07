library(ggplot2)

months <- c("Jan", "Jan", "Jan", "Feb", "Feb", "Feb", "Mar", "Mar", "Mar")
schools <- c("School A", "School B", "School C", "School A", "School B", "School C","School A", "School B", "School C")
nitems <- c(0, 1016, 2001, 666, 1962, 1999, 1776, 2018, 2525)
df <- as.data.frame(cbind(months, schools, nitems))
df$nitems <- as.integer(as.character(df$nitems))


ui <- fluidPage(
  fluidRow(

    column(width = 12, class = "well",
           h5("Left plot controls right plot. Brush to zoom. Click outside brush area to reset."),
           fluidRow(
             column(width = 6,
                    plotOutput("plot1", height = 500,
                               brush = brushOpts(
                                 id = "plot1_brush",
                                 resetOnNew = TRUE
                               ),
                               click="plot1_click"
                    )
             ),
             column(width = 6,
                    plotOutput("plot2", height = 500)
             )
           )
    )
    
  )
)

server <- function(input, output) {
  
  # Linked plots (left and right)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    ggplot(df, aes(schools, nitems)) +
      geom_bar(stat = "identity", fill='goldenrod')
  })
  
  output$plot2 <- renderPlot({
    ggplot(df, aes(schools, nitems)) +
      geom_bar(stat = "identity", fill='goldenrod') +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

}

shinyApp(ui, server)