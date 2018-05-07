library(ggplot2)

months <- c("Jan", "Jan", "Jan", "Feb", "Feb", "Feb", "Mar", "Mar", "Mar")
schools <- c("School A", "School B", "School C", "School A", "School B", "School C","School A", "School B", "School C")
nitems <- c(0, 1016, 2001, 666, 1962, 1999, 1776, 2018, 2525)
df <- as.data.frame(cbind(months, schools, nitems))
df$nitems <- as.integer(as.character(df$nitems))


ui <- fluidPage(
  fluidRow(

    column(width = 12, class = "well",
           h5("Click on plot to see coordinates at right."),
           fluidRow(
             column(width = 5,
                    plotOutput("plot1", height = 500,
                               click="plot1_click"
                    )
             ),
             column(width = 3,
                    h3(textOutput("text1"))
             ),
             column(width = 3,
                    h3(textOutput("text2"))
             )
           )
    )
    
  )
)

server <- function(input, output) {
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    ggplot(df, aes(schools, nitems)) +
      geom_bar(stat = "identity", fill='goldenrod')
  })

  observe({
    if(is.null(input$plot1_click$x)) return(NULL)
    click <- c(input$plot1_click$x, input$plot1_click$y)
    #print(click) #Unneeded diagnostic output to Console

    output$text1 <- renderText({
      expr=click
    })
    
    output$text2 <- renderText({
      expr=round(input$plot1_click$y)
    })
    

  })

}

shinyApp(ui, server)