library(ggplot2)

#Set up bogus dataframe:
months <- c("Jan", "Jan", "Jan", "Feb", "Feb", "Feb", "Mar", "Mar", "Mar", "Apr", "Apr", "Apr")
schools <- c("School A", "School B", "School C", "School A", "School B", "School C",
             "School A", "School B", "School C", "School A", "School B", "School C")
nitems <- c(0, 1016, 2001, 666, 1962, 1999, 1776, 2018, 2525, 1000, 1000, 1000)
df <- as.data.frame(cbind(months, schools, nitems))
df$nitems <- as.integer(as.character(df$nitems))
df$months <- factor(df$months, levels=c("Jan", "Feb", "Mar", "Apr"))

#Define user interface:
ui <- fluidPage(
  fluidRow(

    column(width = 12, class = "well",
           h5("Click on left plot to see coordinates and drill down."),
           fluidRow(
             column(width = 5,
                    plotOutput("plot1", height = 500,
                               click="plot1_click"
                    )
             ),
             column(width = 2,
                    textOutput("text1")
             ),
             column(width = 5,
                    plotOutput("plot2", height=500)
             )
           )
    )
    
  )
)

#Define server:
server <- function(input, output) {
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    p <- ggplot(df, aes(schools, nitems)) +
         geom_bar(stat = "identity", fill='goldenrod')
    ylims <<- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range #assign to outer env with <<-
    p
  })


  observe({
    if(is.null(input$plot1_click$x)) return(NULL)
    clicktext <- c(input$plot1_click$x, input$plot1_click$y)
    cx <- input$plot1_click$x
    cy <- input$plot1_click$y

    output$text1 <- renderText({
      expr=paste("click coords:",clicktext[1], clicktext[2])
    })

    output$plot2 <- renderPlot({
      #Find what school was clicked for:
      school=ifelse(cx>0.5 & cx<=1.5 & cy<=sum(df$nitems[df$schools=="School A"]),"School A",
                  ifelse(cx>1.5 & cx<=2.5 & cy<=sum(df$nitems[df$schools=="School B"]), "School B",
                         ifelse(cx>2.5 & cx<=3.5 & cy<=sum(df$nitems[df$schools=="School C"]), "School C",
                                "")))
      if(!school==""){
        x_pos <- length(unique(df$months))/2 + 0.5 #midway across graph
        y_pos <- 0.8*ylims[2] #80% of way up 
        dfs <- df[df$schools==school,]
        ggplot(dfs, aes(x=months, y=nitems, group=1)) +
          geom_line(size=2) + geom_point(size=5, color='goldenrod') +
          #geom_bar(stat = "identity", fill='goldenrod') +
          coord_cartesian(ylim=ylims) +  #want same y-scale as 1st graph
          annotate("text", label=school, x=x_pos, y=y_pos, size=10)
      }
    })
    

  })

}

shinyApp(ui, server)