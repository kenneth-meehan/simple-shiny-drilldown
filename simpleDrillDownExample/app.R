library(ggplot2)

#Set up bogus dataframe:
Months <- c("Jan", "Jan", "Jan", "Jan",
            "Feb", "Feb", "Feb", "Feb",
            "Mar", "Mar", "Mar", "Mar",
            "Apr", "Apr", "Apr", "Apr",
            "May", "May", "May", "May")
Schools <- c("School A", "School B", "School C", "School D", "School A", "School B", "School C", "School D",
             "School A", "School B", "School C", "School D", "School A", "School B", "School C", "School D",
             "School A", "School B", "School C", "School D")
Nitems <- c(0, 1016, 2001, 501, 666, 1962, 1999, 2019, 1776, 2018, 2525, 3087, 1000, 1000, 1000, 1000, 500, 500, 500, 500)
df <- as.data.frame(cbind(Months, Schools, Nitems))
rm(Months)
rm(Schools)
rm(Nitems)
df$Nitems <- as.integer(as.character(df$Nitems))
df$Months <- factor(df$Months, levels=c("Jan", "Feb", "Mar", "Apr", "May"))



#Define user interface:
ui <- fluidPage(
  
  titlePanel("Shiny Drilldown Example"),
  
  fluidRow(
    
    column(width = 2,
           wellPanel(
             radioButtons("abscissa", "x-axis on left graph",
                          c("Months" = "Months",
                            "Schools" = "Schools"),
                          selected="Months")
           )
    ),

    column(width = 10, class = "well",
           h5("Click on left plot to drill down."),
           fluidRow(
             column(width = 6,
                    plotOutput("plot1", height = 300,
                               click="plot1_click"
                    )
             ),
             column(width = 6,
                    plotOutput("plot2", height = 300)
             )
           )
    )
    
  )
)

#Define server:
server <- function(input, output) {
  
  y_dimension <- "Nitems"
  
  output$plot1 <- renderPlot({
    q <- ggplot(df, aes_string(input$abscissa, as.name(y_dimension))) +
         geom_bar(stat = "identity", fill='goldenrod')
    ylims <<- ggplot_build(q)$layout$panel_scales_y[[1]]$range$range #assign to outer env with <<-
    q
    })

  #make graph a second time (as a function), to reference below
  p <- reactive({ggplot(df, aes_string(input$abscissa, as.name(y_dimension))) +
                 geom_bar(stat = "identity", fill='goldenrod')})
  
  observe({
    if(is.null(input$plot1_click$x)) return(NULL)
    clicktext <- c(input$plot1_click$x, input$plot1_click$y)
    cx <- input$plot1_click$x
    cy <- input$plot1_click$y

    output$text1 <- renderText({
      expr=paste("click coords:",clicktext[1], clicktext[2])
    })

    output$plot2 <- renderPlot({
      #Find what bar was clicked. Need p() and not p because p is a function and not an object.
      nbars <- length(levels(p()$data[, p()$labels$x]))  #how many bars in the source graph, actually how many values of x labels
      xcuts <- seq(0.5, nbars+0.5, 1)                    #x-values of boundaries bewteen bars' click zones
      ytops <- rep(0, nbars)                             #max y values for each bar; initialize each to 0
      for (bar in 1:nbars){
       ytops[bar] <-  sum(df[df[, p()$labels$x]==levels(p()$data[, p()$labels$x])[bar], p()$labels$y]) #add up all y-values for this bar. CONVOLUTED!
       if(cx>xcuts[bar] & cx<=xcuts[bar+1] & cy<=ytops[bar]){
         assign(p()$labels$x, levels(p()$data[, p()$labels$x])[bar]) #now more generic! but now we set schools, not school...
         break                                                       #leave loop once you've found where the click was
       }
      }
      if(exists("Schools")){  #if school has been chosen; drill down to nitems by month
        x_pos <- length(unique(df$Months))/2 + 0.5 #midway across graph
        y_pos <- 0.8*ylims[2] #80% of way up 
        dfs <- df[df$Schools==Schools,]
        ggplot(dfs, aes(x=Months, y=Nitems, group=1)) +
          geom_line(size=2) + geom_point(size=5, color='goldenrod') +
          coord_cartesian(ylim=ylims) +  #want same y-scale as 1st graph
          annotate("text", label=Schools, x=x_pos, y=y_pos, size=10)
      }else{
      if(exists("Months")){  #else if month has been chosen; drill down to nitems by school
        x_pos <- length(unique(df$Schools))/2 + 0.5 #midway across graph
        y_pos <- 0.8*ylims[2] #80% of way up
        dfs <- df[df$Months==Months,]
        ggplot(dfs, aes(x=Schools, y=Nitems, group=1)) +
          geom_line(size=2) + geom_point(size=5, color='goldenrod') +
          coord_cartesian(ylim=ylims) +  #want same y-scale as 1st graph
          annotate("text", label=Months, x=x_pos, y=y_pos, size=10)
      }
      }
    })
    

  })

}

shinyApp(ui, server)