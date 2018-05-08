library(ggplot2)

#Set up bogus dataframe:
months <- c("Jan", "Jan", "Jan", "Jan", "Feb", "Feb", "Feb", "Feb", "Mar", "Mar", "Mar", "Mar", "Apr", "Apr", "Apr", "Apr")
schools <- c("School A", "School B", "School C", "School D", "School A", "School B", "School C", "School D",
             "School A", "School B", "School C", "School D", "School A", "School B", "School C", "School D")
nitems <- c(0, 1016, 2001, 501, 666, 1962, 1999, 2019, 1776, 2018, 2525, 3087, 1000, 1000, 1000, 1000)
df <- as.data.frame(cbind(months, schools, nitems))
df$nitems <- as.integer(as.character(df$nitems))
df$months <- factor(df$months, levels=c("Jan", "Feb", "Mar", "Apr"))

#Define user interface:
ui <- fluidPage(
  fluidRow(

    column(width = 12, class = "well",
           h5("Click on left plot to drill down."),
           fluidRow(
             column(width = 6,
                    plotOutput("plot1", height = 500,
                               click="plot1_click"
                    )
             ),
             # column(width = 2,
             #        textOutput("text1")
             # ),
             column(width = 6,
                    plotOutput("plot2", height=500)
             )
           )
    )
    
  )
)

#Define server:
server <- function(input, output) {
  
  #ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    p <<- ggplot(df, aes(schools, nitems)) +
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
      #Find what bar was clicked:
      nbars <- length(levels(p$data[, p$labels$x]))  #how many bars in the source graph, actually how many values of x labels
      xcuts <- seq(0.5, nbars+0.5, 1)                #x-values of boundaries bewteen bars' click zones
      ytops <- rep(0, nbars)                         #max y values for each bar; initialize each to 0
      for (bar in 1:nbars){
       ytops[bar] <-  sum(df[df[, p$labels$x]==levels(p$data[, p$labels$x])[bar], p$labels$y]) #add up all y-values for this bar. CONVOLUTED!
       if(cx>xcuts[bar] & cx<=xcuts[bar+1] & cy<=ytops[bar]){
         school <- levels(p$data[, p$labels$x])[bar]    #a little ugly that we must say school here. Could improve this?
         break   #leave loop once you've found where the click was
       }
      }
    
      if(exists("school")){  #school won't exist if not set above
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