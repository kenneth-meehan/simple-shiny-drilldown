library(ggplot2)
library(DT)
library(tidyr)

#Define user interface:
ui <- fluidPage(
  
  titlePanel("Shiny Drilldown Example"),
  
  fluidRow(
    
    column(width = 2,
           wellPanel(
             helpText("Moving a slider re-randomizes the data."),
             sliderInput("Nschools", "Number of Schools",
                         min = 2, max = 9,
                         value = 5),
             sliderInput("Nmonths", "Number of Months",
                         min = 2, max = 12,
                         value = 4),
             sliderInput("maxNitemsPerSchoolPerMonth", "Max Items per School per Month",
                         min = 1000, max = 5000,
                         value = 2000, step=500),
             radioButtons("abscissa", "x-axis on First Graph",
                          c("Schools" = "Schools",
                            "Months" = "Months"),
                          selected="Schools"),
             checkboxInput("showTable", "Show data table?", value = FALSE)
           )
    ),
    column(width = 10, class = "well",
           h5("Click on first plot to drill down."),
           fluidRow(
             column(width = 6,
                    plotOutput("plot1", height = 300,
                               click="plot1_click"
                    )
             ),
             column(width = 6,
                    plotOutput("plot2", height = 300)
             )
           ),
           fluidRow(
             column(width=12,
                    DT::dataTableOutput("mytable")
             )
           )
    )
    
  )
)

#Define server:
server <- function(input, output) {
  
  y_dimension <- "Nitems"
  
  df <- reactive({
    Nmonths  <- input$Nmonths
    Nschools <- input$Nschools
    maxNitemsPerSchoolPerMonth <- input$maxNitemsPerSchoolPerMonth
    month_names <- format(ISOdate(2018,1:12,1),"%b")[1:Nmonths] #names of first Nmonths months
    Months  <- rep(month_names, each=Nschools)
    Schools <- rep(paste("School",LETTERS[1:Nschools]), times=Nmonths)
    Nitems <- sample(0:maxNitemsPerSchoolPerMonth,Nmonths*Nschools,replace=T)
    d <- as.data.frame(cbind(Months, Schools, Nitems))
    rm(Months, Schools, Nitems)
    d$Nitems <- as.integer(as.character(d$Nitems))
    d$Months <- factor(d$Months, levels=month_names)
    d
  })
  
  output$plot1 <- renderPlot({
    q <- ggplot(df(), aes_string(input$abscissa, as.name(y_dimension))) +
      geom_bar(stat = "identity", fill='goldenrod')
    ylims <<- ggplot_build(q)$layout$panel_scales_y[[1]]$range$range #assign to outer env with <<-
    q
  })
  
  #make graph a second time (as a function), to reference below
  p <- reactive({ggplot(df(), aes_string(input$abscissa, as.name(y_dimension))) +
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
      #Find what bar was clicked. Need p() and not p because p is a function and not an object. Same for df().
      nbars <- length(levels(p()$data[, p()$labels$x]))  #how many bars in the source graph, actually how many values of x labels
      xcuts <- seq(0.5, nbars+0.5, 1)                    #x-values of boundaries bewteen bars' click zones
      ytops <- rep(0, nbars)                             #max y values for each bar; initialize each to 0
      for (bar in 1:nbars){
        ytops[bar] <-  sum(df()[df()[, p()$labels$x]==levels(p()$data[, p()$labels$x])[bar], p()$labels$y]) #add up all y-values for this bar. Sleek!
        if(cx>xcuts[bar] & cx<=xcuts[bar+1] & cy<=ytops[bar]){         #If click is in this bar,
          assign(p()$labels$x, levels(p()$data[, p()$labels$x])[bar])  #Assign to Schools or Months the label of that bar
          break                                                        #Leave loop once you've found where the click was
        }
      }
      if(exists("Schools")){  #if school has been chosen; drill down to nitems by month
        x_pos <- length(unique(df()$Months))/2 + 0.5 #midway across graph
        y_pos <- 0.8*ylims[2] #80% of way up 
        dfs <- df()[df()$Schools==Schools,]
        ggplot(dfs, aes(x=Months, y=Nitems, group=1)) +
          geom_line(size=2) + geom_point(size=5, color='goldenrod') +
          coord_cartesian(ylim=ylims) +  #want same y-scale as 1st graph
          annotate("text", label=Schools, x=x_pos, y=y_pos, size=10)
      }else{
        if(exists("Months")){  #else if month has been chosen; drill down to nitems by school
          x_pos <- length(unique(df()$Schools))/2 + 0.5 #midway across graph
          y_pos <- 0.8*ylims[2] #80% of way up
          dfs <- df()[df()$Months==Months,]
          ggplot(dfs, aes(x=Schools, y=Nitems, group=1)) +
            geom_bar(stat = "identity", fill='goldenrod') +
            coord_cartesian(ylim=ylims) +  #want same y-scale as 1st graph
            annotate("text", label=Months, x=x_pos, y=y_pos, size=10)
        }
      }
    })
    if (input$showTable){
      output$mytable <- renderDataTable({
        wide <- spread(df(), Months, Nitems)     #Go from tall to wide format.
        colnames(wide)[1] <- "School"            #"School" is better than "Schools".
        wide$School <- as.character(wide$School) #Can't add "Totals" to last row if School is a factor.
        wide[nrow(wide)+1,] <- c("Totals", colSums(wide[,2:ncol(wide)])) #Add totals row.
        wide[, 2:ncol(wide)] <- sapply(wide[, 2:ncol(wide)], as.integer) #Convert to integers.
        wide$Totals <- as.integer(0)                                     #Start totals column with 0's.
        wide[,ncol(wide)] <- c(rowSums(wide[,2:(ncol(wide)-1)]))         #Replace 0's with real totals.
        wide[nrow(wide),1] <- "Totals"
        datatable(wide, rownames=FALSE, options=list(paging=FALSE, searching=FALSE, bInfo=FALSE))
      })
    }
    
  })
  
}

shinyApp(ui, server)