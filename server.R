#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

library(maps)
library(mapproj)
library(quantmod)

download.file("http://shiny.rstudio.com/tutorial/lesson5/census-app/data/counties.rds",
              "counties.rds", mode = "wb")
counties <- readRDS("counties.rds")
#source("Source_file.R")

shinyServer(
  function(input, output) {
    
    output$text1 <- renderText({ 
      paste("You have selected", input$var)
    })
    
    output$text2 <- renderText({ 
      paste("You have chosen a range that goes from",
            input$range[1], "to", input$range[2])
    })
    
    output$text3 <- renderText({ 
      paste("You have chosen a date range",
            input$dates[1], "to", input$dates[2])
    })
    
 
    
    percent_map <- function(var, color, legend.title, min = 0, max = 100) {
      
      # generate vector of fill colors for map
      shades <- colorRampPalette(c("white", color))(100)
      
      # constrain gradient to percents that occur between min and max
      var <- pmax(var, min)
      var <- pmin(var, max)
      percents <- as.integer(cut(var, 100, 
                                 include.lowest = TRUE, ordered = TRUE))
      fills <- shades[percents]
      
      # plot choropleth map
      map("county", fill = TRUE, col = fills, 
          resolution = 0, lty = 0, projection = "polyconic", 
          myborder = 0, mar = c(0,0,0,0))
      
      # overlay state borders
      map("state", col = "white", fill = FALSE, add = TRUE,
          lty = 1, lwd = 1, projection = "polyconic", 
          myborder = 0, mar = c(0,0,0,0))
      
      # add a legend
      inc <- (max - min) / 4
      legend.text <- c(paste0(min, " % or less"),
                       paste0(min + inc, " %"),
                       paste0(min + 2 * inc, " %"),
                       paste0(min + 3 * inc, " %"),
                       paste0(max, " % or more"))
      
      legend("bottomleft", 
             legend = legend.text, 
             fill = shades[c(1, 25, 50, 75, 100)], 
             title = legend.title)
    }
    
    
    if (!exists(".inflation")) {
      .inflation <- getSymbols('CPIAUCNS', src = 'FRED', 
                               auto.assign = FALSE)
    }  
    
    # adjusts yahoo finance data with the monthly consumer price index 
    # values provided by the Federal Reserve of St. Louis
    # historical prices are returned in present values 
    adjust <- function(data) {
      
      latestcpi <- last(.inflation)[[1]]
      inf.latest <- time(last(.inflation))
      months <- split(data)               
      
      adjust_month <- function(month) {               
        date <- substr(min(time(month[1]), inf.latest), 1, 7)
        coredata(month) * latestcpi / .inflation[date][[1]]
      }
      
      adjs <- lapply(months, adjust_month)
      adj <- do.call("rbind", adjs)
      axts <- xts(adj, order.by = time(data))
      axts[ , 5] <- Vo(data)
      axts
    }
    output$map <- renderPlot({
      data <- switch(input$var, 
                     "Percent White" = counties$white,
                     "Percent Black" = counties$black,
                     "Percent Hispanic" = counties$hispanic,
                     "Percent Asian" = counties$asian)
      
      color <- switch(input$var, 
                      "Percent White" = "Red",
                      "Percent Black" = "black",
                      "Percent Hispanic" = "orange",
                      "Percent Asian" = "green")
      
      legend <- switch(input$var, 
                       "Percent White" = "% White",
                       "Percent Black" = "% Black",
                       "Percent Hispanic" = "% Hispanic",
                       "Percent Asian" = "% Asian")
      
      percent_map(var = data, 
                  color = color, 
                  legend.title = legend, 
                  max = input$range[2], 
                  min = input$range[1])
    })
    
    dataInput <- reactive({  
      getSymbols(input$symb, src = "yahoo", 
                 from = input$dates[1],
                 to = input$dates[2],
                 auto.assign = FALSE)
    })
    
    
    finalInput <- reactive({
      if (!input$adjust) return(dataInput())
      adjust(dataInput())
    })
    
    output$plot <- renderPlot({
      chartSeries(finalInput(), theme = chartTheme("white"), 
                  type = "line", log.scale = input$log, TA = NULL)
    })
  }
)