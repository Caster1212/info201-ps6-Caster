

library(shiny)
library(ggplot2)
library(dplyr)

temperature_data <- read.delim(file.path("data","UAH-lower-troposphere-long.csv"))

ui <- fluidPage(
    tabsetPanel(
      
      tabPanel("about",
               p("This app uses", em("satellite temperature data")," from UAH"),
               p("Temperature temp is measured as deviation (deg C)
                 from 1991-2020 baseline"), 
               p("The dataset contains 14310 observations and 5 variables
                 Here is a small (random) sample of data:"),
               fluidRow(
                 column(width = 3,
                        tableOutput("sample_data"))
               )
               ),
      tabPanel("Plots", 
               p("You can analyze the global temperature for different regions. 
                  Select the regions you are interested in. 
                  You see a monthly scatterplot "),
               sidebarLayout(
                 sidebarPanel(
                   fluidRow(
                     column(width = 12,
                            checkboxGroupInput("trend", "show trend(s) or not", 
                                               choices = c("show trend(s)")))
                     ),
                     column(width = 12,
                            uiOutput("regionChoice")
                   )
                 ),
                 mainPanel(
                   plotOutput("plot"), 
                   textOutput("number")
                 )
               )),
      tabPanel("Tables", 
               sidebarLayout(
                 sidebarPanel(
                   p("This panel displays average temperature over different 
                  time periods: months , years and decades"),
                   fluidRow(
                     column(width = 10,
                            radioButtons("Average_temp", "Average over:", 
                                         choices = c("month", "year", "decade")))
                   )
                 ), 
                 mainPanel(
                   textOutput("limit_temp"),
                   tableOutput("table")
                 )
               ))
      
    )
    
    
)


server <- function(input, output) {
  
  output$sample_data <- renderTable({
    temperature_data %>% 
      sample_n(6)
  })
  
  output$regionChoice <- renderUI({
    checkboxGroupInput("regions",
                 "Select the region(s) to display", 
                 choices = unique(temperature_data$region))
  })
  
  hugeSample <- temperature_data %>% 
    sample_n(10000)
  
  filterRegions <- reactive({
    hugeSample %>%
      filter(region %in% input$regions) 
  })
  
  output$plot <- renderPlot({
    ggplot(filterRegions(), aes(year, temp, color = region, linewidth = 1)) +
      geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
      if ("show trend(s)" %in% input$trend) {
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE, span = 1)
      }
  })
  
  output$number <- renderText({
    paste("Time period Inf- -Inf. In total 0 non-missing observations")
    data <- temperature_data %>% 
      filter(region %in% input$regions)
    paste("Time period Inf- -Inf. In total",nrow(data)," non-missing observations")
  
  })
  
  month_temp <- reactive({temperature_data %>% 
      filter(region == "globe") %>% 
    group_by(year, month) %>% 
      select(year, month, temp)
  })
  
  output$table <- renderTable({
    if (input$Average_temp == "month") {
      month_temp()
    } else if (input$Average_temp == "year") {
      temperature_data %>% 
        filter(region == "globe") %>% 
        group_by(year) %>% 
        summarise(temp = mean(temp))
    } else {
      temperature_data %>% 
        filter(region == "globe") %>% 
        mutate(decade = floor(year/10) * 10) %>% 
        group_by(decade) %>% 
        summarise(temp = mean(temp))
    }
  })
  
  output$limit_temp <- renderText({
    if (input$Average_temp == "month") {
    data2 <- temperature_data %>% 
      filter(region == "globe") %>% 
      group_by(year,month)
    paste("Temperature range", min(data2$temp), "-",
          max(data2$temp))
    } else if (input$Average_temp == "year") {
      data2 <- temperature_data %>% 
        filter(region == "globe") %>% 
        group_by(year) %>% 
        summarise(temp = mean(temp))
      paste("Temperature range", min(data2$temp), "-",
            max(data2$temp))
    } else {
      data2 <- 
        temperature_data %>% 
        filter(region == "globe") %>% 
        mutate(decade = floor(year/10) * 10) %>% 
        group_by(decade) %>% 
        summarise(temp = mean(temp))
      paste("Temperature range", min(data2$temp), "-",
            max(data2$temp))
    }
      
  })
    
}

shinyApp(ui = ui, server = server)
