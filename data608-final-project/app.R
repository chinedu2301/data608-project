#Name: Chinedu Onyeka and Charles Ugiagbe
library(tidyverse)
library(readr)
library(gridExtra)
library(scales)
library(lubridate)
library(ggrepel)
library(rgdal)
library(plotly)
library(kableExtra)
library(shiny)
library(viridis)

# Read the data
url_data <- "https://raw.githubusercontent.com/chinedu2301/data608-project/main/data/motor_crash_df.csv"
crash_df <- read.csv(url_data)

# Convert crash_date to date-time
crash_df$crash_date <- as.Date(crash_df$crash_date, format = "%Y-%m-%d")

#extract year from date using the lubridate year() function
crash_df$year <- year(crash_df$crash_date) 
#extract Quarters from date
crash_df$quarter <- quarter(crash_df$crash_date) 
#extract Quarters from date
crash_df$quarter <- quarter(crash_df$crash_date) 
#extract weekday from date
crash_df$weekday <- wday(crash_df$crash_date, label=TRUE)
# Extract time of day from the data
time <- as.POSIXct(strptime(c(crash_df$crash_time), "%H:%M"), "UTC")
x=as.POSIXct(strptime(c("0000","0500","1100","1600","1900","2359"),
                      "%H%M"),"UTC")
labs=c("night","morning","afternoon","evening","night")
day_time <- labs[findInterval(time,x)]
crash_df$day_time <- c(day_time)
day_time_df <- crash_df %>% group_by(day_time) %>% summarise(count_incidents = n()) 
day_time_df
#victims per crash incident
crash_df$victims <- crash_df$number_killed + crash_df$number_injured

#choices for shinnyapp
df <- crash_df
choices <- c("Crash by Year", "Crash by Quarter", "Crash by Weekday", "Crash by Time of Day", "Crash by Borough",
             "Victims per Incident")

# Build the UI for question1
ui <- fluidPage(
  # Panel for Crash by Year
  tabsetPanel( 
    tabPanel(title = "NYC Vehicular Incidents",
             sidebarPanel(
               htmlOutput('message_q1'),
               selectInput('plotType', 'Plot to Display', 
                           unique(choices), selected= choices[0],width = 600)
             ),
             mainPanel(plotlyOutput('plot1_q1'))
    )
  ))

# build the server for question1
server <- function(input, output, session) {
  # Question1 Server Side
  data_year <- reactive({
    df2 <- df %>%
      ggplot(aes(x=as.factor(year))) + geom_bar(stat='count', fill='purple') +
      scale_y_continuous(labels=comma) + labs(x='Year', y='No of Incident', title='Incidents by year') + 
      geom_label(stat = "count", aes(label = ..count.., y = ..count..)) + 
      theme(axis.title = element_text(size = 13), plot.title = element_text(size = 15,
                                                                            hjust = 0.5), panel.background = element_rect(fill = "lemonchiffon")) +
      labs(title = "Incidents by Year", y = "No of Incidents")
    df2
  })
  
  data_quarter <- reactive({
    df2 <- df %>% filter(year!=2013) %>% select(year, quarter) %>% group_by(year) %>% count(quarter) %>%
      ggplot(aes(x=as.factor(quarter), y=n, fill = quarter)) + 
      geom_bar(stat='identity') + scale_y_continuous(labels=comma) + facet_grid(.~year) + 
      labs(x='Quarter', y='Number of incidents', title='Incidents by Quarter') + scale_fill_viridis() + 
      theme(axis.title = element_text(size = 13),
            plot.title = element_text(size = 15,
                                      hjust = 0.5), panel.background = element_rect(fill = "lemonchiffon"))
    
  })
  
  data_weekday <- reactive({
    df2 <- df %>% count(weekday) %>%
      ggplot(aes(x=weekday, y=n)) + geom_bar(stat='identity', fill=rainbow(n=7)) +
      scale_y_continuous(labels=comma) +
      labs(x='Weekday', y='Number of incidents', title='Incidents by Weekday') + 
      theme(axis.title = element_text(size = 13),
            plot.title = element_text(size = 15,
                                      hjust = 0.5), panel.background = element_rect(fill = "lemonchiffon"))
    
  })
  
  data_timeofday <- reactive({
    df2 <- day_time_df %>% ggplot(aes(x=reorder(day_time, count_incidents), y=count_incidents)) + 
      geom_bar(stat='identity', fill='purple') +
      scale_y_continuous(labels=comma) +
      labs(x='new_time', y='Number of incidents', title='Incidents by time') + theme(axis.title = element_text(size = 13),
                                                                                     plot.title = element_text(size = 15,
                                                                                                               hjust = 0.5), panel.background = element_rect(fill = "lemonchiffon")) +
      labs(title = "Incidents by time of day", x = "time_of_day")
    
  })    
  
  data_borough <- reactive({
    df2 <- plotly::ggplotly(df %>% count(borough) %>%
                              ggplot(aes(x=reorder(borough, n), y=n, fill=n, text=borough)) +
                              geom_bar(stat='identity', fill='violet') +
                              labs(x='', y='Number of crashes'),
                            tooltip=c("text", "y"))
    
  })
  
  data_victims <- reactive({
    df2 <- ggplotly(df %>% count(contributing_factor) %>%
                      ggplot(aes(x=contributing_factor, y=n, fill=n, text=contributing_factor)) +
                      geom_bar(stat='identity', fill='red') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                      labs(x='', y='Number of crashes'),
                    tooltip=c("text", "y"))
    
  })
  
  
  
  # output Plot
  
  output$plot1_q1 <- renderPlotly({
    if (input$plotType == "Crash by Year"){
      df2 <- data_year()
    } else if(input$plotType == "Crash by Quarter"){
      df2 <- data_quarter()
    } else if(input$plotType == "Crash by Weekday"){
      df2 <- data_weekday()
    } else if(input$plotType == "Crash by Time of Day"){
      df2 <- data_timeofday()
    } else if(input$plotType == "Crash by Borough"){
      df2 <- data_borough()
    } else{
      df2 <- data_victims()
    }
  }
  
  )
}

shinyApp(ui = ui, server = server)
