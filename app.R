# Application
library(shiny)
library(shinythemes)
library(markdown)
library(plotly)

# Data Wrangling
library(tidyverse)
library(magrittr)
library(lubridate)



theme_set(theme_minimal())
mypath<- "/home/ubuntu/Projects/data/"
df_alpha <- read.csv(paste0(mypath,"disease_n_disaster_final.csv"))
options(scipen = 100)

# Data Preparation

to_order <- c("Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan")

#to_order <- c("dic", "nov", "oct", "sep", "ago", "jul", "jun", "may", "abr", "mar", "feb", "ene")


df_alpha %<>% mutate(Date = parse_date_time(Date, "%d/%m/%y") %>% as_date(), #as.Date(Date),
                     Year = year(Date) %>% factor(ordered = T, levels = 2010:(Sys.Date() %>% year)),
                     Month = months(Date, abbreviate = T) %>% factor(ordered = T, levels = to_order),
                     Quarter = quarters(Date))

df_lambda <- df_alpha %>% group_by(Country, Event, Month) %>% summarise(Avg = mean(Value))
df_alpha %<>% left_join(df_lambda, by = c("Country", "Event", "Month"))
df_alpha %<>% mutate(Category = case_when(Value / Avg <= 0.75 ~ "Green",
                                          Value / Avg >= 1.25 ~ "Red",
                                          TRUE ~ "Yellow"))

# Defining UI
ui <- navbarPage(#title = "Relief Web v1.0",
                 title=div(img(src="reliefweb.png", height="12%", width="12%", align = "left")),
                 theme = shinytheme("flatly"), #simplex
                 tabPanel(title = "Dashboard",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                              #tags$style(".well {background-color:#dec4de;}"),
                              # Selecting a disease
                              selectInput(inputId = "mainevent", label = strong("Indicator"),
                                          choices = unique(df_alpha$Event), selected = "Dengue"),
                              
                              # Selecting countries
                              selectInput(inputId = "country", label = strong("Countries"),
                                          multiple = TRUE, choices = NULL, selected = NULL),
                              
                              # Selecting years to be plotted
                              dateRangeInput(inputId = "date", label = strong("Date Range"),
                                             start =min(df_alpha$Date), end = Sys.Date(),
                                             min = min(df_alpha$Date), max = Sys.Date(),
                                             format = "yyyy-mm-dd", startview = "month",
                                             weekstart = 0, language = "en", separator = " to "),
                              
                              # If another event available for a chosen country to add a regression plot
                              conditionalPanel(condition = "output.ncountries == 1 && output.nevents > 1 ",
                                               selectInput(inputId = "auxevent",
                                                           label = strong("Compare with Indicator"),
                                                           choices = NULL,
                                                           selected = NULL),
                              ),
                              
                              # Whether to include country-specific regression lines
                              conditionalPanel(condition = "output.ncountries > 1",
                                               checkboxInput(inputId = "contrasts",
                                                             label = strong("Show Country-specific Trends"),
                                                             value = FALSE)
                              ),
                              hr(),
                              h3("Description"),
                              HTML('ReliefWeb dashboard for disease and disaster monitoring in the
                                 Asia-Pacific region. Version 1.0.<br>For more information, contact the developer: 
                                      <a href="https://github.com/alinacherkas" target="_blank">Alina Cherkas</a>.'),
                              
                              
                            ),
                            # Output
                            mainPanel(width = 9,
                              tabsetPanel(
                                tabPanel("Compare Countries",
                                         fluidRow(
                                           splitLayout(cellWidths = c("50%", "50%"),
                                                       plotlyOutput(outputId = "mainplot"),
                                                       plotlyOutput(outputId = "auxplot"))
                                         )
                                ),
                                tabPanel("Compare Events",
                                         fluidRow(
                                           splitLayout(cellWidths = c("50%", "50%"),
                                                       plotlyOutput(outputId = "eventslineplot"),
                                                       plotlyOutput(outputId = "eventsplot"))
                                         )
                                )
                              )
                              
                            )
                          )
                 ),
                 tabPanel("About Us",
                          img(src='ReliefWeb logo.svg', height="40%", width="40%", align = "right"),
                          includeMarkdown("./www/about.md")
                 ),
                 tabPanel("What We Do",
                          includeMarkdown("./www/whatwedo.md")
                 )
)

# Define server function
server <- function(input, output, session) {
  #Updating the choice of countries depending on the choice of the main event
  observeEvent(input$mainevent, {
    choices <- df_alpha %>% filter(Event == input$mainevent) %>% pull(Country) %>% unique()
    updateSelectInput(session = session, inputId =  "country", choices = choices, selected = "Sri Lanka") 
  })
  
  # Checkbox conditional on the number of countries selected
  output$ncountries <- reactive({
    length(input$country)
  })
  
  output$nevents <- reactive({
    df_alpha %>% filter(Country == input$country) %>% pull(Event) %>% n_distinct()
  })
  
  outputOptions(output, 'ncountries', suspendWhenHidden=FALSE)
  outputOptions(output, 'nevents', suspendWhenHidden=FALSE)
  
  # Adds only auxiliarry events Available for a given country, less the chosen main event
  observeEvent(input$country, {
    choices <- df_alpha %>% filter(Country == input$country,
                                   Event != input$mainevent) %>% pull(Event) %>% unique()
    if(identical(choices, character(0))) {
      updateSelectInput(session = session, inputId =  "auxevent", selected = NULL)
    } else {
      updateSelectInput(session = session, inputId =  "auxevent", choices = choices)
    }
    
  })
  
  # Subset data
  selected_data <- reactive({
    req(input$date)
    df_alpha %>% filter(Event == input$mainevent, Country %in% input$country,
                        Date >= input$date[1] & Date <= input$date[2]) %>% drop_na()
  })
  
  # Data for auxiliary plots
  selected_data2 <- reactive({
    req(input$auxevent, input$country)
    events <- c(input$mainevent, input$auxevent)
    df_alpha %>% filter(Event %in% events, Country %in% input$country,
                        Date >= input$date[1] & Date <= input$date[2]) %>% drop_na()
  })
  
  # Create mainplot object the plotOutput function is expecting
  output$mainplot <- renderPlotly({
    req(input$country)
    df_lambda <- selected_data()
    n.Years <- year(df_lambda$Date) %>% n_distinct()
    
    if (n.Years < 4) {
      to_breaks <- "3 months"
      to_labels <- "%Y-%b"
    } else {
      to_breaks <- "1 year"
      to_labels <- "%Y"
    }
    
    p <- ggplot(df_lambda, aes(x = Date, y = Value, group = Country, colour = Country)) + 
      geom_line() +
      scale_x_date(date_breaks = to_breaks, date_labels = to_labels) +
      scale_y_continuous(labels = scales::unit_format(unit = "K",
                                                      scale = 1e-3,
                                                      sep = "",
                                                      accuracy = 1)) +
      labs(title = paste("Figure 1. Time Trend of", input$mainevent)) 
    #+ theme(axis.text.x = element_text(angle = 60, hjust = 1))
    ggplotly(p, tooltip = c("group", "Date", "Value")) %>% layout(legend = list(orientation = "h", x = 0.1, y = 1.05))
  })
  
  
  # Create the second plot
  output$auxplot <- renderPlotly({
    req(input$country)
    df_lambda <- selected_data()
    
    if (length(input$country) == 1) {
      
      p <- ggplot(df_lambda, aes(x = Year, y = Month, fill = Category)) + 
        geom_tile(color = "white") +
        geom_text(aes(label = Value), size = 2) + 
        scale_fill_manual(values = c("seagreen4", "orangered3", "lightgoldenrod2")) +
        labs(title = paste("Figure 2. Heatmap of", input$mainevent))
      ggplotly(p, tooltip = c("Year", "Month", "fill")) #%>% layout(legend = list(orientation = "h", x = 0.3, y = 1.05))
      
    } else{
      
      # Calculating the mininmum common data point
      to_start <- df_lambda %>% group_by(Country) %>% summarise(Start = min(Date, na.rm=TRUE)) %>% pull(Start) %>% max
      df_lambda %<>% filter(Date > to_start)
      
      n.Years <- format(df_lambda$Date,"%Y") %>% n_distinct()
      
      if (n.Years < 4) {
        to_breaks <- "3 months"
        to_labels <- "%Y-%b"
      } else {
        to_breaks <- "1 year"
        to_labels <- "%Y"
      }
      
      
      if (input$contrasts == TRUE) {
        fm <- lm(Value ~ Date*Country, data = df_lambda)
        df_lambda %<>% cbind(pred = predict(fm))
        df_lambda %<>% mutate(pred = ifelse(pred > 0, pred, NA))
        
        
        p <- ggplot(data = df_lambda, aes(x = Date, y = Value, color = Country)) +
          geom_point() + geom_line(aes(y = pred)) + 
          scale_x_date(date_breaks = to_breaks, date_labels = to_labels) +
          scale_y_continuous(labels = scales::unit_format(unit = "K",
                                                          scale = 1e-3,
                                                          sep = "",
                                                          accuracy = 1)) +
          labs(title = "Figure 2. Regression") + 
          theme(axis.text.x = element_text(angle = 60, hjust = 1))
        
      } else {
        fm <- lm(Value ~ Date, data = df_lambda)
        df_lambda %<>% cbind(pred = predict(fm))
        df_lambda %<>% mutate(pred = ifelse(pred > 0, pred, NA))
        
        p <- ggplot(data = df_lambda, aes(x = Date, y = Value, color = Country)) +
          geom_point() + geom_line(aes(y = pred), color = "black") +
          scale_x_date(date_breaks = to_breaks, date_labels = to_labels) +
          scale_y_continuous(labels = scales::unit_format(unit = "K",
                                                          scale = 1e-3,
                                                          sep = "",
                                                          accuracy = 1)) +
          labs(title = "Figure 2. Regression") + 
          theme(axis.text.x = element_text(angle = 60, hjust = 1))
      }
      
      ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.1, y = 1.05))
      
    }
    
    
  })
  
  output$eventslineplot <- renderPlotly({
    df_lambda <- selected_data2()
    check <- df_lambda %>% group_by(Country) %>% summarise(N = n_distinct(Event)) %>% pull(N) %>% sum
    req(input$auxevent)
    validate(need(length(input$country) == 1, "Warning: You can only compare events within one country."))
    validate(need(check > 1, paste("Warning: There is only one indicator available for", input$country) ))

    # Calculate values as percentage from maximum value
    to_start <- df_lambda %>% group_by(Event) %>% summarise(Start = min(Date, na.rm=TRUE)) %>% pull(Start) %>% max
      
    df_lambda %<>% filter(Date > to_start) %>% group_by(Event) %>% 
      mutate(Value = round(Value/max(Value, na.rm=TRUE) * 100,2) )
      
      
      n.Years <- format(df_lambda$Date,"%Y") %>% n_distinct()
      
      if (n.Years < 5) {
        to_breaks <- "3 months"
        to_labels <- "%Y-%b"
      } else {
        to_breaks <- "1 year"
        to_labels <- "%Y"
      }
      
      p <- ggplot(df_lambda, aes(x = Date, y = Value, colour = Event)) +
        geom_point() +
        geom_line(linetype = "dashed") +
        labs(title = "Figure 3. Time Trend of Events", y = "People Affected (% from max)")
      ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.1, y = 1.05))
  })
  
  
  output$eventsplot <- renderPlotly({
    #req(input$auxevent, input$country)
    df_lambda <- selected_data2()
    check <- df_lambda %>% group_by(Country) %>% summarise(N = n_distinct(Event)) %>% pull(N) %>% sum
    req(input$auxevent)
    validate(need(length(input$country) == 1, ""))
    validate(need(check > 1, ""))
    
    df_lambda %<>% reshape2::dcast(Country + Year + Month ~ Event, value.var="Value") %>% drop_na()
      
    p <- ggplot(df_lambda, aes_string(x = input$mainevent, y = input$auxevent, text = "Year")) + 
      geom_point(size = 4, shape = 1) +
      scale_x_continuous(labels = scales::unit_format(unit = "K",
                                                      scale = 1e-3,
                                                      sep = "",
                                                      accuracy = 1)) +
      scale_y_continuous(labels = scales::unit_format(unit = "K",
                                                      scale = 1e-3,
                                                      sep = "",
                                                      accuracy = 1)) +
      
        labs(title = "Figure 4. Scatterplot of Two Events")#+
      # theme(axis.text.x = element_text(angle = 60, hjust = 1))
    ggplotly(p)
    })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)