library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(readr)
library(zoo)
library(scales)
library(lubridate)

abbrev<-data.frame(Full = state.name, Abb = state.abb)
covid_csv<-'https://covidtracking.com/api/v1/states/daily.csv'
state_csv<-'https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv'

download.file(covid_csv, destfile = ".data.csv")
data<-read.csv(".data.csv") %>%
  merge(abbrev, by.x = "state", by.y = "Abb") %>%
  mutate(date = as.Date(paste(substr(date,0,4),"-",substr(date,5,6),"-",substr(date,7,8), sep="")))

download.file(state_csv,destfile = ".pop.csv")
pop<-read.csv(".pop.csv") %>% select(NAME,POPESTIMATE2019)
colnames(pop)<-c("State","Population")


ui <- dashboardPage(skin = "black",
  dashboardHeader(title=span(tagList(icon("viruses"), "COVID-19"))),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
    tabItems(
      #map
      tabItem(
        "tab_map",
        column(9, align = "center",
               fluidRow(
                 box(title = "Map of Quarantine Statuses",
                     plotlyOutput("map", height = 700), width=NULL, height=805)
               )
        ),
        column(3, align = "center",
               box(title = "Travel Advisory States",
                 fluidRow(
                   tableOutput("quarantine")
                 ), width = NULL, height=805
               )
        )
      ),
      # trends
      tabItem(
        "tab_trends",
        column(12,
               fluidRow(
                 column(6,
                        fluidRow(
                          column(6,
                                 fluidRow(
                                   box(valueBoxOutput("hospital", width = NULL), width = 12, height = 125)
                                 ),
                                 fluidRow(
                                   box(valueBoxOutput("positive", width = NULL),width = 12, height = 125)
                                 )
                          ),
                          column(6,
                                 fluidRow(
                                   box(valueBoxOutput("icu", width = NULL), width = 12, height = 125)
                                 ),
                                 fluidRow(
                                   box(valueBoxOutput("negative", width = NULL), width = 12, height = 125)
                                 )
                          )
                        ),
                        fluidRow(
                          box(valueBoxOutput("days", width = NULL), width = 12, height = 125)
                        )
                 ),
                 column(6, align = "center",
                        box(title = "Cumulative Positive Tests, Hospitalizations, and Recovered",
                            plotlyOutput("cumulative", height = 350), width = NULL, height = 415)
                 )
               ),
               fluidRow(
                 column(12, align = "center",
                        box(title = "Daily Positive and Negative Tests",
                            plotlyOutput("trends", height = 300),width=NULL, height = 360)
                 )
               )
        )
      ),
      #quarantine
      tabItem(
        "tab_quarantine",
        
        column(12,
               fluidRow(
                 column(9, align = "center",
                        box(title = paste("Percent of Positive Tests"),
                            plotlyOutput("percent", height = 320),width=NULL, height = 377)
                 ),
                 column(3,
                        fluidRow(
                          box(infoBoxOutput("percentthresh", width=NULL),width = NULL, height = 112)
                        ),
                        fluidRow(
                          box(infoBoxOutput("percentcurrent", width=NULL),width = NULL, height = 112)
                        ),
                        fluidRow(
                          box(infoBoxOutput("percentstatus", width=NULL),width = NULL, height = 112)
                        )
                 )
                 
               ),
               fluidRow(
                 column(9, align = "center",
                        box(title = "10 per 100,000 Residents Positive Tests",
                            plotlyOutput("rate", height = 320),width=NULL, height=377)
                 ),
                 column(3,
                        fluidRow(
                          box(infoBoxOutput("ratethresh", width=NULL),width = NULL, height = 112)
                        ),
                        fluidRow(
                          box(infoBoxOutput("ratecurrent", width = NULL),width = NULL, height = 112)
                        ),
                        fluidRow(
                          box(infoBoxOutput("ratestatus",width=NULL),width = NULL, height = 112)
                        )
                        
                 )
                 
               )
        )
      )
    )
    
    
  )
)

server <- function(input, output) {
  
  time <- reactive({
    invalidateLater(1000*60*60*60)
    format(round_date(Sys.time(),unit="1 hour"), "%H:%M:%S")
  })

  selectedState<-reactive({input$states})
  selectedMinDate<-reactive({input$dates[1]})
  selectedMaxDate<-reactive({input$dates[2]})

  output$menu <- renderMenu({
    temp<-state_data()
    update<-paste("Updated as of",temp$date[nrow(temp)])
    sidebarMenu(id = "sidebarmenu",
                menuItem("Map", tabName = "tab_map", icon = icon("globe-americas")),
                menuItem("Trends", tabName = "tab_trends", icon = icon("chart-line")),
                menuItem("Quarantine", tabName = "tab_quarantine",icon = icon("house-user")),
                
                selectInput("states","Select a State", unique(data$Full)),
                uiOutput("dates"),
                h5(helpText("Data is shown as reported"), align = "center"),
                h5(helpText(update),align="center")
    )
  })
  
  state_data <- reactive({
    

    if(time() == "19:00:00"){
      download.file(covid_csv, destfile = ".data.csv")
      data<-read.csv(".data.csv") %>%
        merge(abbrev, by.x = "state", by.y = "Abb") %>%
        mutate(date = as.Date(paste(substr(date,0,4),"-",substr(date,5,6),"-",substr(date,7,8), sep="")))
    }
    
    data %>%
      merge(pop, by.x = "Full", by.y = "State") %>% 
      group_by(state) %>%
      arrange(date) %>%
      mutate(positiveIncrease = abs(positiveIncrease),
             negativeIncrease = abs(negativeIncrease),
             totalTestResultsIncrease = positiveIncrease + negativeIncrease,
             death = abs(death),
             PercPos = ifelse(totalTestResultsIncrease == 0, 0, positiveIncrease/totalTestResultsIncrease * 100),
             PercAvg = rollmean(x = PercPos,7, align = "right", fill = NA),
             RateTen = ceiling(Population*10/100000),
             RateEight = ceiling(Population*8/100000),
             RateAvg = ceiling(rollmean(x = positiveIncrease,7,align="right", fill=NA)),
             RateStatus = ifelse(RateTen > RateAvg & RateEight < RateAvg,"#ff9e00",
                                 ifelse(RateTen > RateAvg,"#00a756","#ee2d04")),
             PercStatus = ifelse(PercAvg > 9 & PercAvg < 10,"#ff9e00",
                                 ifelse(PercAvg < 10,"#00a756","#ee2d04")),
             Status = ifelse(PercStatus == "#00a756" & RateStatus == "#00a756","#00a756",
                             ifelse(PercStatus == "#ee2d04" | RateStatus == "#ee2d04", "#ee2d04","#00a756")))
  })
  
  output$dates<-renderUI({
    temp<-state_data() %>%
      filter(Full==selectedState())
    
    dateRangeInput("dates","Select Date Range", format = "yyyy-mm-dd", startview = "month",
                   min = min(temp$date), max = max(temp$date), start = min(temp$date), end = max(temp$date))
  })
  
  output$trends<-renderPlotly({
    temp<-state_data() %>%
      filter(Full==selectedState())
    
    temp %>%
      plot_ly(x = ~date,
              y = ~positiveIncrease,
              type = "bar",
              name = "Positive Tests",
              marker = list(color = "#ee2d04")) %>% 
      add_trace(y = ~negativeIncrease,
                name = "Negative Tests",
                marker = list(color = "#ff9e00")) %>%
      layout(xaxis = list(title = "Date",
                          range = c(selectedMinDate(),selectedMaxDate())),
             yaxis = list(title = "Number of Tests"),
             barmode = "stack",
             hovermode = "x unified") %>%
      config(displayModeBar = FALSE)
  })
  
  output$cumulative<-renderPlotly({
    temp<-state_data() %>%
      filter(Full==selectedState())
    
    temp %>%
      plot_ly(x=~date,
              y=~positive,
              type = "scatter",
              mode = "line",
              hovertemplate = '%{y}<extra>Positive</extra>',
              line = list(color = "#ee2d04"),
              name = "Positive") %>%
      add_trace(y=~hospitalized,
                hovertemplate = '%{y}<extra>Hospitalized</extra>',
                line = list(color = "#ff9e00"),
                name = "Hospitalized") %>%
      add_trace(y=~recovered,
                hovertemplate = '%{y}<extra>Recovered</extra>',
                line = list(color = "#00a756"),
                name = "Recovered") %>%
      add_trace(y=~death,
                hovertemplate = '%{y}<extra>Deaths</extra>',
                line = list(color = "#0273cc"),
                name = "Deaths") %>%
      layout(xaxis = list(title = "Date",
                          range = c(selectedMinDate(),selectedMaxDate())),
             yaxis = list(title = "Number of Cases"),
             hovermode = "x unified") %>%
      config(displayModeBar = FALSE)
  })
  
  output$percent<-renderPlotly({
    temp<-state_data() %>%
      filter(Full==selectedState(), !is.na(PercAvg))
    
    temp %>%
      plot_ly(x=~date,
                y=~10,
                type = "scatter",
                mode = "line",
                hovertemplate = '%{y:.2f}%<extra>Threshold</extra>') %>%
      add_trace(x=~date,
                y=~PercAvg,
                type="bar",
                name = "7-Day Average",
                marker = list(color = ~PercStatus),
                hovertemplate = '%{y:.2f}%<extra>7-Day Average</extra>') %>%
      layout(yaxis = list(title = "7-Day Average"),
             xaxis = list(title = "Date",
                          range = c(selectedMinDate(),selectedMaxDate())),
             showlegend = FALSE,
             hovermode = "x unified") %>%
      config(displayModeBar = FALSE)
  })
  
  output$rate<-renderPlotly({
    temp<-state_data() %>%
      filter(Full==selectedState(), !is.na(RateAvg))
    
    temp %>%
      plot_ly(x=~date, 
                y=~RateTen, 
                type="scatter",
                mode="line",
                name = "Threshold") %>%
      add_trace(x = ~date,
            y = ~RateAvg,
            type = "bar",
            name = "7-Day Average",
            marker = list(color = ~RateStatus)) %>% 
      layout(xaxis = list(title = "Date",
                          range = c(selectedMinDate(),selectedMaxDate())),
             yaxis = list(title = "7-Day Average"),
             showlegend = FALSE,
             hovermode = "x unified") %>%
      config(displayModeBar = FALSE)
  })
  
  output$ratethresh<-renderInfoBox({
    temp<-state_data() %>%
      filter(Full==selectedState())
    infoBox(
      "Needs to be under", 
      temp$RateTen[nrow(temp)], 
      icon = icon("bars"),
      color = "blue"
    )
  })
  
  output$percentthresh<-renderInfoBox({
    temp<-state_data() %>%
      filter(Full==selectedState())
    infoBox(
      "Needs to be under", 
      paste(10,"%",sep=""), 
      icon = icon("bars"),
      color = "blue"
    )
  })
  
  output$ratecurrent<-renderInfoBox({
    temp<-state_data() %>%
      filter(Full==selectedState())
    infoBox(
      "7-day Average", 
      HTML(paste(temp$RateAvg[nrow(temp)],br(),
                 ifelse(temp$RateAvg[nrow(temp)] - temp$RateAvg[nrow(temp)-1] > 0, "Increase of", "Decrease of"), abs(temp$RateAvg[nrow(temp)] - temp$RateAvg[nrow(temp)-1]))),
      icon = icon(ifelse(temp$RateAvg[nrow(temp)] > temp$RateAvg[nrow(temp)-1], "angle-double-up", "angle-double-down")),
      color = ifelse(temp$RateAvg[nrow(temp)] > temp$RateAvg[nrow(temp)-1], "red","green")
    )
  })
  
  output$percentcurrent<-renderInfoBox({
    temp<-state_data() %>%
      filter(Full==selectedState())
    infoBox(
      "7-day Average", 
      HTML(paste(round(temp$PercAvg[nrow(temp)],2),"%",br(),
                 ifelse(temp$PercAvg[nrow(temp)] - temp$PercAvg[nrow(temp)-1] > 0, "Increase of ", "Decrease of "), round(abs(temp$PercAvg[nrow(temp)] - temp$PercAvg[nrow(temp)-1]),2), "%",sep="")),
      icon = icon(ifelse(temp$PercAvg[nrow(temp)] > temp$PercAvg[nrow(temp)-1], "angle-double-up","angle-double-down")),
      color = ifelse(temp$PercAvg[nrow(temp)] > temp$PercAvg[nrow(temp)-1], "red", "green")
    )
  })
  
  output$ratestatus<-renderInfoBox({
    temp<-state_data() %>%
      filter(Full==selectedState())
    infoBox(
      "Status",
      ifelse(temp$RateTen[nrow(temp)] > temp$RateAvg[nrow(temp)],"Meets requirement","Does not meet requirement"),
      icon = icon(ifelse(temp$RateTen[nrow(temp)] > temp$RateAvg[nrow(temp)] & temp$RateEight[nrow(temp)] < temp$RateAvg[nrow(temp)],"exclamation-triangle",
                         ifelse(temp$RateTen[nrow(temp)] > temp$RateAvg[nrow(temp)],"thumbs-up","thumbs-down"))),
      color = ifelse(temp$RateTen[nrow(temp)] > temp$RateAvg[nrow(temp)] & temp$RateEight[nrow(temp)] < temp$RateAvg[nrow(temp)],"yellow",
                     ifelse(temp$RateTen[nrow(temp)] > temp$RateAvg[nrow(temp)],"green","red"))
    )
  })
  
  output$percentstatus<-renderInfoBox({
    temp<-state_data() %>%
      filter(Full==selectedState())
    infoBox(
      "Status",
      ifelse(temp$PercAvg[nrow(temp)] < 10,"Meets requirement","Does not meet requirement"),
      icon = icon(ifelse(temp$PercAvg[nrow(temp)] >9 & temp$PercAvg[nrow(temp)] < 10,"exclamation-triangle",
                         ifelse(temp$PercAvg[nrow(temp)] < 10,"thumbs-up","thumbs-down"))),
      color = ifelse(temp$PercAvg[nrow(temp)] > 9 & temp$PercAvg[nrow(temp)] < 10,"yellow",
                     ifelse(temp$PercAvg[nrow(temp)] < 10,"green","red"))
    )
  })
  
  output$map<-renderPlotly({
    temp<-state_data() %>%
      ungroup() %>%
      slice(n())
    currentdate<-temp$date
    
    temp<-state_data() %>%
      filter(date == currentdate) %>%
      arrange(Full) %>%
      mutate(PercStatus = ifelse(PercAvg > 9 & PercAvg < 10,"yellow",
                                 ifelse(PercAvg < 10,"green","red")),
             RateStatus = ifelse(RateTen > RateAvg & RateEight < RateAvg,"yellow",
                                 ifelse(RateTen > RateAvg,"green","red")),
             Color = ifelse(PercStatus == "green" & RateStatus == "green",1,
                            ifelse(PercStatus == "red" | RateStatus == "red",3,2)))
    
    colors<-data.frame(States = state.name, Colors = temp$Color) %>% 
      mutate(Hex=case_when(Colors==1~"#00a756",
                           Colors==2~"#ff9e00",
                           Colors==3~"#ee2d04"))
    
    g <- list(
      scope = 'usa'
    )
    
    plot_ly(z = colors$Colors, 
            locations = state.abb,
            type = 'choropleth', 
            locationmode = 'USA-states', 
            colors = colors$Hex,
            hovertemplate = colors$States, 
            name = " ") %>%
      layout(geo = g) %>% 
      hide_colorbar() %>%
      config(displayModeBar = FALSE)
  })
  
  output$quarantine<-renderTable({
    temp<-state_data() %>%
      ungroup() %>%
      slice(n())
    currentdate<-temp$date
    
    temp<-state_data() %>%
      filter(date == currentdate) %>%
      arrange(Full) %>%
      mutate(PercStatus = ifelse(PercAvg > 9 & PercAvg < 10,"yellow",
                                 ifelse(PercAvg < 10,"green","red")),
             RateStatus = ifelse(RateTen > RateAvg & RateEight < RateAvg,"yellow",
                                 ifelse(RateTen > RateAvg,"green","red")),
             Color = ifelse(PercStatus == "green" & RateStatus == "green",0,
                            ifelse(PercStatus == "red" | RateStatus == "red", 2,1))) %>%
      ungroup() %>%
      filter(Color == 2)
    
    numb<-ceiling(length(temp$Full)/2)
    col1<-temp$Full[1:numb]
    col2<-if(length(temp$Full) %% 2 == 0){
      temp$Full[(numb+1):length(temp$Full)]
    } else {
      c(temp$Full[(numb+1):length(temp$Full)],"")}
    
    data.frame(col1 = col1, col2=col2)
    
  }, colnames = FALSE)
  
  output$hospital<-renderValueBox({
    temp<-state_data() %>%
      filter(Full==selectedState())
    
    valueBox(temp$hospitalizedCurrently[nrow(temp)], "Current Hospitalizations", icon = icon("hospital"), color = "yellow")
  })
  
  output$icu<-renderValueBox({
    temp<-state_data() %>%
      filter(Full==selectedState())
    
    valueBox(temp$inIcuCurrently[nrow(temp)], "Current ICU Patients", icon = icon("procedures"), color = "blue")
  })
  
  output$positive<-renderValueBox({
    temp<-state_data() %>%
      filter(Full==selectedState())
    
    valueBox(temp$positiveIncrease[nrow(temp)], "Positive Tests Today", icon = icon("user-plus"), color = "red")
    
  })
  
  output$negative<-renderValueBox({
    temp<-state_data() %>%
      filter(Full==selectedState())
    
    valueBox(temp$negativeIncrease[nrow(temp)], "Negative Tests Today", icon = icon("user-minus"), color = "green")
    
  })
  
  output$days<-renderValueBox({
    temp<-state_data() %>%
      filter(Full==selectedState())
    
    valueBox(paste(as.numeric(temp$date[nrow(temp)]-temp$date[1]), "days"), "Since First COVID Case", icon = icon("calendar-day"), color = "blue")
  })
  

}

shinyApp(ui, server)