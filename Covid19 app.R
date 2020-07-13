
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(gapminder)
library(shinyWidgets)

cv= read.csv("covid.csv")

Country = cv$location
Deaths = cv$cvd_death_rate
stringency = cv$stringency_index

 









ui <- shinyUI(
  dashboardPage( skin = "red",
    dashboardHeader(title = "Covid-19 ShinyApp"),
    dashboardSidebar(
      sidebarMenu( width = 150,
        menuItem("Dashboard",tabName = "dashboard"),
        menuItem("Visualizations", tabName = "covidcases"),#remove empty space
        #menuItem("item2", tabName = "item2"), #give tab name
        menuItem("Covid-19 Death Rate",tabName = "vsl"), #give tab name
        menuItem("Our Data",tabName = "rawdata"), #give tab name
       # menuItem("About",tabName = "abt"), #give tab name
        
        setBackgroundColor("ghostwhite")
      )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                  valueBoxOutput("value1")
                  ,valueBoxOutput("value2")
                  ,valueBoxOutput("value3"),
                  
                  box(title = "Number of Covid-19 Tests VS New Cases on the basis of Stringency rate followed by Countries", color = "red",width = 10, status = "danger", height = "776",solidHeader = T,
                      gapminder %>%
                        plot_ly(
                          x = ~cv$total_cases, 
                          y = ~cv$total_tests, 
                          size = ~cv$population, 
                          color = ~stringency, 
                          frame = "", 
                          text = ~cv$iso_code, 
                          hoverinfo = "text",
                          type = 'scatter',
                          mode = 'markers'
                        ) %>%
                        layout( 
                          xaxis = list(
                            type = "log",
                            title = "New Cases"
                          ),
                          yaxis = list(
                            title = "Total Tests"
                          )
                        )
                  )  
                )),
        
        tabItem(tabName = "About", #remove empty space
                h2("Death Rate by Coronavirus in Different Count")
                
        ),
        
        
      
        
        tabItem(tabName = "covidcases", #remove empty space
                #h1("Numbers by Team"),
                fluidRow(
                  box(title = "Test statuses in Different Countries", color = "red",width = 6, status = "danger", height = "400",solidHeader = T,
                    #title = "Total Cases"
                    #status = "primary"
                    #solidHeader = TRUE 
                    collapsible = TRUE ,
                    plotOutput("revenuebyPrd", height = "350px")
                  ),
                  box(title = "Death Rate of People above 65", color = "red",width = 6, status = "danger", height = "400",solidHeader = T,
                      #title = "Total Cases"
                      #status = "danger"
                      #solidHeader = TRUE 
                      collapsible = TRUE ,
                      plotOutput("AgeFactor", height = "350px")
                  )
                  ,box(
                    title = "Availibility of Hospital Beds for effected patients",
                    color = "red",
                    width = 6,
                    height = "400"
                    ,status = "danger"
                    ,solidHeader = TRUE 
                    ,collapsible = TRUE 
                    ,plotOutput("revenuebyRegion", height = "350px")
                  ),
                  
                  box(
                    title = "Countries following Frequent Hand sanitization ",
                    color = "red",
                    width = 6,
                    height = "400"
                    ,status = "danger"
                    ,solidHeader = TRUE 
                    ,collapsible = TRUE 
                    ,plotOutput("handwsh", height = "350px")
                  )
        )
        ),
        
        
      
        
        tabItem(tabName = "vsl", #remove empty space
                h2("Death Rate by Covid-19 in Different Countries"),
               
                  
                  # Sidebar panel for inputs ----
                fluidRow( 
                selectInput(inputId="options",label = "select any country",choices = c("Pakistan" ,"Bangladesh", "Ethiopia",
                                                                                        
                                                                                        "Colombia","Costa Rica","Ghana","Indonesia",
                                                                                      "India","Kazakhstan","Kenya","Mexico","Myanmar",
                                                                                      "Nepal","Paraguay","Tunisia","Uganda",
                                                                                      "Vietnam","South Africa" ),
                            selected = "", multiple = F),
                
                
                plotOutput(outputId = "distPlot2")
                
                  )
        
        
        
                  
      
        ),
        
        
        
        tabItem(tabName = "rawdata",
                h2("Upload File after Data cleaning "),
                h3("Data Source: https://ourworldindata.org/coronavirus-source-data "),
                
               
                 sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    # Input: Select a file ----
                    fileInput("file1", "Choose CSV File",
                              multiple = TRUE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Input: Checkbox if file has header ----
                    checkboxInput("header", "Header", TRUE),
                    
                    # Input: Select separator ----
                    radioButtons("sep", "Separator",
                                 choices = c(Comma = ",",
                                             Semicolon = ";",
                                             Tab = "\t"),
                                 selected = ","),
                    
                    # Input: Select quotes ----
                    #radioButtons("quote", "Quote",
                     #            choices = c(None = "",
                      #                       "Double Quote" = '"',
                       #                      "Single Quote" = "'"),
                        #         selected = '"'),
                    
                    # Horizontal line ----
                    #tags$hr(),
                    
                    #Input: Select number of rows to display ----
                    radioButtons("disp", "Display",
                                 choices = c(Head = "head",
                                             All = "all"),
                                 selected = "head")
                    
                  ),
                  
                  
                 
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    # Output: Data file ----
                    tableOutput("contents"),
                   
                    
                
        )
      ))
      
      
      
      
      
      ))

    
   
    
  )
  
  
  )
    

    







#create the server functions for the dashboard  
server <- function(input, output) { 
  
  
  
  output$distPlot2 <- renderPlot({
    Pak <- subset(cv,cv$location=="Pakistan")
    if(input$options == "Pakistan"){
      
      ggplot(Pak, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        
        ggtitle("Death Rate in Pakistan") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    
    }else if(input$options == "Bangladesh"){
      
      ban <- subset(cv, cv$location =="Bangladesh")
      ggplot(ban, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Bangladesh") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    }else if(input$options == "Colombia"){
      col <- subset(cv, cv$location =="Colombia")
      
      ggplot(col, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Colombia") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    }else if(input$options == "Costa Rica"){
      Costa <- subset(cv, cv$location =="Costa Rica")
      
      ggplot(Costa, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Costa Rica") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    
    }else if(input$options == "Ghana"){
      Gha <- subset(cv, cv$location =="Ghana")
      
      ggplot(Gha, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Ghana") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    }else if(input$options == "Indonesia"){
      Indo <- subset(cv, cv$location =="Indonesia")
      ggplot(Indo, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Indonesia") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    }else if(input$options == "India"){
      Ind <- subset(cv, cv$location =="India")
      
      ggplot(Ind, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in India") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    }else if(input$options == "Kazakhstan"){
      Kazak <- subset(cv, cv$location =="Kazakhstan")
      ggplot(Kazak, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Kazakhstan") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    }else if(input$options == "Kenya"){
      Ken <- subset(cv, cv$location =="Kenya")
      ggplot(Ken, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Kenya") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    }else if(input$options == "Mexico"){
      Mex <- subset(cv, cv$location =="Mexico")
      
      ggplot(Mex, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Mexico") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    }else if (input$options == "Myanmar"){
      Mya <- subset(cv, cv$location =="Myanmar")
      
      ggplot(Mya, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Myanmar") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    }else if(input$options == "Nepal"){
      Nep <- subset(cv, cv$location =="Nepal")
      
      ggplot(Nep, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Nepal") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    }else if(input$options == "Paraguay"){
      Par <- subset(cv, cv$location =="Paraguay")
      
      ggplot(Par, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Paraguay") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    } else if(input$options == "Ethiopia"){
      Eth <- subset(cv,cv$location=="Ethiopia")
      ggplot(Eth, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Ethiopia ") +
        ylab("Total Cases")+
        xlab("Death Rate")
    }else if(input$options == "Tunisia"){
      Tun <- subset(cv, cv$location =="Tunisia")
      
      ggplot(Tun, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Tunisia") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    }else if(input$options == "Uganda"){
      Uga <- subset(cv, cv$location =="Uganda")
      
      ggplot(Uga, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Uganda") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    }else if(input$options == "Vietnam"){
      Vie <- subset(cv, cv$location =="Vietnam")
     
      ggplot(Vie, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in Vietnam") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")}
    
      else if (input$options == "South Africa"){
     # tst = ((cv$new_tests/cv$total_tests)*100)
      #dt = ((cv$cvd_death_rate/cv$total_deaths)*100)
      
      SA <- subset(cv, cv$location =="South Africa")
      ggplot(SA, aes(x=new_deaths, y=total_cases, group=location, color=location)) +
        geom_point(size = 1.5) +
        ggtitle("Death Rate in South Africa") +
        theme_bw() +
        ylab("Total Cases")+
        xlab("Death Rate")
    }
   
  
  
  })
    

  output$contents <- renderTable({
    
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
      
       }
    
  })
  
  
  #some data manipulation to derive the values of KPI boxes
  total_cases <- sum(cv$total_cases)
  total_deaths <- sum(cv$total_deaths)
  total_tests <- sum(cv$total_tests)
  #total_deaths <- cv %>% group_by(total_cases) %>% summarise(value = sum(new_deaths)) %>% filter(value==max(value))
  #new_tests <- cv %>% group_by(total_cases) %>% summarise(value = sum(Recoveries)) %>% filter(value==max(value))
  
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total_cases, format="d", big.mark=',')
      ,paste('Total cases')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "black")
    
    
  })
  
  
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(total_deaths, format="d", big.mark=',')
      ,'Total Deaths'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")
    
  })
  
  
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(total_tests, format="d", big.mark=',')
      ,paste('Total Tests')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "black")
    
  })
  
  #Table 1
  
  output$revenuebyPrd <- renderPlot({
    ggplot(data = cv, 
           aes(x=tests_units, y=total_cases, fill=factor(location))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Test Statuses") + 
      xlab("Total cases") + theme(legend.position="bottom" 
                                  ,plot.title = element_text(size=15, face="bold")) + 
        labs(fill = "location")
  })
  
  #Table2
  
  output$revenuebyRegion <- renderPlot({
    
    #ts = ((cv$new_tests/cv$total_tests)*100)
    #bds = ((cv$hospital_beds_per_100k/cv$population)*100)
    #pp = ((cv$total_cases/population)*100)
    ggplot(data = cv, 
           aes(x=hospital_beds_per_100k,y=location, fill=factor(location))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab(" cases ") + 
      xlab("Availibility of Beds in Hospital ") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
       labs(fill = "Region")
  })
  #Table3
  
  dt =  ((cv$aged_65_older/cv$median_age)*100)
  #cvd= ((cv$new_deaths/cv$total_deaths)*100)
  output$AgeFactor <- renderPlot({
    ggplot(data = cv, 
           aes(x=total_deaths, y=dt, fill=factor(location))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Total deaths") + 
      xlab("Percentage of People above 65 years ") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      labs(fill = "Region")
  })
  
  #Table4
  
 
  #cvd= ((cv$new_deaths/cv$total_deaths)*100)
  output$handwsh <- renderPlot({
    ggplot(data = cv, 
           aes(x=handwashing_facilities, y=location, fill=factor(location))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Countries") + 
      xlab("Percentage of Hand washing Facility") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      labs(fill = "Region")
  })
  
  
}


shinyApp(ui, server)
