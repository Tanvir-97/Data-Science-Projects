#Please the install part if you do not have the libraries given below
# install codes are given below 
# API style for gganimate is of latest version please update if you have older one.


#install.packages('shiny')
#install.packages('dplyr')
#install.packages('shinydashboard')
#install.packages('plotly')
#install.packages('readxl')
#install.packages('threejs')
#install.packages('maptools')
#install.packages('maps')
#install.packages('ggplot2')
#install.packages('ggthemes')
#install.packages('gganimate')
#install.packages('mapdata')
#install.packages('lubridate')


library(shiny)
library(dplyr)
library(plotly)
library(readxl)
library(shinydashboard)
library(threejs)
library(maptools)
library(maps)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(gganimate)
library(mapdata)

#-----------------------read data----------------------------------------

covid_data<-data.frame(read_excel("total_table.xlsx"))
for_map_color<-data.frame(read_excel("final.xlsx"))
for_globe<-data.frame(read.csv("latest.csv"))
temp_data<-data.frame(read.csv("for_map.csv"))

for_map_color$Country.Region[for_map_color$Country.Region == "US"] <- "USA"

#-------------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Covid-19 Situation Analysis"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Dashboard 1", tabName = "dashboard1", icon = icon("dashboard")),
      menuItem("Dashboard 2", icon = icon("dashboard"), tabName = "dashboard2"),
      menuItem("Data Source", icon = icon("table"), tabName = "source_data"
               )
    ),
    # Custom CSS to hide the default logout panel
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    
    # The dynamically-generated user panel
    uiOutput("userpanel")
  ),
  dashboardBody(
    tags$style(".nav-tabs {background-color: #85C1E9;}

      .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
      background-color: transparent;
      border-color: transparent;
      }
      .nav-tabs-custom .nav-tabs li.active {
          border-top-color: #FFF;
      }"),
    tags$head(tags$style(HTML('/* body */.content-wrapper, .right-side {background-color: #000;}'))),
    

    tabItems(
      # first dashboard page
      tabItem(tabName = "dashboard1",
              h1("Graphical representation of Covid-19 cases", color ="white"),
              tabBox(
                title = "CASES TREND", height = "600px",width= "400px",
                # building multiple pannels for dahboard1
                tabPanel("CASE TYPE BREAKDOWN",h4("Breakdown of total cases into types of cases over the time."),
                         plotlyOutput("case_distribution")),
                tabPanel("INDIVIDUAL CASE TYPE",h4("Growth of cases over time in 2020"),h5("Explore and analyse using multiple combinations from Input"),
                         fluidPage(
                           mainPanel(
                             div(style="display:flex",
                                 column(8,
                                        selectInput("var1",selected="select",
                                                    label = "Choose variable",                      
                                                    choices = c("Month","Date" ))
                                 ),
                                 column(8,
                                        selectInput("var2",selected="select",
                                                    label = "Choose variable",                      
                                                    choices = c( "Active",
                                                                 "Recovered",
                                                                 "Deaths",
                                                                 "Confirmed",
                                                                 "Confirmed_Monthly",
                                                                 "Active_Monthly",
                                                                 "Recovered_Monthly",
                                                                 "Deaths_Monthly"))
                                 )
                             ))),
                         plotlyOutput("individual_cases"))
              ),
              
              fluidRow(
                # infoBox for instructions
                infoBox("INSTRUCTIONS FOR BAR PLOT ",
                        h5("-Click initially "),
                        h5("-Hover over for values "),
                        ),
                infoBox("INSTRUCTIONS FOR SCATTER PLOT ", 
                        h5("-Hover over for values"),
                        h5("-Hover/Select options")
                       )
              )
              
              
              
              
      ),
      # dashboard 2 page 
      tabItem(tabName = "dashboard2",
              h1("Spatial representation of Covid-19 cases", color="white"),
              tabBox(
                title = "CASES ACROSS GLOBE", height = "800px",width= "700px",
                # building pannls for page 2
                tabPanel("LATEST COUNT",
                         h3("TRY: ZOOM-IN, ZOOM-OUT, CLICK-AND-ROTATE"),
                         h5("Spike's length represents the confirmed cases count checked last."),
                         globeOutput("globeplot")),
                tabPanel("WORLD TEMPERATURE",
                         fluidRow(
                           column(12,h4("Average temperature of the countries across the world"), 
                                 # output plot along with hover 
                                   div(
                                    style = "position:relative",
                                    plotOutput("world_color", 
                                               hover = hoverOpts("plot_hover", delay = 0, delayType = "debounce")),
                                    uiOutput("hover_info")
                                  ))
                                  ,
                           
                           column(12,h4("Countries who missed in taking early precautions "),plotOutput("late"))
                           
                         )
                         ),
                tabPanel("VIRUS GROWTH",h4("CASE GROWTH ALL OVER WORLD WITH TIME"),
                        h4("MY ANALYSIS:"),
                         h5("The temperature might had an effect on the COIVID_19 virus 
                                     which could have helped to control the virus.
                                     Although the negligence of some countries or the delay in taking precautions 
                                     overpowered the chances offered by nature to control or limit it. "),
                         h5(" ***NOTE :Please wait for a while, annimated GIF could take about a minute or two to process the output.***" ),
                         imageOutput("map_plot") )
                
              )
      ),
      #adding link to data source 
      tabItem(tabName="source_data",
              fluidRow(
              tags$h1(" Covid -19 Data Link"),
              tags$a(href="https://www.kaggle.com/imdevskp/corona-virus-report?select=covid_19_clean_complete.csv",
                     "Click here") ,
              
              tags$h1("World Temperature Link"),
              tags$a(href="https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data",
                     "Click here"),
              
              tags$h1("World Measure for Covid-19 Link"),
              tags$a(href="https://www.kaggle.com/paultimothymooney/covid19-containment-and-mitigation-measures?select=COVID+19+Containment+measures+data.csv",
                     "Click here")
              ))

  )))




server <- function(input, output, session) {
  
  #------------------------------Plots for tab 1------------------------------------
  
  case_data <-covid_data 
  
  output$case_distribution <- renderPlotly({
  fig1 <- plot_ly(case_data, x = ~Date, y = ~Deaths, type = 'bar', name = 'Deaths')
  fig1 <- fig1 %>% add_trace(y = ~Recovered, name = 'Recovered')
  fig1 <- fig1 %>% add_trace(y = ~Active, name = 'Active')
  fig1 <- fig1 %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
  fig1
  })
  output$individual_cases <- renderPlotly({
    
    fig2 <- plot_ly(case_data, x = ~case_data[,input$var1], y = ~case_data[,input$var2],
            type= "scatter",mode = 'markers+lines',  
            marker = list(size = 10),name=input$var2)
    fig2 <- fig2 %>% layout(xaxis = list(title = input$var1), yaxis = list(title = input$var2))
    fig2  
  })
  
  

  
  #------------------------------Plots for tab 2------------------------------------
 #--------------------------------for globe--------------------------- 
  output$globeplot <- renderGlobe({
    earth <- system.file("images/world.jpg", package = "threejs")
    
    globejs(img = earth,
            lat=for_globe$Lat, 
            long=for_globe$Long,
            value = for_globe$Confirmed/100, color = "#00aaff",
            atmosphere = TRUE,
            height = "300px",
            bg = "black")  
  })
  

  
  #---------------------------------cleaning data --------------------------------
  
  
  # a bit of cleaning for faulty countries map and world temp map 
  map.world <- map_data(map="world")
  
  
  merged<-merge(map.world, for_map_color, by.x=c("region"),
                by.y=c("Country.Region"))
  
  lis1 <-c("USA","Brazil","India","Russia","Pakistan") 
  map.world_late<-subset(merged, merged$region %in% lis1 == TRUE)

  #----------------------------------animation--------------------------------  
  # animmation gif for virus growth
  
  world <- ggplot() +
    borders("world", colour = "gray85",fill="gray80") +
    theme_map()
  
  output$map_plot <- renderImage({ 
    
    outfile <- tempfile(fileext='.gif')
    map1<-world +
    geom_point(aes(x = Long, y = Lat, size = Confirmed ),
               data = temp_data, colour = 'blue', alpha = .5) + 
    transition_manual(Date, cumulative = TRUE)+
    labs(size = 'Number of Cases') + ggtitle("Moving to frame {frame}")
    
    animate(map1, height = 600, width =1000)
    anim_save("outfile.gif")
    
    list(src = "outfile.gif",
         contentType = 'image/gif'
    )}, deleteFile = TRUE)
  
  
  
  
#___________________________for late precaution countries-------------------
  
  
  
  output$late <- renderPlot({
    ggplot() +
      borders("world", colour = "#1A5276")+
      geom_map(data=map.world_late, map=map.world, aes(map_id=region, x=long, y=lat, late="#2E86C1"))
    
  })
  
  
  
  #--------------------temp of world-------------------------------------
  output$world_color <- renderPlot({
    ggplot() +
    geom_map(data=merged, map=map.world, aes(map_id=region, x=long, y=lat, fill=avg_temp)) +
    scale_fill_gradient(low = "lightblue", high = "blue", guide = "colourbar")
    
  })
  
  # for hover in world temp map
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(merged, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    wellPanel(
      p(HTML(paste0("<b> Region: </b>", point$region, "<br/>",
                    "<b> Avg-Temp: </b>", point$avg_temp, "<br/>"
                    )))
    )
    
    
  })
  
}

shinyApp(ui, server)





