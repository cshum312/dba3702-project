library(dplyr)
library(tidyr)
library(ggplot2)
library(onemapsgapi)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shiny)

setwd("C:/Users/user/working_directory")
restaurants <- read.csv("restaurants.csv") %>%
    filter(!is.na(Discount))
yummylist <- restaurants$Title

ui <- fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "darkly"),
    titlePanel("BudgetSG"),
    
    tabsetPanel(id = "tabs",
                tabPanel("Map", id = "map", 
                         sidebarLayout(
                             sidebarPanel(
                                 checkboxGroupInput("checkGroup", 
                                                    "Location Type", 
                                                    choices = list("Attractions" = 1, 
                                                                   "Restaurants" = 2, 
                                                                   "Transport" = 3),
                                                    selected = 0),
                                 radioButtons("plot", "Plot Type", choices = c("Cluster","Point", "Heatmap")),
                                 htmlOutput("ZoneText")
                             ),
                             mainPanel(leafletOutput("Zone"))
                         )),
                tabPanel("Attraction Metrics", id = "attr",
                         sidebarLayout(
                             sidebarPanel(
                                 radioButtons("type1", "Plot Type", choices = list("Bar", "Pie")),
                                 sliderInput("ranking", "Number of Rankings", min = 1, max = 298, value = c(1, 10)),
                                 htmlOutput("AttrText")
                             ),
                             mainPanel(plotOutput("Attr"),
                                       tableOutput("AttrList"))
                         )),
                tabPanel("Food Metrics", id = "food",
                         sidebarLayout(
                             sidebarPanel(
                                 radioButtons("type2", "Plot Type", choices = list("Bar", "Pie")), 
                                 selectInput("category", label = "Category", 
                                             choices = list("Area","Price","Discount")),
                                 htmlOutput("YumsText")
                             ),
                             mainPanel(plotOutput("Yums"))
                         )),
                tabPanel("Restaurant Info", id = "info",
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("info", label = "Choose Restaurant",
                                             choices = yummylist)
                             ),
                             mainPanel(
                                 htmlOutput("Text")
                             )
                         ))
    ))

server <- function(input, output, session){
    
    setwd("C:/Users/chant/OneDrive - National University of Singapore/NUS/Y4_S1/DBA3702/DBA3702 R Scribbles")
    getAttractionData <- reactive({
        attractions <- read.csv("attractions.csv") %>%
            select(Name, lat, lng, URL) %>%
            rename(Latitude = lat, Longitude = lng) %>%
            mutate(Type = 1, Color = 'Red', popup= paste0('<a href =', URL, '>', Name, '</a>'))
        return(attractions)
    })
    
    getRestaurantData <- reactive({
        restaurants <- read.csv("restaurants.csv") %>%
            filter(!is.na(Discount)) %>%
            select(Title, lat, lng, Link) %>%
            rename(Name = Title, Latitude = lat, Longitude = lng, URL = Link)
        restaurants$Latitude <- as.numeric(restaurants$Latitude) 
        restaurants <- restaurants %>%
            filter(!is.na(Latitude)) %>%
            filter(!is.na(Longitude)) %>%
            mutate(Type = 2, Color = 'Green', popup= paste0('<a href =', URL, '>', Name, '</a>'))
        return(restaurants)
    })
    
    getTransportData <- reactive({
        mrt <- read.csv("mrt_stations.csv") %>%
            select(STN_NAME, lat, lng) %>%
            rename(Name = STN_NAME, Latitude = lat, Longitude = lng)
        
        bus <- read.csv("bus_stops.csv") %>%
            select(Bus.Stop, lat, lng) %>%
            rename(Name = Bus.Stop, Latitude = lat, Longitude = lng)
        
        transports <- rbind(bus, mrt) %>%
            mutate(Type = 3, Color = 'Blue', popup= Name)
        return(transports)
    })
    
    getAreaData <- reactive({
        token <- 'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOjgxMjgsInVzZXJfaWQiOjgxMjgsImVtYWlsIjoiZTAzNzM5MDFAdS5udXMuZWR1IiwiZm9yZXZlciI6ZmFsc2UsImlzcyI6Imh0dHA6XC9cL29tMi5kZmUub25lbWFwLnNnXC9hcGlcL3YyXC91c2VyXC9zZXNzaW9uIiwiaWF0IjoxNjM2MzkyMTk2LCJleHAiOjE2MzY4MjQxOTYsIm5iZiI6MTYzNjM5MjE5NiwianRpIjoiODM1ZDBkNjRjN2E4MzBkMmVlYjQwNjJhYjNiM2UzMzgifQ.74wCvWJgcoOK-78FQduGEMGSQfTqOlfInPDi1xbRxN4'
        get_planning_areas(token, read = "sf")
    })
    
    alldata <- function(){
        attractions <- getAttractionData()
        restaurants <- getRestaurantData()
        transports <- getTransportData()
        alldata <- bind_rows(attractions, restaurants, transports)
        
        return(alldata)
    }
    
    plotMap <- function(){
        
        m <- leaflet() %>% addTiles() %>% 
            addPolygons(data = getAreaData(), label = ~name, color = 'Grey')
        
        return(m)
    }
    
    toListen <- reactive({
        list(input$plot,input$checkGroup)
    })
    
    observeEvent(toListen(), {
        alldata <- alldata()
        mydata <- alldata[alldata$Type %in% input$checkGroup, ]
        
        leafy <- leafletProxy("Zone", data = mydata) %>%
            clearGroup("Cluster") %>%
            clearGroup("Point") %>%
            clearGroup("Heatmap")
        
        if(input$plot == "Cluster"){
            leafy %>%
                addCircleMarkers(clusterOptions = markerClusterOptions(), lat = ~Latitude, lng = ~Longitude, 
                                 radius =5, popup = ~popup, group = "Cluster")
        }
        
        else if(input$plot == "Point"){
            leafy %>%
                addCircleMarkers(lat = ~Latitude, lng = ~Longitude, popup = ~popup,
                                 radius =5, color = ~Color, weight = 0.2,
                                 opacity = 0.5, group = "Point")
        }
        
        else{
            leafy %>%
                addWebGLHeatmap(lat = ~Latitude, lng = ~Longitude, size = 2000, 
                                group = "Heatmap")
        }
        
    })
    
    getAttrData <- reactive({
        attracts <- read.csv("attractions.csv") %>%
            select(X, Name, area) %>%
            rename(Ranking = X, Area = area) %>%
            mutate(Ranking = rownames(.))
        return(attracts)
    })
    
    filterAttr <- function(){
        allattr <- getAttrData()
        myattr <- allattr[input$ranking[1]:input$ranking[2], ] %>%
            group_by(Area) %>%
            summarize(Count = n())
        myattr <- as.data.frame(myattr)
        return(myattr)
    }
    
    plotGraph1 <- reactive({
        
        if (input$type1 == "Bar"){
            attrPlot <- ggplot(filterAttr(), aes_string(x = "Area", y = "Count")) +
                geom_bar(stat = "identity", fill = 'red') +
                ggtitle("Number of Attractions in each Area") +
                theme(axis.text.x = element_text(angle=90, size = 12),
                      axis.text.y = element_text(face= "bold", size = 12), 
                      axis.title.x = element_text(size=14, face="bold"),
                      axis.title.y = element_text(size=14, face="bold"),
                      title=element_text(size=18, face='bold'))
        }
        else {
            attrPlot <- ggplot(filterAttr(), aes_string(x = 1, y = "Count", fill ="Area", label = "Area")) +
                geom_col(width = 1) +
                coord_polar(theta = "y") +
                ggtitle("Number of Attractions in each Area") +
                theme(axis.title.x = element_text(size=14, face="bold"),
                      axis.title.y = element_text(size=14, face="bold"),
                      title=element_text(size=18, face='bold'))
        }
        
        return(attrPlot)  
    })
    
    makeTable <- reactive({
        allattr <- getAttrData()
        attrTable <- allattr[input$ranking[1]:input$ranking[2], ]
        return(attrTable)
    })
    
    getFoodData <- reactive({
        foodies <- read.csv("restaurants.csv") %>%
            filter(!is.na(Discount)) %>%
            select(Title, Price, Discount, area) %>%
            rename(Name = Title, Area = area)
        return(foodies)
    })
    
    filterFood <- function(){
        allfood <- getFoodData()
        
        if (input$category == "Price"){
            myfood <- allfood %>%
                group_by(Price) %>%
                summarize(Count = n())
            myfood <- as.data.frame(myfood)
        }
        else if (input$category == "Discount"){
            myfood <- allfood %>%
                group_by(Discount) %>%
                summarize(Count = n())
            myfood <- as.data.frame(myfood)
        }
        else {
            myfood <- allfood %>%
                group_by(Area) %>%
                summarize(Count = n())
            myfood <- as.data.frame(myfood)
        }
        return(myfood)
    }
    
    plotGraph2 <- reactive({
        
        if (input$type2 == "Bar"){
            foodPlot <- ggplot(filterFood(), aes_string(x = input$category, y = "Count")) + 
                geom_bar(stat = "identity", fill = 'green') + 
                ggtitle(paste("Number of Restaurants in each",input$category,"Category")) +
                theme(axis.text.x = element_text(angle=90, size = 12),
                      axis.text.y = element_text(face= "bold", size = 12), 
                      axis.title.x = element_text(size=14, face="bold"),
                      axis.title.y = element_text(size=14, face="bold"),
                      title=element_text(size=18, face='bold'))
        }
        else{
            foodPlot <- ggplot(filterFood(), aes_string(x = 1, y = "Count", fill =input$category, label = input$category)) +
                geom_col(width = 1) +
                coord_polar(theta = "y") +
                ggtitle(paste("Number of Restaurants in each",input$category,"Category")) +
                theme(axis.title.x = element_text(size=14, face="bold"),
                      axis.title.y = element_text(size=14, face="bold"),
                      title=element_text(size=18, face='bold'))
            
        }
        return(foodPlot)  
    })
    
    output$Zone <- renderLeaflet({
        plotMap()
    })
    
    output$ZoneText <- renderUI({
        HTML(paste(paste0("<b>","Instructions","</b>"), 
                   "<b> 1. </b> Select the Location Type(s) to view.",
                   "<b> 2. </b> Select the Plot Type you wish to view in.",
                   "<b> 3. </b> Mouse over regions to see region name.",
                   "<b> 4. </b> If you have chosen a given region (eg. Orchard), you may double click on the region to view the surrounding attractions, transport, and restaurants to decide where to go.",
                   "<b> e.g. </b> Use Cluster plot to zoom in, then switch to Point plot to see locations or select other location types, then click to view name of location and links to reviews, where available.",
                   sep = '<br/>'))
    })
    
    output$Attr <- renderPlot({
        plotGraph1()
    })
    
    output$AttrText <- renderUI({
        HTML(paste(paste0("<b>","Instructions","</b>"), 
                   "<b> 1. </b> Choose type of plot.",
                   "<b> 2. </b> Choose the number of attractions to view via their rankings.", 
                   "",
                   "<b>  </b>   You can adjust the slider to 20 to show the top 20 most popular attractions in Singapore!",
                   sep = '<br/>'))
    })
    
    output$AttrList <- renderTable({
        makeTable()
    })
    
    output$Yums <- renderPlot({
        plotGraph2()
    })
    
    output$YumsText <- renderUI({
        HTML(paste(paste0("<b>","Instructions","</b>"), 
                   "<b> 1. </b> Choose type of plot.",
                   "<b> 2. </b> Choose category of metric.",
                   "",
                   "            Under <b> Area </b>, you will be able to see the number of restaurants in each Area.",
                   "            Under <b> Price </b>, you will be able to see the number of restaurants ranging from cheap to expensive.",
                   "            Under <b> Discount </b>, you will be able to see the number of restaurants that are having ongoing discounts.",
                   sep = '<br/>'))
    })
    
    output$Text <- renderUI({
        selected <- restaurants[restaurants$Title == input$info, ]
        
        HTML(paste(paste0("<b>","Cuisine: ","</b>", as.character(selected[4])), 
                   paste0("<b>","Price: ","</b>", as.character(selected[7])),  
                   paste0("<b>","Good For: ","</b>", as.character(selected[8])), 
                   paste0("<b>","Discount: ","</b>", as.character(selected[15]), "%"), 
                   paste0("<b>","Location: ","</b>", as.character(selected[9])),  
                   sep = '<br/>'))
    })
}

shinyApp(ui,server)
