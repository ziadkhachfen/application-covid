library("FactoMineR")
library("factoextra")
library(hrbrthemes)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(plotrix)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(tidyverse)




shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Tableau de bord Covid-19",
                    titleWidth = 300),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Acceuil", tabName = "acceuil", icon = icon("fa-thin fa-virus")),
        menuItem("Tableau de bord", tabName = "graphes", icon = icon("dashboard")),
        menuItem("Covid-19 mapper", tabName = "covidmaps", icon = icon("fa-thin fa-map")),
        menuItem("Classification des États", tabName = "classif", icon = icon("list-ol")),
        menuItem("Données", icon = icon("database"), href = "https://data.world/mschnars/covid-19/workspace/file?filename=Covid+ESRI")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem("acceuil", align="center",fluidRow(column(4,imageOutput("myImage")),column(4,imageOutput("myImage2")),column(4, imageOutput("myImage1")), strong("Ce tableau de 
                                                bord a été créé à des fins éducatives et pour présenter les techniques de visualisation 
                                                de données rendues possibles par R Shiny. Nous utilisons les données COVID-19 provenant de 
                                                Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)."),fluidRow(column(2,strong("Données disponibles"))), fluidRow(column(4,h4("Dernière mise à jour des données: 31/01/2022")))) 
                
        ),
        
        tabItem("covidmaps",bootstrapPage(
          leafletOutput("mymap", width = "100%", height = "500px"),
          absolutePanel(column(4,dateRangeInput(
            inputId = "dates",
            label = "Période",
            start = "2021-01-22",
            end = "2022-01-31"))
            , column(4,selectInput(inputId = "choix", label = "", 
                                   choices = c("Nombre des nouveaux cas","Nombre de décés",
                                               "Nombre de cas confirmé"))),
            
            column(4,selectInput("colors", "Color Scheme",
                                 rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
            ))
            
            
            
          ))
          
        ),
        tabItem("classif",tabBox(title = "Classification des états",height = "250px",width = 12,
                                 selected = "Tab1", tabPanel(title = "classification hiérarchique", plotOutput("hierar")),
                                 tabPanel(title =" Classification par groupe", plotOutput("groupe")))
                
        ),
        tabItem("graphes", 
                fluidRow(column(6,selectInput(inputId ="state", label = "Veuillez selectionner un état" , 
                                              choices= c(unique(data$Province_State)))),
                         column(6,dateInput(inputId = "dates1",
                                            label = "Veuillez insérer une date",
                                            value ="2021-11-28",
                                            min = "2021-01-22",
                                            max = "2022-01-31", 
                                            format = "yyyy-mm-dd"))),
                fluidRow(column(6,textOutput("text"))),
                fluidRow( infoBox("Nombre total de décès",textOutput("totaldeces"), color = "red"), 
                          infoBox("Nombre total de cas confirmés", textOutput('totalconfirme'), color = "orange"), infoBox("Nombre total de nouveaux cas", 
                                                                                                                           textOutput("totalnouveaucas"),color = "yellow"),
                          fluidRow(infoBox("Taux d'incidence ", textOutput("incidencerate"),color = "purple"), 
                                   infoBox("Population",textOutput("pop"), color = "green")),
                          fluidRow(tabBox(width = 12, selected = "Map", tabPanel(  title = "Map", leafletOutput("map")),
                                          tabPanel( title= "Table", dataTableOutput("table")))
                          ),
                          fluidRow(tabBox(width= 6, selected = "décès", tabPanel( title = "décès",plotOutput("deces")), 
                                          tabPanel(  title = "confirmés", plotOutput("confirme")), tabPanel( title = "Nouveaux cas", 
                                                                                                             plotOutput("nvcas")), tabPanel(title = "Taux d'incidence", plotOutput("tauxinciden")) ),
                                   
                                   tabBox(width = 6 , selected = "pie chart décès", tabPanel(title ="pie chart décès", plotOutput("piedeces")), 
                                          tabPanel(title = "pie chart confirmés", plotOutput("pieconfirmed")), tabPanel(title = "pie chart nouveau cas", 
                                                                                                                        plotOutput("pienvcas"))))),
                
                fluidRow(column(4,downloadButton(outputId = "Download1", "Telecharger les données"))
                )
                
        ))
      
    ),
    title = "Titre dans le navigateur",
    skin = "black"
  ),
  server = function(input, output) {
    
    ####### Image acceuil
    output$myImage <- renderImage({
      pfad <- "C:\\Users\\Goldenshop.ma\\Downloads\\logo ubfc.png"
      list(src = pfad,
           contentType = 'image/png',
           width = 300,
           height = 200,
           alt = "This is alternate text")
    }, deleteFile = F)
    
    output$myImage1 <- renderImage({
      pfad <- "C:\\Users\\Goldenshop.ma\\Downloads\\lmb.png"
      list(src = pfad,
           contentType = 'image/png',
           width = 300,
           height = 200,
           alt = "This is alternate text")
    }, deleteFile = F)
    
    output$myImage2 <- renderImage({
      pfad <- "C:\\Users\\Goldenshop.ma\\Downloads\\covid.png"
      list(src = pfad,
           contentType = 'image/png',
           width = 200,
           height = 200,
           alt = "This is alternate text")
    }, deleteFile = F)
    
    
    
    ##### Maps
    
    ### Couleur de la palette
    r_colors <- rgb(t(col2rgb(colors()) / 255))
    names(r_colors) <- colors()
    
    
    ## creation de la maps
    ##construction de la data 
    
    points <- reactive({
      new_data <-  data  %>%  filter(Date <= as.POSIXct(input$dates[2], tz="UTC") &  Date >= as.POSIXct(input$dates[1], tz="UTC"))
      if (input$choix == "Nombre des nouveaux cas")
      {
        new_data <- new_data %>% select(Lat, Long_,Admin2,Date,Province_State,NewCases)
        new_data <- new_data %>% group_by(Lat, Long_, Admin2, Province_State) %>% summarise(NewCases = sum(NewCases))
        
      }
      else{
        if (input$choix ==  "Nombre de décés")
        {
          new_data <- new_data %>% filter(Date == as.POSIXct(input$dates[2], tz="UTC"))
          new_data <- new_data %>% select(Lat, Long_,Admin2,Province_State,Deaths_x)
          
        }
        else{
          new_data <- new_data %>% filter(Date == as.POSIXct(input$dates[2], tz="UTC"))
          new_data <- new_data %>% select(Lat, Long_,Admin2,Province_State,Confirmed_x)
        }
      }
      new_data
      
    })
    
    colorpal <- reactive({
      data_col <- points()
      colorNumeric(input$colors, data_col$NewCases)
    })
    
    output$mymap <- renderLeaflet({
      pal <- colorpal()
      if (input$choix == "Nombre des nouveaux cas")
      {
        map <-  leaflet(points()) %>% setView(lng = -99, lat = 45, zoom = 3) %>%  
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE))%>% 
          addCircles(lat = ~Lat, lng = ~Long_ ,radius = ~sqrt(NewCases)*30,weight = 1, 
                     fillColor = ~pal(NewCases), 
                     color = "red",fillOpacity = 0.7, popup = ~paste(NewCases))
        
      }
      else
      {
        if (input$choix ==  "Nombre de décés")
        {
          map <-  leaflet(points()) %>% setView(lng = -99, lat = 45, zoom = 2) %>%  
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE))%>%
            addCircles(lat = ~Lat, lng = ~Long_ ,radius = ~sqrt(Deaths_x)*30,weight = 1, 
                       fillColor = ~pal(Deaths_x), 
                       color = "red",fillOpacity = 0.7, popup = ~paste(Deaths_x))
        }
        else
        {
          map <-  leaflet(points()) %>% setView(lng = -99, lat = 45, zoom = 2) %>%  
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE))%>%
            addCircles(lat = ~Lat, lng = ~Long_ ,radius = ~sqrt(Confirmed_x)*30,weight = 1, 
                       fillColor = ~pal(Confirmed_x), 
                       color = "red",fillOpacity = 0.7, popup = ~paste(Confirmed_x))
        }
      }
      map
      
      
    })
    
    
    ### Donnees de la classification 
    output$hierar <- renderPlot({
      ### Donnes classification 
      data <- data %>% filter(data$Lat != 0 & data$Long_ != 0)
      new <-  data  %>%  filter(Date <= as.POSIXct(input$dates[2], tz="UTC") &  Date >= as.POSIXct(input$dates[1], tz="UTC"))
      data1 <- data %>% filter(Date == as.POSIXct(input$dates[2], tz="UTC"))
      data1 <-  data1 %>% group_by(Province_State) %>% summarise(Lat= mean(Lat),
                                                                 Long_ = mean(Long_), Population = sum(Population), Confirmed_x = sum(Confirmed_x),
                                                                 Deaths_x = sum(Deaths_x))
      data2 <- new %>% group_by(Province_State) %>% summarise(NewCases = sum(NewCases))
      new_data <- left_join(data1, data2)
      ## ACP 
      matric_data <-as.matrix(new_data[,-1])
      row.names(matric_data) <- new_data$Province_State
      
      res.pca <- PCA(matric_data, scale.unit = TRUE, ncp = 3, graph = FALSE)
      res.hcpc <- HCPC(res.pca, graph = FALSE)
      g <- fviz_dend(res.hcpc, 
                     cex = 0.7,                     # Taille du text
                     palette = "jco",               # Palette de couleur ?ggpubr::ggpar
                     rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
                     rect_border = "jco",           # Couleur du rectangle
                     labels_track_height = 5     # Augment l'espace pour le texte
      )
      g
    })
    output$groupe <- renderPlot({
      ### Donnes classification 
      data <- data %>% filter(data$Lat != 0 & data$Long_ != 0)
      new <-  data  %>%  filter(Date <= as.POSIXct(input$dates[2], tz="UTC") &  Date >= as.POSIXct(input$dates[1], tz="UTC"))
      data1 <- data %>% filter(Date == as.POSIXct(input$dates[2], tz="UTC"))
      data1 <-  data1 %>% group_by(Province_State) %>% summarise(Lat= mean(Lat),
                                                                 Long_ = mean(Long_), Population = sum(Population), Confirmed_x = sum(Confirmed_x),
                                                                 Deaths_x = sum(Deaths_x))
      data2 <- new %>% group_by(Province_State) %>% summarise(NewCases = sum(NewCases))
      new_data <- left_join(data1, data2)
      ## ACP 
      matric_data <-as.matrix(new_data[,-1])
      row.names(matric_data) <- new_data$Province_State
      
      res.pca <- PCA(matric_data, scale.unit = TRUE, ncp = 3, graph = FALSE)
      res.hcpc <- HCPC(res.pca, graph = FALSE)
      k <-  fviz_cluster(res.hcpc,
                         repel = TRUE,            # Evite le chevauchement des textes
                         show.clust.cent = TRUE, # Montre le centre des clusters
                         palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
                         ggtheme = theme_minimal(),
                         main = "Factor map"
      )
      k
    })
    
    ### donnes table sortie
    output$table <- renderDataTable({
      data_new <- data %>% select(Lat, Long_,Admin2,Date,Province_State,NewCases, Deaths_x, Confirmed_x)
      data_new <- data_new %>% filter(Province_State == input$state & Date == as.POSIXct(input$dates1[1], tz="UTC") )
      
    },options = list(
      pageLength = 5))
    output$table1 <- reactive({
      data_new <- data %>% select(Lat, Long_,Admin2,Date,Province_State,NewCases, Deaths_x, Confirmed_x)
      data_new <- data_new %>% filter(Province_State == input$state & Date == as.POSIXct(input$dates1[1], tz="UTC") )
      data_new
    })
    
    output$map <- renderLeaflet({  
      pal <- colorpal()
      data_new <- data %>% select(Lat, Long_,Admin2,Date,Province_State,NewCases, Deaths_x, Confirmed_x)
      data_new <- data %>% filter(Province_State == input$state & Date == as.POSIXct(input$dates1[1], tz="UTC"))
      map1 <-  leaflet(data_new) %>% setView(lng = -99, lat = 45,  zoom = 4) %>%  
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE))%>%
        addCircles(lat = ~Lat, lng = ~Long_ ,radius = ~(Deaths_x)*20,weight = 1, 
                   fillColor = ~pal(Deaths_x), color = "red",fillOpacity = 0.7, 
                   popup = ~paste(Deaths_x))
      map1
    })
    
    output$totaldeces <- renderText({
      
      data_new <- data %>% select(Lat, Long_,Admin2,Date,Province_State,NewCases, Deaths_x, Confirmed_x)
      data_new <- data_new %>% filter(Province_State == input$state & Date == input$dates1[1])
      total <- sum(data_new$Deaths_x)
      total
    })
    
    output$totalconfirme <- renderText({
      
      data_new <- data %>% select(Lat, Long_,Admin2,Date,Province_State,NewCases, Deaths_x, Confirmed_x)
      data_new <- data_new %>% filter(Province_State == input$state & Date == input$dates1[1])
      total <- sum(data_new$Confirmed_x)
      total
    })
    output$totalnouveaucas <- renderText({
      
      data_new <- data %>% select(Lat, Long_,Admin2,Date,Province_State,NewCases, Deaths_x, Confirmed_x)
      data_new <- data_new %>% filter(Province_State == input$state & Date == input$dates1[1])
      total <- sum(data_new$NewCases)
      total
    })
    output$pop <- renderText({
      
      data_new <- data %>% select(Lat, Long_,Admin2,Date,Population,Province_State,NewCases, Deaths_x, Confirmed_x)
      data_new <- data_new %>% filter(Province_State == input$state & Date == input$dates1[1])
      total <- sum(data_new$Population)
      total
    })
    output$pop <- renderText({
      
      data_new <- data %>% select(Lat, Long_,Admin2,Date,Population,Province_State,NewCases, Deaths_x, Confirmed_x, IncidenceRate)
      data_new <- data_new %>% filter(Province_State == input$state & Date == input$dates1[1])
      total <- sum(data_new$Population)
      total
    })
    
    output$incidencerate <- renderText({
      data_new <- data %>% select(Lat, Long_,Admin2,Date,Population,Province_State,NewCases, Deaths_x, Confirmed_x, IncidenceRate)
      data_new <- data_new %>% filter(Province_State == input$state & Date == input$dates1[1])
      ind <-  sum(data_new$NewCases) / sum(data_new$Population) *100000
    })
    output$deces <-  renderPlot({
      data_new  <- data %>% select(Province_State, Date, Deaths_x)
      data_new <- data_new %>% filter(Province_State == input$state & Date <= as.POSIXct(input$dates1[1], tz="UTC") )
      data_new <- data_new %>% group_by(Province_State, Date) %>% summarise(Deaths_x = sum(Deaths_x))
      k <- data_new %>% ggplot(aes(x =Date, y = Deaths_x))+geom_area(fill="#69b3a2", alpha=0.5) +
        geom_line(color="#f92354") +
        ylab("Evolution de nombre total de decés") +
        theme_ipsum()
      
      k
    })
    output$confirme <-  renderPlot({
      data_new  <- data %>% select(Province_State, Date, Confirmed_x)
      data_new <- data_new %>% filter(Province_State == input$state & Date <= as.POSIXct(input$dates1[1], tz="UTC") )
      data_new <- data_new %>% group_by(Province_State, Date) %>% summarise(Confirmed_x= sum(Confirmed_x))
      k <- data_new %>% ggplot(aes(x =Date, y = Confirmed_x))+geom_area(fill="#69b3a2", alpha=0.5) +
        geom_line(color="#f95d23") +
        ylab("Evolution de nombre total de cas confirmé") +
        theme_ipsum()
      
      k 
    })
    output$nvcas <-  renderPlot({
      data_new  <- data %>% select(Province_State, Date, NewCases)
      data_new <- data_new %>% filter(NewCases >= 0 & Province_State == input$state & Date <= as.POSIXct(input$dates1[1], tz="UTC") )
      data_new <- data_new %>% group_by(Province_State, Date) %>% summarise(NewCases= sum(NewCases))
      k <- data_new %>% ggplot(aes(x =Date, y = NewCases))+geom_area(fill="#69b3a2", alpha=0.5) +
        geom_line(color="#69b3a2") +
        ylab("Evolution de nombre total de nouveau cas par jour ") +
        theme_ipsum()
      
      k
    })
    
    output$tauxinciden <- renderPlot({
      data_new  <- data %>% select(Province_State, Date, NewCases, Population, Confirmed_x)
      data_new <- data_new %>% filter(NewCases >= 0 & Province_State == input$state & Date <= as.POSIXct(input$dates1[1], tz="UTC") )
      data_new <- data_new %>% group_by(Province_State, Date) %>% summarise(Population = sum(Population), Confirmed_x = sum(Confirmed_x), NewCases = sum(NewCases))
      tauxincide <- (data_new$NewCases / data_new$Population) *100000
      data_new <-  data.frame(data_new,tauxincide)
      k <- data_new %>% ggplot(aes(x=Date, y = tauxincide ))+geom_area(fill="#69b3a2", alpha=0.5) +
        geom_line(color="#69b3a2") +
        ylab("Evolution du taux d'incidence par jour ") +
        theme_ipsum()
      k
    })
    
    
    output$piedeces <- renderPlot({
      myPalette <- brewer.pal(10, "Set2") 
      data_new <- data %>% select(Lat, Long_,Admin2,Date,Province_State,NewCases, Deaths_x, Confirmed_x)
      data_new <- data_new %>% filter(Province_State == input$state & Date == as.POSIXct(input$dates1[1], tz="UTC"))
      permutation <- order(data_new$Deaths_x ,decreasing = TRUE)
      data_new <- data_new[permutation,]
      top_ten <- data_new[1:10,]
      pie(top_ten$Deaths_x ,labels=top_ten$Admin2,explode=0.1,col=myPalette,border="white",
          main="Top 10 régions avec le nombre de decés le plus elevé ")
    })
    
    output$pieconfirmed <- renderPlot({
      myPalette <- brewer.pal(10, "Set2") 
      data_new <- data %>% select(Lat, Long_,Admin2,Date,Province_State,NewCases, Deaths_x, Confirmed_x)
      data_new <- data_new %>% filter(Province_State == input$state & Date == as.POSIXct(input$dates1[1], tz="UTC"))
      permutation <- order(data_new$Confirmed_x ,decreasing = TRUE)
      data_new <- data_new[permutation,]
      top_ten <- data_new[1:10,]
      pie(top_ten$Confirmed_x ,labels=top_ten$Admin2,explode=0.1,col=myPalette,border="white",
          main="Top 10 régions avec le nombre de cas confirmés le plus elevé ")
    })
    
    output$pienvcas <- renderPlot({
      myPalette <- brewer.pal(10, "Set2") 
      data_new <- data %>% select(Lat, Long_,Admin2,Date,Province_State,NewCases, Deaths_x, Confirmed_x)
      data_new <- data_new %>% filter(Province_State == input$state & Date == as.POSIXct(input$dates1[1], tz="UTC"))
      permutation <- order(data_new$NewCases ,decreasing = TRUE)
      data_new <- data_new[permutation,]
      top_ten <- data_new[1:10,]
      pie(top_ten$NewCases ,labels=top_ten$Admin2,explode=0.1,col=myPalette,border="white",
          main="Top 10 régions avec le nombre de nouveaux cas enregistés le plus elevé ")
    })
    output$text <- renderText({
      paste("vous avez selectionné",input$state)
    })
    output$Download1 <- downloadHandler(
      filename = function() {
        paste("data download", ".csv", sep = "")
      },
      content = function(file) {
        write.table(table(), file, row.names = FALSE)
      }
    )
  })

