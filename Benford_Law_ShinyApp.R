#### Library ##############################################
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(esquisse)
library(maps)
library(jsonlite)
library(kableExtra)
library(DataExplorer)
library(magrittr)
library(benford.analysis)
library(BenfordTests)
library(ggpubr)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(reshape2)
library(kableExtra)
library(plotly)
library(leaflet)
library(sp)
library(rgdal)
library(htmltools)
############################################################
timeoutSeconds <- 3000
#### Dataset ##############################################
movie <- read_csv("tmdb_5000_movies.csv")
crew <- read_csv("tmdb_5000_credits.csv")
mv <- movie
world <- map_data("world")
world_list <- (distinct(world, region))$region
###########################################################

#### Data Preparation #####################################
mv <- mv %>% select(-tagline,-homepage, -overview)
mv_kws <- mv %>% filter(nchar(keywords)>2) %>% mutate(js = lapply(keywords,fromJSON)) %>% 
  unnest(js) %>% select(id, title, keyword=name)
mv_gnr <- mv %>% filter(nchar(genres)>2) %>% mutate(js = lapply(genres,fromJSON)) %>%
  unnest(js) %>% select(id, title, genres=name)
mv_company <- mv %>% filter(nchar(production_companies)>2) %>% mutate(js = lapply(production_companies,fromJSON)) %>%
  unnest(js) %>% select(id, title, production_companies=name)
mv_country <- mv %>% filter(nchar(production_countries)>2) %>% mutate(js = lapply(production_countries,fromJSON)) %>%
  unnest(js) %>% select(id, title, production_countries=name)
crew_clean <- crew %>% filter(nchar(crew)>2) %>% mutate(js=lapply(crew,fromJSON)) %>% unnest(js)
director <- crew_clean %>% filter(job=="Director") %>% mutate(director=name) %>% select(movie_id, director)
mv_gnr %<>% group_by(genres) %>% mutate(total=n()) %>% ungroup()
mv_company %<>% group_by(production_companies) %>% mutate(total=n()) %>% ungroup()
mv_country %<>% group_by(production_countries) %>% mutate(total=n()) %>% ungroup()
mv_clean <- mv %>% select(-keywords,-genres, -production_companies, -production_countries, -spoken_languages)
mv_clean$budget <- as.numeric(mv_clean$budget)
mv_clean <- mv_clean %>% mutate(released_year=format(as.Date(release_date, format="%Y/%m/%d"),"%Y"))
mv_clean <- mv_clean %>% mutate(released_month=format(as.Date(release_date, format="%Y/%m/%d"),"%m"))
mv_clean$released_month <- as.numeric(mv_clean$released_month)
mv_clean$released_year <- as.numeric(mv_clean$released_year)
mv_clean %<>% mutate(gross=revenue-budget)
mv_clean %<>% mutate(ROI=gross/budget)
mv_everything <- mv_country %>% select(id, production_countries) %>% left_join(mv_clean, by="id")
mv_everything <- mv_company %>% select(id,production_companies) %>% left_join(mv_everything, by="id")

# Customized for Shiny use only
release_y <- c(unique(mv_clean$released_year))
movie2 <- movie %>% mutate(released_year=format(as.Date(release_date, format="%Y/%m/%d"),"%Y"))
bfd.budget <- benford(mv_clean$budget)
bfd.rev <- benford(mv_clean$revenue)
bfd.votec <- benford(mv_clean$vote_count)
bfd.popular <- benford(mv_clean$popularity)
sus_budget <- getSuspects(bfd.budget,mv_clean) 
sus_budget2 <- sus_budget %>% mutate(sus_bud=c(rep(1,nrow(sus_budget)))) %>% select(id, sus_bud)
sus_rev <- getSuspects(bfd.rev, mv_clean) 
sus_rev2 <- sus_rev %>% mutate(sus_rv=c(rep(1,nrow(sus_rev)))) %>% select(id, sus_rv)
sus_votec <- getSuspects(bfd.votec, mv_clean) 
sus_votec2 <- sus_votec %>% mutate(sus_vote=c(rep(1,nrow(sus_votec)))) %>% select(id, sus_vote)
sus_pop <- getSuspects(bfd.popular,mv_clean)
genre_ta <- mv_gnr %>% select(-title) %>% left_join(mv_everything, by="id") %>% na.omit()
gnr_kws_ta <- mv_kws %>% select(-title) %>% left_join(genre_ta, by="id") %>% na.omit() %>%
                group_by(keyword) %>% mutate(count=n()) %>% ungroup()
overview <- movie %>% select(id, overview)
ovv_tidy <- overview %>% unnest_tokens(word, overview) %>% anti_join(stop_words)
ovv_afinn <- ovv_tidy %>% inner_join(get_sentiments("afinn")) %>% group_by(id) %>% 
  summarise(sentiment=sum(score)) %>% left_join(genre_ta, by="id") %>%
  select(id, title, sentiment, genres,budget, popularity, revenue, vote_average, vote_count)
ovv_afinn %<>% group_by(title) %>% unique()
# Country df for making map
mapdf <- mv_country %>% select(id, production_countries) %>% left_join(mv_clean, by="id")
mapdf %<>% select(id, production_countries,released_year) %>% na.omit()
mapdf$production_countries[mapdf$production_countries=="United States of America"] <- "USA"
mapdf$production_countries[mapdf$production_countries=="United Kingdom"] <- "UK"
mapdf %<>% filter(production_countries %in% world_list)
mapdf <- left_join(mapdf, sus_budget2,by="id")
mapdf <- left_join(mapdf, sus_rev2, by="id")
mapdf <- left_join(mapdf,sus_votec2,by="id")
mapdf[is.na(mapdf)] <- 0
mapdf %<>% group_by(production_countries, released_year) %>% mutate(suspecious=sum(sus_bud,sus_rv,sus_vote)) %>%
              ungroup() %>% select(-id,-sus_bud,-sus_rv,-sus_vote) %>% unique()
maptemp <- mv_everything %>% group_by(production_countries, released_year) %>% count() %>% 
              ungroup() %>% rename(total=n)
maptemp$production_countries[maptemp$production_countries=="United States of America"] <- "USA"
maptemp$production_countries[maptemp$production_countries=="United Kingdom"] <- "UK"
mapdf <- left_join(mapdf, maptemp, by=c("production_countries","released_year"))
mapdf <- left_join(world, mapdf, by=c("region"="production_countries")) %>% select(-subregion)
mapdf[is.na(mapdf)] <- 0
mapdf %<>% filter(released_year!=0)
# Interactive map
mapdf2 <- mv_country %>% select(id, production_countries) %>% left_join(mv_clean, by="id")
mapdf2 %<>% select(id, production_countries) %>% na.omit()
mapdf2 <- left_join(mapdf2, sus_budget2,by="id")
mapdf2 <- left_join(mapdf2, sus_rev2, by="id")
mapdf2 <- left_join(mapdf2,sus_votec2,by="id")
mapdf2[is.na(mapdf2)] <- 0
mapdf2 %<>% group_by(production_countries) %>% mutate(suspecious=sum(sus_bud,sus_rv,sus_vote)) %>%
  ungroup() %>% select(-id,-sus_bud,-sus_rv,-sus_vote) %>% unique()
maptemp2 <- mv_country %>% group_by(production_countries) %>% count() %>% 
  ungroup() %>% rename(total=n)
mapdf2 <- left_join(mapdf2, maptemp2, by="production_countries")
#######################################################################

#### ShinyApp ##########################################################
# Define UI for application that draws a histogram
ui <- dashboardPage(
   skin = "red", # Set theme color
   dashboardHeader(title = "Movies Box Office"),
   dashboardSidebar(
     sidebarMenu(
       menuItem("Home Page", tabName = "hp", icon = icon("info-sign", lib = "glyphicon")),
       menuItem("OverView", tabName = "ov" ,icon = icon("comment",lib = "glyphicon")),
       menuItem("Benford Analysis", tabName = "ba", icon = icon("eye-open",lib = "glyphicon"),
                menuItem("Budget",tabName = "bd",icon = icon("usd",lib = "glyphicon")),
                menuItem("Revenue",tabName = "rv", icon = icon("bitcoin",lib = "glyphicon")),
                menuItem("Vote Count", tabName = "vc",icon = icon("user",lib = "glyphicon")),
                menuItem("Popularity", tabName = "pop", icon = icon("heart",lib = "glyphicon"))),
       menuItem("Text Analysis", tabName = "ta", icon = icon("text-width",lib = "glyphicon"),
                menuItem("Genre and Keywords", tabName = "gk", icon = icon("th",lib = "glyphicon")),
                menuItem("Sentiment Analysis", tabName = "sa", icon = icon("heart-empty",lib = "glyphicon"))),
       menuItem("Map", tabName = "map", icon = icon("map-marker",lib = "glyphicon"),
                menuItem("Static - View by Year", tabName = "map1", icon = icon("leaf",lib = "glyphicon")),
                menuItem("Interactive", tabName = "map2", icon = icon("fire",lib = "glyphicon"))),
       menuItem("Other EDA", tabName = "eda", icon = icon("picture",lib = "glyphicon"))
     )
   ),
   dashboardBody(
     tabItems(
       # Home Page
       tabItem(
         tabName = "hp",
         fluidRow(
           box(
             title = "About the Project",width = 8,solidHeader = TRUE,status = "danger",collapsible = TRUE,
             h3("Objectives"),
             print("In this project, I'm going to do the Benford analysis on movies' budget, revenue, vote average(ratings), vote count. Over the past a few years, a lot of movies are accused to be misreporting their revenue so that they could attract more attention from the public. Meanwhile,lots of production companies are accused to pay for people to write positive reviews to their movies.")
           )
         ),
         mainPanel(imageOutput("hp1"))
       ),
       # Overview
       tabItem(
         tabName = "ov",
         fluidRow(
           box(
             title = "Data Source", width = 8, solidHeader = TRUE, status = "danger",collapsible = TRUE,
             print("In the analysis, I will use 2 datasets, the movie dataset and the credit dataset. In the movie dataset, there are 4803 movies with 20 explanatory variables including title, language, released date, budget, revenue, runtime and so on. The credit dataset contains the list of crews and directors for each movie.")
           ),
           box(
             title = "Raw Data Frame", status = "danger",collapsible = TRUE,solidHeader = TRUE,
             DT::dataTableOutput("dataframe"), width = 12, height = 600
           ),
           box(
             title = "View by Year",status = "danger",collapsible = TRUE,
             selectInput("pickyear","Select Year",choices = release_y)
           ),
           downloadButton("dmovie","tmdb_5000_Movies"),
           downloadButton("dmovie2","tmdb_5000_Credits")
         )
       ),
       # Benford Analysis - Budget
       tabItem(
         tabName = "bd",
         plotOutput("bfd_bd",height = 400),
         fluidRow(
           column(6,box(plotOutput("sus_com_bd"),width = 12)),
           column(6,box(plotOutput("sus_dir_bd"),width = 12))
         )
       ),
       # Benford Analysis - Revnue
       tabItem(
         tabName = "rv",
         plotOutput("bfd_rv",height = 400),
         fluidRow(
           column(6,box(plotOutput("sus_com_rv"),width = 12)),
           column(6,box(plotOutput("sus_dir_rv"),width = 12))
         )
       ),
       # Benford Analysis - Vote Count
       tabItem(
         tabName = "vc",
         plotOutput("bfd_vc",height = 400),
         fluidRow(
           column(6,box(plotOutput("sus_com_vc"),width = 12)),
           column(6,box(plotOutput("sus_dir_vc"),width = 12))
         )
       ),
       # Benford Analysis - Popularity
       tabItem(
         tabName = "pop",
         plotOutput("bfd_pop",height = 400),
         fluidRow(
           column(6,box(plotOutput("sus_com_pop"),width = 12)),
           column(6,box(plotOutput("sus_dir_pop"),width = 12))
         )
       ),
       # Text Analysis - Genre and Keywords
       tabItem(
         tabName = "gk",
         fluidRow(
           column(6,box(title="Genre", status = "danger",solidHeader = TRUE,
                        plotOutput("genre_tm"),width = 12)),
           column(6,box(title="Keywords", status = "danger",solidHeader = TRUE,
                        plotOutput("keyword_tm"),width = 12))
         ),
         fluidRow(
           box(
             title = "From Year", status = "danger", solidHeader = TRUE,
             sliderInput("vbyyear1", "From", min = min(gnr_kws_ta$released_year), 
                         max = max(gnr_kws_ta$released_year), value = 1950)
           ),
           box(
             title = "View by Country", status = "danger", solidHeader = TRUE,
             selectInput("pickcompany","Select a Company", choices = unique(gnr_kws_ta$production_companies))
           )
         ),
         fluidRow(
           box(
             title = "To Year", status = "danger", solidHeader = TRUE,
             sliderInput("vbyyear2", "to", min = min(gnr_kws_ta$released_year), 
                         max = max(gnr_kws_ta$released_year), value = 2010)
           )
         )
       ),
       # Text Analysis - Sentiment Analysis
       tabItem(
         tabName = "sa",
         fluidRow(
           box(
             title = "Table of Sentiment Score for each Movie", status = "danger", solidHeader = TRUE,
             DT::dataTableOutput("sentable"), width = 12, height = 400
           )
         ),
         fluidRow(
           column(6,plotOutput("distsent",height = 500)),
           column(6, box(
             title = "View by Genre", status = "danger", solidHeader = TRUE,
             selectInput("selectgenre","Select a Genre", choices = unique(ovv_afinn$genres))
              )
           )
         )
       ),
       # Map1
       tabItem(
         tabName = "map1",
         fluidRow(
           column(12,
           box(
             title = "Static Map - View by Year",status = "danger", solidHeader = TRUE,
             plotOutput("themap"), height = 500, width = 12
           )
         )),
         fluidRow(
                      box(title = "Select From Year",status = "danger", solidHeader = TRUE,
                         sliderInput("mapbyyear1","From", min = min(mapdf$released_year), 
                                     max = max(mapdf$released_year), value = 1960)),
           box(title = "Statistics", status = "danger", solidHeader = TRUE,
                        selectInput("selectstat","Total or Suspecious",choices = c("Total Number of Movies",
                                                                                  "Number of Suspecious Movies")))

         ),
         fluidRow(
           box(title = "Select To Year",status = "danger", solidHeader = TRUE,
               sliderInput("mapbyyear2","to", min = min(mapdf$released_year), 
                           max = max(mapdf$released_year), value = 1990))
         )
       ),
       # Map2
       tabItem(
         tabName = "map2",
         fluidRow(
           column(12,
                  box(
                    title = "Interactive Map",status = "danger", solidHeader = TRUE,
                    leafletOutput("themap2"), height = 500, width = 12
                  )
           )),
         fluidRow(
           box(title = "Statistics", status = "danger", solidHeader = TRUE,
               selectInput("selectstat2","Total or Suspecious",choices = c("Total Number of Movies",
                                                                          "Number of Suspecious Movies")))
         )
       ),
       # Other EDA
       tabItem(
         tabName = "eda",
         fluidRow(
           column(12,box(title = "Overall Trend of Number of Movies",status = "danger", solidHeader = TRUE,
                         plotlyOutput("trendmv"), height = 450))
         ),
         fluidRow(
           column(12, box(title = "Revenue by Genre",status = "danger", solidHeader = TRUE,
                         plotlyOutput("revbygen"), height = 450))
         )
       )
     )
   )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

   # Home Page
   output$hp1<- renderImage({
     Leg<-"www/hp1.PNG"
     list(src=Leg)
   },deleteFile = FALSE)
   
   # Overview
   output$dataframe <- DT::renderDataTable({
     df <- movie2[movie2$released_year==input$pickyear,]
     DT::datatable(df,
                   options = list(scrollX = T,scrollY = "400px"))
   })
   output$dmovie <- downloadHandler(filename = "tmdb_5000_movies",
                                    content = function(file){write.csv(movie,file)},
                                    contentType = "text/csv")
   output$dmovie2 <- downloadHandler(filename = "tmdb_5000_credits",
                                    content = function(file){write.csv(crew,file)},
                                    contentType = "text/csv")
   # Benford - Budget
   output$bfd_bd <- renderPlot({
     plot(bfd.budget)
   })
   output$sus_com_bd <- renderPlot({
       sus_bud_com <- left_join(sus_budget,mv_company, by="title") %>% select(production_companies,total) %>% 
         group_by(production_companies) %>% mutate(count=n()) %>% arrange(desc(count)) %>% 
         ungroup() %>% na.omit()
       sus_bud_com %<>% group_by(production_companies) %>% unique() %>% ungroup()
       count1 <- sus_bud_com %>% slice(1:15)
       ggplot(count1,aes(x=reorder(production_companies,-count),y=count))+
         geom_histogram(stat = "identity", fill="#0c4c8a")+ 
         theme_minimal()+
         theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
         xlab("Production Company") + ylab("Number of Suspecious Movies") + 
         ggtitle("Suspecious Budget by Company") 
     })
    output$sus_dir_bd <- renderPlot({
       sus_bud_dr <- left_join(sus_budget, director, by=c("id"="movie_id")) %>% group_by(director) %>% count() %>%
         arrange(desc(n)) %>% ungroup() %>% slice(1:15)
       ggplot(sus_bud_dr,aes(x=reorder(director, -n), y=n))+geom_histogram(stat = "identity", fill="#0c4c8a")+
         theme_minimal()+
         theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
         xlab("Director") + ylab("Number of Suspecious Movies") +
         ggtitle("Suspecious Budget by Director")
     })
    
    # Benford - Revenue
    output$bfd_rv <- renderPlot({
      plot(bfd.rev)
    })
    output$sus_com_rv <- renderPlot({
      sus_rev_com <- left_join(sus_rev,mv_company, by="title") %>% select(production_companies,total) %>% 
        group_by(production_companies) %>% mutate(count=n()) %>% arrange(desc(count)) %>% 
        ungroup() %>% na.omit()
      sus_rev_com %<>% group_by(production_companies) %>% unique() %>% ungroup()
      count3 <- sus_rev_com %>% slice(1:15)
      ggplot(count3,aes(x=reorder(production_companies,-count),y=count))+
        geom_histogram(stat = "identity", fill="#b2df8a")+ 
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab("Production Company") + ylab("Number of Suspecious Movies") + 
        ggtitle("Suspecious Revenue by Company")
    })
    output$sus_dir_rv <- renderPlot({
      sus_rev_dr <- left_join(sus_rev, director, by=c("id"="movie_id")) %>% group_by(director) %>% count() %>%
        arrange(desc(n)) %>% ungroup() %>% slice(1:15)
      ggplot(sus_rev_dr,aes(x=reorder(director, -n), y=n))+geom_histogram(stat = "identity", fill="#b2df8a")+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab("Director") + ylab("Number of Suspecious Movies") +
        ggtitle("Suspecious Revenue by Director")
    })
    
    # Benford - Vote Count
    output$bfd_vc <- renderPlot({
      plot(bfd.votec)
    })
    output$sus_com_vc <- renderPlot({
      sus_vote_com <- left_join(sus_votec,mv_company, by="title") %>% select(production_companies,total) %>% 
        group_by(production_companies) %>% mutate(count=n()) %>% arrange(desc(count)) %>% 
        ungroup() %>% na.omit()
      sus_vote_com %<>% group_by(production_companies) %>% unique() %>% ungroup()
      count5 <- sus_vote_com %>% slice(1:15)
      ggplot(count5,aes(x=reorder(production_companies,-count),y=count))+
        geom_histogram(stat = "identity", fill="#ef3b2c")+ 
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab("Production Company") + ylab("Number of Suspecious Movies") + 
        ggtitle("Suspecious Vote Count by Company")
    })
    output$sus_dir_vc <- renderPlot({
      sus_vote_dr <- left_join(sus_votec, director, by=c("id"="movie_id")) %>% group_by(director) %>% count() %>%
        arrange(desc(n)) %>% ungroup() %>% na.omit() %>% slice(1:15)
      ggplot(sus_vote_dr,aes(x=reorder(director, -n), y=n))+geom_histogram(stat = "identity", fill="#ef3b2c")+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab("Director") + ylab("Number of Suspecious Movies") +
        ggtitle("Suspecious Vote Count by Director")
    })
    
    # Benford - Popularity
    output$bfd_pop <- renderPlot({
      plot(bfd.popular)
    })
    output$sus_com_pop <- renderPlot({
      sus_pop_com <- left_join(sus_pop,mv_company, by="title") %>% select(production_companies,total) %>% 
        group_by(production_companies) %>% mutate(count=n()) %>% arrange(desc(count)) %>% 
        ungroup() %>% na.omit()
      sus_pop_com %<>% group_by(production_companies) %>% unique() %>% ungroup()
      count7 <- sus_pop_com %>% slice(1:15)
      ggplot(count7,aes(x=reorder(production_companies,-count),y=count))+
        geom_histogram(stat = "identity", fill="#9c179e")+ 
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab("Production Company") + ylab("Number of Suspecious Movies") + 
        ggtitle("Suspecious Popularity by Company")
    })
    output$sus_dir_pop <- renderPlot({
      sus_pop_dr <- left_join(sus_pop, director, by=c("id"="movie_id")) %>% group_by(director) %>% count() %>%
        arrange(desc(n)) %>% ungroup() %>% na.omit() %>% slice(1:15)
    ggplot(sus_pop_dr,aes(x=reorder(director, -n), y=n))+geom_histogram(stat = "identity", fill="#9c179e")+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab("Director") + ylab("Number of Suspecious Movies") +
        ggtitle("Suspecious Popularity by Director")
    })
    
    # Text Analysis - Genre
    output$genre_tm <- renderPlot({
      gnr_kws1 <- gnr_kws_ta[gnr_kws_ta$released_year %in% c(input$vbyyear1:input$vbyyear2),]
      gnr_kws2 <- gnr_kws1[gnr_kws1$production_companies==input$pickcompany,]
      wordcloud(words = unique(gnr_kws2$genres), freq = unique(gnr_kws2$total),max.words = 200, 
                scale = c(5,.2), min.freq = 3,random.order = FALSE, rot.per = .15,
                colors = brewer.pal(n=12, name = "Paired"))
    })
    
    # Text Analysis - Keywords
    output$keyword_tm <- renderPlot({
      gnr_kws1 <- gnr_kws_ta[gnr_kws_ta$released_year %in% c(input$vbyyear1:input$vbyyear2),]
      gnr_kws2 <- gnr_kws1[gnr_kws1$production_companies==input$pickcompany,]
      wordcloud(words = unique(gnr_kws2$keyword), freq = unique(gnr_kws2$count),max.words = 100, 
                scale = c(1.5,.5), min.freq = 3,random.order = FALSE, rot.per = .5,
                colors = brewer.pal(n=8, name = "Dark2"))
    })
    
    # Text Analysis - Sentiment Analysis
    
    output$sentable <- DT::renderDataTable({
      dt <- ovv_afinn[ovv_afinn$genres==input$selectgenre,]
      DT::datatable(dt,options=list(scrollX = T,scrollY = "200px"))
    })
    output$distsent <- renderPlot({
      dt <- ovv_afinn[ovv_afinn$genres==input$selectgenre,]
      ggplot(data = dt) + aes(x = sentiment) + 
        geom_histogram(aes(y=..density..), bins = 50, fill="gray") +
        geom_density(adjust = 0.7, fill = "#9ecae1", alpha= 0.5,color="#9ecae1") +
        labs(title = "Distribution of Sentiment Scores", y = "Density") +
        theme_minimal()
    })
    
    # Map
    output$themap <- renderPlot({
      maptp <- mapdf[mapdf$released_year %in% c(input$mapbyyear1:input$mapbyyear2),]
      maptp %<>% group_by(region,released_year) %>% mutate(total_mv=sum(unique(total))) %>%
                    mutate(suspecious_mv=sum(unique(suspecious)))
      if (input$selectstat=="Total Number of Movies"){
        ggplot(maptp)+geom_polygon(aes(x=long,y=lat,group=group,fill=total_mv))+coord_quickmap()+
          scale_fill_gradient(low = "light blue", high="blue")+theme_minimal()
        
      } else {
        ggplot(maptp)+geom_polygon(aes(x=long,y=lat,group=group,fill=suspecious_mv))+coord_quickmap()+
          scale_fill_gradient(low = "pink", high="red")+theme_minimal()
        
      }
    })
    
    # Map2
    output$themap2 <- renderLeaflet({
      url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"
      tmp <- tempdir()
      file <- basename(url)
      download.file(url,file)
      unzip(file,exdir = tmp)
      # Load data
      country_spatial <- readOGR(dsn = tmp, layer = "ne_50m_admin_0_countries", encoding = "UTF-8")
      # Get country name
      cname <- country_spatial@data[["NAME"]]
      data <- sp::merge(country_spatial,mapdf2,by.x="NAME",by.y="production_countries",sort=FALSE,duplicateGeoms=FALSE,all.x=FALSE)
      labels <- sprintf(
        "<strong>%s</strong><br/>%s Total Number of Movies<br/>%g Suspecious Movies",
        data$NAME_EN, data$total, data$suspecious
      ) %>% lapply(htmltools::HTML)
      
      # Leaflet
      m <- leaflet(data) %>% addTiles() %>% setView(55,3.4,1.5)
      if (input$selectstat2=="Total Number of Movies"){
        pal <- colorNumeric("Greens", domain = mapdf2$total)
        m_total <- m %>% addPolygons(
          fillColor = ~pal(data$total),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        ) %>%
          addLegend(pal = pal, values = ~total, opacity = 0.7, title = "Total Number of Movies",
                    position = "bottomright")
        m_total
        
      } else {
        pal <- colorNumeric("Reds", domain = mapdf2$suspecious)
        m_sus <- m %>% addPolygons(
          fillColor = ~pal(data$suspecious),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        ) %>%
          addLegend(pal = pal, values = ~suspecious, opacity = 0.7, title = "Total Number of Movies",
                    position = "bottomright")
        m_sus
        
      }
    })
    
    # Other EDA
    output$trendmv <- renderPlotly({
      trend_mv <- mv_clean %>% select(original_title, released_year) %>% filter(released_year!=2017)
      trend_mv <- trend_mv %>% group_by(released_year) %>% summarise(count=n())
      trend_mv <- na.omit(trend_mv)
      trend_plot <- plot_ly(trend_mv, x=~released_year, y=~count)
      trace0 <- fitted(loess(count ~ as.numeric(released_year), data = trend_mv))
      trend_plot %<>%
        add_trace(y = ~count, mode="lines+markers") %>%
        add_trace(y=~trace0, mode="lines") %>%
        layout(title = "Year and Movies",showlegend = FALSE) %>%
        dplyr::filter(count == max(count)) %>%
        layout(annotations = list(x = ~released_year, y = ~count, text = "Peak", showarrow = T))
      ggplotly(trend_plot)
    })
    output$revbygen <- renderPlotly({
      vag <- mv_clean %>% select(id, vote_average, vote_count, popularity, revenue, budget) %>% 
        left_join(mv_gnr, by="id") %>% select(-total) %>% na.omit()
      b <- plot_ly(vag, x=~revenue, color = ~genres, type = "box") %>% layout(title="Revenue by Genre",showlegend = FALSE)
      ggplotly(b)
    })
    
}
 
# Run the application 
shinyApp(ui = ui, server = server)

