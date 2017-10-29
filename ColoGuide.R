library(shinydashboard)
library(leaflet)
library(plotly)
library(stringr)
library(xlsx)
library(ggmap)
library(stringi)
library(ade4)
library(clValid)
library(tm)
library(lsa)
library(tokenizers)
library(quanteda)
library(xlsx)


library(xlsx)

tfidf <- function(mat){
  tf <- mat
  id=function(col){sum(!col==0)}
  idf <- log(nrow(mat)/apply(mat, 2, id))
  tfidf <- mat
  for(word in names(idf)){tfidf[,word] <- tf[,word] * idf[word]}
  return(tfidf)
}
adduser <- function(){ 
  
  profilnew <- read.csv("charactere.csv")
  profiles <- read.xlsx2(file = "profilsneww.xlsx", sheetName = "Sheet1")
  profiles$X. <- NULL
  columns <- colnames(profiles)
  na.omit(profiles)
  
  #create vector for the new user
  newuser <- c(as.character(profilnew$x[1]))
  
  
  for (i in 1:nrow(profilnew)){
    for(j in 1:length(columns)){
      if(toupper(as.character(profilnew[i,2])) == toupper(columns[j])){
        
        newuser[j] <- 1
        break
      }}
  }
  
  for(i in 1:length(newuser)){
    if(is.na(newuser[i]))
      newuser[i] <- 0
  }
  
  s = length(newuser) + 1
  
  for(i in s:35){
    newuser <- c(newuser, "0")
  }
  if(length(newuser) == 34){
    newuser <- c(newuser, "0")
  }
  
  newuser <- c(newuser, as.character(profilnew$x[nrow(profilnew)]))
  
  
  
  #levels(profiles) <- c(levels(profiles), newuser)
  res <- cbind.data.frame(split(newuser, rep(1:36, times=length(newuser)/36)), stringsAsFactors=F)
  
  colnames(res) <- columns
  p <- rbind(profiles, res)
  
  write.xlsx(p, "profilsneww.xlsx")
  
}

similarity <- function(){
  profiles <- read.csv("annonceur.csv")
  AnnonceFinally <- read.csv("xAnnonceFinally.csv")
  AnnonceFinally2 <- read.csv("x2AnnonceFinally.csv")
  Annonces <- rbind(AnnonceFinally, AnnonceFinally2)
  profiles$X <- NULL
  newusers <- read.xlsx("profilsneww.xlsx", "Sheet1")
  newusers$NA. <- NULL
  new <- nrow(newusers)
  newuser <- newusers[new,]
  colnames(newuser) <- colnames(profiles)
  profilesAcomparer <- rbind(profiles, newuser)
  
  #####preparer la matrice binaire des caracteres!
  Characters <- profilesAcomparer[,2:35]
  
  ####Clustering according to characters!
  SoMiavcideal <- c()
  SoMiavcnonideal <- c()
  profilideal <- c(1,1,	0,	0,	0,	0,	0,	1,	1,	0,	0,	1,	1,	1,	0,	1,	0,	1,	0,	0,	0,1,1,1,1,1,1,1,1,1,1,1,1,1)
  profilnonideal <- c(0,	0,	1,	1,	1,	1,	1,	0,	0,	1,	1,	0,	0,	0,	1,	0,	1,	0,	1,	1,	1,0,0,0,0,0,0,0,0,0,0,0,0,0)
  
  for ( i in 1:nrow(Characters)){ 
    
    acideal <- rbind(profilideal, as.numeric(Characters[i,]))
    acnonideal <- rbind(profilnonideal, as.numeric(Characters[i,]))
    SoMiideal <- dist.binary(as.matrix(acideal), method = 2 , diag = FALSE, upper = FALSE)
    SoMinonideal <- dist.binary(as.matrix(acnonideal), method = 2 , diag = FALSE, upper = FALSE)
    
    SoMiavcideal <- c(SoMiavcideal, SoMiideal)
    SoMiavcnonideal <- c(SoMiavcnonideal,SoMinonideal )
    
  }
  
  #la matrice de la ML
  MatrixForML <- cbind(SoMiavcideal, SoMiavcnonideal)
  
  #Primar Clustering 
  clusters <- hclust(dist(MatrixForML))
  clusterCut <- cutree(clusters, 10)
  
  #bind profiles with their primar clusters
  profilesAcomparerdesc <- cbind(profilesAcomparer , clusterCut)
  
  
  #Extract only those having same cluster with new user
  
  descriptionDoc <- c()
  indexnew <- nrow(profilesAcomparerdesc)
  fin = nrow(profilesAcomparerdesc) - 1
  primarannounces <- Annonces[1,]
  similarprimar <- c()
  primarannounces <- c()
  for(i in 1:fin){
    if(profilesAcomparerdesc$clusterCut[i] == profilesAcomparerdesc$clusterCut[indexnew]){
      descriptionDoc <- c(descriptionDoc, as.character(profilesAcomparerdesc$description[i]))
      similarprimar <- c(similarprimar, as.character(profilesAcomparerdesc$nom[i]))
      primarannounces <- rbind (primarannounces, Annonces[i,])
    }
  }
  n <- nrow(primarannounces)
  primarannounces <- primarannounces[2:n,]
  
  
  
  
  #Add new user desc to its similar
  descriptionDoc <- c(descriptionDoc, as.character(profilesAcomparerdesc$descAnnonceurs.x[indexnew]))
  myCorpus <- Corpus(VectorSource(descriptionDoc))
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("french"))
  tdm <- TermDocumentMatrix(myCorpus, control = list(removePunctuation = TRUE, stopwords=stop))
  tdm1 <- as.matrix(tdm)
  
  
  tfidf1 <- tfidf(tdm1)
  lsa <- lsa( tfidf1, dims=dimcalc_share())
  
  #create doc similarities
  # MDS with LSA
  
  dist.mat.lsa <- dist(t(as.textmatrix(lsa)))  # compute distance matrix
  matrixsim <- as.matrix(dist.mat.lsa)
  #hierarchical with 11 clusters!
  clusterCut2 <- kmeans(dist(matrixsim),2)
  
  
  #####Extract only similar profiles with their names and announces
  f = length(descriptionDoc) - 1
  ClusterNewUser <- clusterCut2$cluster[length(clusterCut2)]
  finalannounces <- primarannounces[1,]
  finalprofiles <- c()
  for(i in  1:f){
    if(clusterCut2$cluster[i] == ClusterNewUser){
      finalannounces <- rbind(finalannounces,primarannounces[i,])
      finalprofiles <- c(finalprofiles,similarprimar[i])
    }
  }
  n1 <- nrow(finalannounces)
  finalannounces <- finalannounces[2:n1,]
  
  sim <- cbind(finalannounces, finalprofiles)
  
  return(sim)
  
}



ui <- dashboardPage(
  
  dashboardHeader(title= "ColoGuide",
                  
                  dropdownMenu(type = "messages")
  ),
  
  
  dashboardSidebar(
    
    
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    sidebarMenu(id="tags",
                menuItem("Accueil", tabName = "accueil", icon = icon("home")),
                menuItem("Inscription", tabName = "dashboardins", icon = icon("user-circle") ),
                menuItem("Deposer Annonce", tabName = "Deposerannonce", icon = icon("home") ),
                menuItem("Chercher ma maison", tabName = "dashboard1", icon = icon("bed") ),
                menuItem("Mieux connaitre ma region", tabName = "dashboard", icon = icon("map-marker"))
                
    )),
  
  dashboardBody( 
    tags$head(
      tags$style(HTML(".my_style_1{ 
                      background-image: url(https://www.izroom.com/blog/wp-content/uploads/2017/06/colocation-%C3%A9tranger.jpg);
                      
                      }"))),
    tabItems(
      
      tabItem(tabName = "accueil",list(tags$head(tags$style("body {background-color: black; }")),
                                       HTML('<img src="https://pbs.twimg.com/media/DGEFZPrXoAIWDad.jpg:large", width="100%"    
                                            style="float:right"/>'))),
      
      tabItem(tabName = "dashboardins", 
              
              fluidRow(
                tabBox(
                  title = tagList(shiny::icon("user-circle"), "Informations personnelles"),
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "250px",
                  tabPanel("Je suis", textInput("Nom", "Pseudo"), textAreaInput("descriptionperso","Décrivez vous...", value = "Parlez de vos centres d’intérêt et de votre colocation idéale")
                  ),
                  tabPanel("Je parle", checkboxGroupInput("Langages", "Cochez les langues que vous parlez: ",
                                                          c("Arabe" = "Ar","Anglais"="An" , "Français"="Fr", "Espagnol"="Es", "Italien" = "It", "Allemand"="Al" ,"Chinois" = "Ch")))
                ),
                tabBox(title = tagList(shiny::icon("child"), "Personnalité"),
                       side = "right", height = "250px",
                       tabPanel("Je suis plutot", checkboxGroupInput("comportement", "Cochez les situations où vous vous voyez le plus:",
                                                                     c("J’aime prendre en mains les situations et les événements. "="Responsable","Je tiens mes promesses."="honnete", 
                                                                       "Je panique rapidement."="Peureux","Je crois que les gens ont fondamentalement de bonnes intentions."="TropGentil",
                                                                       "J’oublie souvent de remettre les choses à la bonne place."="MalOrganise","Je dis la vérité meme si elle est blessante."= "direct",
                                                                       "Je fais plus que ce que l’on attend de moi."="Aidant", "J’insulte les gens quand ils le meritent."="Vulgaire",
                                                                       "Je reste calme sous la pression." ="Calme", "je déteste suivre les régles. je déteste qu'on me donne des instructions. Meme gentillement" = "Tetu",
                                                                       "je suis les régles" = "Discipline", "je parle trop." ="Bavard", "je pleure rapidement." = "Sensible",
                                                                       "Gentil avec les gens juste pour garder une bonne réputation." ="Hypocrite")
                       )),
                       tabPanel("Je suis plutot",checkboxGroupInput("comportement", "Cochez les situations où vous vous voyez le plus: ",
                                                                    c("Je me fais facilement des amis." ="Sociable","Je fais confiance aux autres." ="Optimiste" ,"Je me fâche facilement."="nerveux",
                                                                      "J’apprécie beaucoup les grandes réceptions et réunions. "="Ceremonieux", "Il m'arrivait d’utiliser et manipuler les autres pour arriver à mes fins.Mais ça n'a pa été très méchant."="Manipulateur",
                                                                      "Je n’aime pas le désordre. J’aime ranger."="organise", "Je me sens souvent triste."="pessimiste") 
                       )),
                       tabPanel("Habitudes", checkboxGroupInput("Habits", "Cochez vos habitudes: ",
                                                                c("je me couche tot et me léve tot" = "dormir", "je mange beaucoup ailleurs" = "manger",
                                                                  "Je joue avec un instrument musical" = "music", "Je sors très souvent"="fetard",
                                                                  "J'invite souvent mes amis chez moi" = "amis", "Je prèfére qu'on partage les repas"="cuisine")))
                       
                )),
              actionButton("action", label = "Créer Profil")),
      
      
      
      tabItem(tabName = "Deposerannonce",
              textInput("TitleAnnonce","Titre annonce"),
              
              textInput("price","Prix"),
              selectInput("Ville", "Ville", c("Paris", "Seine-et-Marne","Yvelines",
                                              "Essone", "Hauts-de-Seine", "Seine-Saint-Denis",
                                              "Val-d'Oise")),
              textAreaInput("description","Description"),
              actionButton("action", "Submit") ),
      
      tabItem(tabName = "dashboard1",
              box(title = "Mes criteres", background = "navy", solidHeader = TRUE,
                  selectInput("BudgetMax","BudgetMax en Euro", c( "400", "500", "600",
                                                                  "700", "800", "900", "1000", "1200")),
                  selectInput("Ville","Ville", c("Paris", "Essone"),
                              actionButton("Submit", "Submit")            )
                  
                  
                  
                  
              ),
              
              box(width= 12, 
                  tableOutput("contents"),
                  tableOutput("annonce"))
      ),
      
      tabItem(tabName = "dashboard",
              box(width = 12, height = NULL,
                  infoBoxOutput("Population"),
                  infoBoxOutput('infoBox1'),
                  infoBoxOutput("Security"),
                  infoBoxOutput("Transport"),
                  infoBoxOutput("Services"),
                  infoBoxOutput("Education"),
                  infoBoxOutput("Shops"),
                  infoBoxOutput("progressBox"),
                  infoBoxOutput("Entertainement"),
                  infoBoxOutput("Culture")),
              box(leafletOutput("FranceMap")),
              box(selectInput("Location", "Location", c("Paris 19e ARRON 75019", "Paris 10e ARRON 75010","Paris 11e ARRON 75011",
                                                        "Paris 12e ARRON 75012", "Paris 13e ARRON 75013", "Paris 14e ARRON 75014",
                                                        "Paris 15e ARRON 75015", "Paris 16e ARRON 75016","SAINT DENIS 93200","PARIS 9e ARRON 75009",
                                                        "MEAUX 77100", "MELUN 77000.", "FONTAINEBLEAU 77300", "BUSSY SAINT MARTIN 77600",
                                                        "CHELLES 77500", "CHAMPS SUR MARNE 77420", "PROVINS 77160", "COULOMMIERS 77120","ASNIERES SUR SEINE 92600",
                                                        "ANTONY 92160", "BAGNEUX 92220", "BOULOGNE BILLANCOURT 92100" ,"BOURG LA REINE 92340",
                                                        "CLAMART 92140","GENNEVILLIERS 92230", "NANTERRE 92000" , "NEUILLY SUR SEINE 92200" ))),
              box(plotlyOutput("actualities"))
      )
                                       )
      )
  )



server <- function(input, output, session) { 
  
  data <- read.xlsx("organizes.xlsx", "Sheet1")
  rssfinale <- read.csv("rssfinale.csv")
  
  
  observeEvent(input$inscri, {
    updateTabItems(session, "tags","dashboardins")
    
  })
  
  
  
  output$annonce <- renderTable({
    Titre <- c()
    Description <- c()
    Addresse <- c()
    Prix <- c()
    Name <- c()
    
    sim <- similarity()
    sim <- na.omit(sim)
    if(input$BudgetMax == 400){ 
      
      for(i in 1:nrow(sim)){ 
        if(as.character(sim$Prix[i]) <= "400"){
          if(nchar(as.character(sim$Prix[i])) == 3){ 
            Titre[i] <- as.character(sim$TitlesFinal[i])
            Description[i] <- as.character(sim$DescFinal[i])
            Addresse[i] <- as.character(sim$Locations[i])
            Prix[i] <- as.character(sim$Prix[i])
            Name[i] <- as.character(sim$finalprofiles[i])
          }}}}
    if(input$BudgetMax == 500){ 
      
      for(i in 1:nrow(sim)){ 
        if(as.character(sim$Prix[i]) <= "500"){
          if(nchar(as.character(sim$Prix[i])) == 3){ 
            Titre[i] <- as.character(sim$TitlesFinal[i])
            Description[i] <- as.character(sim$DescFinal[i])
            Addresse[i] <- as.character(sim$Locations[i])
            Prix[i] <- as.character(sim$Prix[i])
            Name[i] <- as.character(sim$finalprofiles[i])
          }}}}
    if(input$BudgetMax == 600){ 
      
      for(i in 1:nrow(sim)){ 
        if(as.character(sim$Prix[i]) <= "600"){
          if(nchar(as.character(sim$Prix[i])) == 3){ 
            Titre[i] <- as.character(sim$TitlesFinal[i])
            Description[i] <- as.character(sim$DescFinal[i])
            Addresse[i] <- as.character(sim$Locations[i])
            Prix[i] <- as.character(sim$Prix[i])
            Name[i] <- as.character(sim$finalprofiles[i])
            
          }}}}
    if(input$BudgetMax == 700){ 
      
      for(i in 1:nrow(sim)){ 
        if(as.character(sim$Prix[i]) <= "700"){
          if(nchar(as.character(sim$Prix[i])) == 3){ 
            Titre[i] <- as.character(sim$TitlesFinal[i])
            Description[i] <- as.character(sim$DescFinal[i])
            Addresse[i] <- as.character(sim$Locations[i])
            Prix[i] <- as.character(sim$Prix[i])
            Name[i] <- as.character(sim$finalprofiles[i])
          }}}}
    if(input$BudgetMax == 800){ 
      
      for(i in 1:nrow(sim)){ 
        if(as.character(sim$Prix[i]) <= "800"){
          if(nchar(as.character(sim$Prix[i])) == 3){ 
            Titre[i] <- as.character(sim$TitlesFinal[i])
            Description[i] <- as.character(sim$DescFinal[i])
            Addresse[i] <- as.character(sim$Locations[i])
            Prix[i] <- as.character(sim$Prix[i])
            Name[i] <- as.character(sim$finalprofiles[i])
          }}}}
    if(input$BudgetMax == 900){ 
      
      for(i in 1:nrow(sim)){ 
        if(as.character(sim$Prix[i]) <= "900"){
          if(nchar(as.character(sim$Prix[i])) == 3){ 
            Titre[i] <- as.character(sim$TitlesFinal[i])
            Description[i] <- as.character(sim$DescFinal[i])
            Addresse[i] <- as.character(sim$Locations[i])
            Prix[i] <- as.character(sim$Prix[i])
            Name[i] <- as.character(sim$finalprofiles[i])
          }}}}
    if(input$BudgetMax == 1000){ 
      
      for(i in 1:nrow(sim)){ 
        if(as.character(sim$Prix[i]) <= "1.000"){
          if(nchar(as.character(sim$Prix[i])) <= 5 ){ 
            Titre[i] <- as.character(sim$TitlesFinal[i])
            Description[i] <- as.character(sim$DescFinal[i])
            Addresse[i] <- as.character(sim$Locations[i])
            Prix[i] <- as.character(sim$Prix[i])
            Name[i] <- as.character(sim$finalprofiles[i])
          }}}}
    if(input$BudgetMax == 1200){ 
      
      for(i in 1:nrow(sim)){ 
        if(as.character(sim$Prix[i]) <= "1.200"){
          if(nchar(as.character(sim$Prix[i])) <= 5 ){ 
            Titre[i] <- as.character(sim$TitlesFinal[i])
            Description[i] <- as.character(sim$DescFinal[i])
            Addresse[i] <- as.character(sim$Locations[i])
            Prix[i] <- as.character(sim$Prix[i])
            Name[i] <- as.character(sim$finalprofiles[i])
          }}}}
    
    result <- cbind(Titre,Description,Addresse,Prix,Name)
    result <- na.omit(result)
    result
  })
  
  
  
  observe({
    if(input$action > 0){
      valoress<-renderText({
        input$langue
      })
      valores<-renderText({
        input$comportement
      })
      valoressC<-renderText({
        input$comp
      })
      valoresN<-renderText({
        input$Nom
      })
      
      valoresdesc<-renderText({
        input$descriptionperso
      })
      
      
      
      data<-unlist(strsplit(paste(valoresN(),valores(),valoressC(),valoress()),split=" "))
      desc <-  valoresdesc()
      profil <- c(data, desc)
      write.csv(profil, "charactere.csv")
      adduser()
      updateTabsetPanel(session,"tags",selected = "dashboard1")
      
    }
  })
  
  output$FranceMap <- renderLeaflet({
    x <- input$Location
    if (x == "Paris 19e ARRON 75019") {
      as <- geocode("Paris 19")
      m19 <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=as$lon, lat=as$lat)
      
    }
    if (x == "Paris 10e ARRON 75010") {
      as <- geocode("Paris 10")
      m19 <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=as$lon, lat=as$lat)
      
    }
    if (x == "Paris 11e ARRON 75011") {
      as <- geocode("Paris 11")
      m19 <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=as$lon, lat=as$lat)
      
    }
    if (x == "Paris 12e ARRON 75012") {
      as <- geocode("Paris 12")
      m19 <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=as$lon, lat=as$lat)
      
    }
    if (x == "Paris 13e ARRON 75013") {
      as <- geocode("Paris 13")
      m19 <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=as$lon, lat=as$lat)
      
    }
    if (x == "Paris 14e ARRON 75014") {
      as <- geocode("Paris 14")
      m19 <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=as$lon, lat=as$lat)
      
    }
    if (x == "Paris 15e ARRON 75015") {
      as <- geocode("Paris 15")
      m19 <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=as$lon, lat=as$lat)
      
    }
    if (x == "Paris 16e ARRON 75016") {
      as <- geocode("Paris 16")
      m19 <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=as$lon, lat=as$lat)
      
    }
    m19
  })
  output$Education <- renderInfoBox({
    x <- input$Location
    color <- 'green'
    if (x == "Paris 19e ARRON 75019") value <- paste(data[7,2] , "10", sep="/"); v <- as.numeric(data$PARIS.19E.ARRONDISSEMENT..75019.[7])
    if (x == "Paris 10e ARRON 75010") value <- paste(data$PARIS.10E.ARRONDISSEMENT..75010.[7], "10", sep="/"); v <- as.numeric(data$PARIS.10E.ARRONDISSEMENT..75019.[7])
    if (x == "Paris 11e ARRON 75011") value <- paste(data$PARIS.11E.ARRONDISSEMENT..75011..[7], "10", sep="/"); v <- as.numeric(data$PARIS.11E.ARRONDISSEMENT..75011..[7])
    if (x == "Paris 15e ARRON 75015") value <- paste(data$PARIS.15E.ARRONDISSEMENT..75015.[7], "10", sep="/"); v <- as.numeric(data$PARIS.15E.ARRONDISSEMENT..75015.[7])
    if (x == "Paris 16e ARRON 75016") value <- paste(data$PARIS.16E.ARRONDISSEMENT..75016.[7], "10", sep="/"); v <- as.numeric(data$PARIS.16E.ARRONDISSEMENT..75016.[7])
    if (x == "Paris 12e ARRON 75012") value <- paste(data$X.PARIS.12E.ARRONDISSEMENT..75012.[7], "10", sep="/"); v <- as.numeric(data$X.PARIS.12E.ARRONDISSEMENT..75012.[7])
    if (x == "Paris 13e ARRON 75013") value <- paste(data[7,8], "10", sep="/"); v <- as.numeric(data$X.PARIS.13E.ARRONDISSEMENT..75013.[7])
    if (x == "Paris 14e ARRON 75014") value <- paste(data$X.PARIS.14E.ARRONDISSEMENT..75014.[7], "10", sep="/"); v <- as.numeric(data$X.PARIS.14E.ARRONDISSEMENT..75014.[7])
    if (v < 5) color <- "red"
    
    infoBox(value = value, title = 'Enseignement', color = color,  icon = icon("mortar-board"))
    
  })
  # 
  output$infoBox1 <- renderInfoBox({
    x <- input$Location
    color <- 'green'
    if (x == "Paris 19e ARRON 75019") value <- paste(data[4,2] , "10", sep="/"); v <- as.numeric(data$PARIS.19E.ARRONDISSEMENT..75019.[4])
    if (x == "Paris 10e ARRON 75010") value <- paste(data$PARIS.10E.ARRONDISSEMENT..75010.[4], "10", sep="/"); v <- as.numeric(data$PARIS.10E.ARRONDISSEMENT..75019.[4])
    if (x == "Paris 11e ARRON 75011") value <- paste(data$PARIS.11E.ARRONDISSEMENT..75011..[4], "10", sep="/"); v <- as.numeric(data$PARIS.11E.ARRONDISSEMENT..75011..[4])
    if (x == "Paris 15e ARRON 75015") value <- paste(data$PARIS.15E.ARRONDISSEMENT..75015.[4], "10", sep="/"); v <- as.numeric(data$PARIS.15E.ARRONDISSEMENT..75015.[4])
    if (x == "Paris 16e ARRON 75016") value <- paste(data$PARIS.16E.ARRONDISSEMENT..75016.[4], "10", sep="/"); v <- as.numeric(data$PARIS.16E.ARRONDISSEMENT..75016.[4])
    if (x == "Paris 12e ARRON 75012") value <- paste(data$X.PARIS.12E.ARRONDISSEMENT..75012.[4], "10", sep="/"); v <- as.numeric(data$X.PARIS.12E.ARRONDISSEMENT..75012.[4])
    if (x == "Paris 13e ARRON 75013") value <- paste(data[4,8], "10", sep="/"); v <- as.numeric(data$X.PARIS.13E.ARRONDISSEMENT..75013.[4])
    if (x == "Paris 14e ARRON 75014") value <- paste(data$X.PARIS.14E.ARRONDISSEMENT..75014.[4], "10", sep="/"); v <- as.numeric(data$X.PARIS.14E.ARRONDISSEMENT..75014.[4])
    if (v < 5) color <- "red"
    
    infoBox(value = value, title = 'Sante', color = color, icon= icon("ambulance"))
    
  })
  
  output$Security <- renderInfoBox({
    x <- input$Location
    color <- 'green'
    if (x == "Paris 19e ARRON 75019") value <- paste(data[3,2] , "10", sep="/"); v <- as.numeric(data$PARIS.19E.ARRONDISSEMENT..75019.[3])
    if (x == "Paris 10e ARRON 75010") value <- paste(data$PARIS.10E.ARRONDISSEMENT..75010.[3], "10", sep="/"); v <- as.numeric(data$PARIS.10E.ARRONDISSEMENT..75019.[3])
    if (x == "Paris 11e ARRON 75011") value <- paste(data$PARIS.11E.ARRONDISSEMENT..75011..[3], "10", sep="/"); v <- as.numeric(data$PARIS.11E.ARRONDISSEMENT..75011..[3])
    if (x == "Paris 15e ARRON 75015") value <- paste(data$PARIS.15E.ARRONDISSEMENT..75015.[3], "10", sep="/"); v <- as.numeric(data$PARIS.15E.ARRONDISSEMENT..75015.[3])
    if (x == "Paris 16e ARRON 75016") value <- paste(data$PARIS.16E.ARRONDISSEMENT..75016.[3], "10", sep="/"); v <- as.numeric(data$PARIS.16E.ARRONDISSEMENT..75016.[3])
    if (x == "Paris 12e ARRON 75012") value <- paste(data$X.PARIS.12E.ARRONDISSEMENT..75012.[3], "10", sep="/"); v <- as.numeric(data$X.PARIS.12E.ARRONDISSEMENT..75012.[3])
    if (x == "Paris 13e ARRON 75013") value <- paste(data[3,8], "10", sep="/"); v <- as.numeric(data$X.PARIS.13E.ARRONDISSEMENT..75013.[3])
    if (x == "Paris 14e ARRON 75014") value <- paste(data$X.PARIS.14E.ARRONDISSEMENT..75014.[3], "10", sep="/"); v <- as.numeric(data$X.PARIS.14E.ARRONDISSEMENT..75014.[3])
    if (v < 5) color <- "red"
    
    infoBox(value = value, title = 'Securite', color = color, icon= icon("key"))
    
  })
  # 
  # 
  # 
  output$Population <- renderInfoBox({
    x <- input$Location
    
    if (x == "Paris 19e ARRON 75019") {value <- paste(data[1,2] , "10", sep="/"); v <- as.numeric(data$PARIS.19E.ARRONDISSEMENT..75019.[1])}
    if (x == "Paris 10e ARRON 75010") value <- paste(data$PARIS.10E.ARRONDISSEMENT..75010.[1], "10", sep="/"); v <- as.numeric(data$PARIS.10E.ARRONDISSEMENT..75019.[1])
    if (x == "Paris 11e ARRON 75011") value <- paste(data$PARIS.11E.ARRONDISSEMENT..75011..[1], "10", sep="/"); v <- as.numeric(data$PARIS.11E.ARRONDISSEMENT..75011..[1])
    if (x == "Paris 15e ARRON 75015") value <- paste(data$PARIS.15E.ARRONDISSEMENT..75015.[1], "10", sep="/"); v <- as.numeric(data$PARIS.15E.ARRONDISSEMENT..75015.[1])
    if (x == "Paris 16e ARRON 75016") value <- paste(data$PARIS.16E.ARRONDISSEMENT..75016.[1], "10", sep="/"); v <- as.numeric(data$PARIS.16E.ARRONDISSEMENT..75016.[1])
    if (x == "Paris 12e ARRON 75012") value <- paste(data$X.PARIS.12E.ARRONDISSEMENT..75012.[1], "10", sep="/"); v <- as.numeric(data$X.PARIS.12E.ARRONDISSEMENT..75012.[1])
    if (x == "Paris 13e ARRON 75013") value <- paste(data[1,8], "10", sep="/"); v <- as.numeric(data$X.PARIS.13E.ARRONDISSEMENT..75013.[1])
    if (x == "Paris 14e ARRON 75014") value <- paste(data$X.PARIS.14E.ARRONDISSEMENT..75014.[1], "10", sep="/"); v <- as.numeric(data$X.PARIS.14E.ARRONDISSEMENT..75014.[1])
    
    
    
    infoBox(value=value, title = 'Environnement', color= 'green', icon = icon("tree"))
    
    
  })
  
  output$Services<- renderInfoBox({
    data <- read.xlsx("organizes.xlsx", "Sheet1")
    x <- input$Location
    color <- 'green'
    if (x == "Paris 19e ARRON 75019") value <- paste(data[9,2] , "10", sep="/"); v <- data$PARIS.19E.ARRONDISSEMENT..75019.[9]
    if (x == "Paris 11e ARRON 75011") value <- paste(data$PARIS.11E.ARRONDISSEMENT..75011..[9], "10", sep="/"); v <- data$PARIS.11E.ARRONDISSEMENT..75019.[9]
    if (x == "Paris 10e ARRON 75010") value <- paste(data$PARIS.10E.ARRONDISSEMENT..75010.[9], "10", sep="/"); v <- data$PARIS.10E.ARRONDISSEMENT..75019.[9]
    if (x == "Paris 15e ARRON 75015") value <- paste(data$PARIS.15E.ARRONDISSEMENT..75015.[9], "10", sep="/"); v <- data$PARIS.15E.ARRONDISSEMENT..75019.[9]
    if (x == "Paris 16e ARRON 75016") value <- paste(data$PARIS.16E.ARRONDISSEMENT..75016.[9], "10", sep="/"); v <- data$PARIS.16E.ARRONDISSEMENT..75019.[9]
    if (x == "Paris 12e ARRON 75012") value <- paste(data$X.PARIS.12E.ARRONDISSEMENT..75012.[9], "10", sep="/"); v <- data$X.PARIS.12E.ARRONDISSEMENT..75012.[9]
    if (x == "Paris 13e ARRON 75013") value <- paste(data[9,8], "10", sep="/"); v <- data$X.PARIS.13E.ARRONDISSEMENT..75013.[9]
    if (x == "Paris 14e ARRON 75014") value <- paste(data$X.PARIS.14E.ARRONDISSEMENT..75014.[9], "10", sep="/"); v <- data$X.PARIS.14E.ARRONDISSEMENT..75014.[9]
    if (v < 5) color <- "red"
    
    infoBox(value=value, title = 'Qualite de Vie', color= color, icon = icon("eur"))
    
    
  })
  
  output$Shops <- renderInfoBox({
    x <- input$Location
    color <- 'green'
    if (x == "Paris 19e ARRON 75019") value <- paste(data[8,2] , "10", sep="/"); v <- as.numeric(data$PARIS.19E.ARRONDISSEMENT..75019.[8])
    if (x == "Paris 10e ARRON 75010") value <- paste(data$PARIS.10E.ARRONDISSEMENT..75010.[8], "10", sep="/"); v <- as.numeric(data$PARIS.10E.ARRONDISSEMENT..75019.[8])
    if (x == "Paris 11e ARRON 75011") value <- paste(data$PARIS.11E.ARRONDISSEMENT..75011..[8], "10", sep="/"); v <- as.numeric(data$PARIS.11E.ARRONDISSEMENT..75011..[8])
    if (x == "Paris 15e ARRON 75015") value <- paste(data$PARIS.15E.ARRONDISSEMENT..75015.[8], "10", sep="/"); v <- as.numeric(data$PARIS.15E.ARRONDISSEMENT..75015.[8])
    if (x == "Paris 16e ARRON 75016") value <- paste(data$PARIS.16E.ARRONDISSEMENT..75016.[8], "10", sep="/"); v <- as.numeric(data$PARIS.16E.ARRONDISSEMENT..75016.[8])
    if (x == "Paris 12e ARRON 75012") value <- paste(data$X.PARIS.12E.ARRONDISSEMENT..75012.[8], "10", sep="/"); v <- as.numeric(data$X.PARIS.12E.ARRONDISSEMENT..75012.[8])
    if (x == "Paris 13e ARRON 75013") value <- paste(data[8,8], "10", sep="/"); v <- as.numeric(data$X.PARIS.13E.ARRONDISSEMENT..75013.[8])
    if (x == "Paris 14e ARRON 75014") value <- paste(data$X.PARIS.14E.ARRONDISSEMENT..75014.[8], "10", sep="/"); v <- as.numeric(data$X.PARIS.14E.ARRONDISSEMENT..75014.[8])
    if (v < 5) color <- "red"
    
    infoBox(value = value, title = 'Commerces', color = color,  icon = icon("shopping-cart"))
    
  })
  output$Transport <- renderInfoBox({
    x <- input$Location
    color <- 'green'
    if (x == "Paris 19e ARRON 75019") value <- paste(data[2,2] , "10", sep="/"); v <- as.numeric(data$PARIS.19E.ARRONDISSEMENT..75019.[2])
    if (x == "Paris 10e ARRON 75010") value <- paste(data$PARIS.10E.ARRONDISSEMENT..75010.[2], "10", sep="/"); v <- as.numeric(data$PARIS.10E.ARRONDISSEMENT..75019.[2])
    if (x == "Paris 11e ARRON 75011") value <- paste(data$PARIS.11E.ARRONDISSEMENT..75011..[2], "10", sep="/"); v <- as.numeric(data$PARIS.11E.ARRONDISSEMENT..75011..[2])
    if (x == "Paris 15e ARRON 75015") value <- paste(data$PARIS.15E.ARRONDISSEMENT..75015.[2], "10", sep="/"); v <- as.numeric(data$PARIS.15E.ARRONDISSEMENT..75015.[2])
    if (x == "Paris 16e ARRON 75016") value <- paste(data$PARIS.16E.ARRONDISSEMENT..75016.[2], "10", sep="/"); v <- as.numeric(data$PARIS.16E.ARRONDISSEMENT..75016.[2])
    if (x == "Paris 12e ARRON 75012") value <- paste(data$X.PARIS.12E.ARRONDISSEMENT..75012.[2], "10", sep="/"); v <- as.numeric(data$X.PARIS.12E.ARRONDISSEMENT..75012.[2])
    if (x == "Paris 13e ARRON 75013") value <- paste(data[2,8], "10", sep="/"); v <- as.numeric(data$X.PARIS.13E.ARRONDISSEMENT..75013.[2])
    if (x == "Paris 14e ARRON 75014") value <- paste(data$X.PARIS.14E.ARRONDISSEMENT..75014.[2], "10", sep="/"); v <- as.numeric(data$X.PARIS.14E.ARRONDISSEMENT..75014.[2])
    if (v < 5) color <- "red"
    
    infoBox(value = value, title = 'Transports', color = color,  icon = icon("bus"))
    
  })
  
  output$Entertainement <- renderInfoBox({
    x <- input$Location
    color <- 'green'
    if (x == "Paris 19e ARRON 75019") value <- paste(data[5,2] , "10", sep="/"); v <- as.numeric(data$PARIS.19E.ARRONDISSEMENT..75019.[5])
    if (x == "Paris 10e ARRON 75010") value <- paste(data$PARIS.10E.ARRONDISSEMENT..75010.[5], "10", sep="/"); v <- as.numeric(data$PARIS.10E.ARRONDISSEMENT..75019.[5])
    if (x == "Paris 11e ARRON 75011") value <- paste(data$PARIS.11E.ARRONDISSEMENT..75011..[5], "10", sep="/"); v <- as.numeric(data$PARIS.11E.ARRONDISSEMENT..75011..[5])
    if (x == "Paris 15e ARRON 75015") value <- paste(data$PARIS.15E.ARRONDISSEMENT..75015.[5], "10", sep="/"); v <- as.numeric(data$PARIS.15E.ARRONDISSEMENT..75015.[5])
    if (x == "Paris 16e ARRON 75016") value <- paste(data$PARIS.16E.ARRONDISSEMENT..75016.[5], "10", sep="/"); v <- as.numeric(data$PARIS.16E.ARRONDISSEMENT..75016.[5])
    if (x == "Paris 12e ARRON 75012") value <- paste(data$X.PARIS.12E.ARRONDISSEMENT..75012.[5], "10", sep="/"); v <- as.numeric(data$X.PARIS.12E.ARRONDISSEMENT..75012.[5])
    if (x == "Paris 13e ARRON 75013") value <- paste(data[5,8], "10", sep="/"); v <- as.numeric(data$X.PARIS.13E.ARRONDISSEMENT..75013.[5])
    if (x == "Paris 14e ARRON 75014") value <- paste(data$X.PARIS.14E.ARRONDISSEMENT..75014.[5], "10", sep="/"); v <- as.numeric(data$X.PARIS.14E.ARRONDISSEMENT..75014.[5])
    if (v < 5) color <- "red"
    
    infoBox(value = value, title = 'Loisirs', color = color,  icon = icon("futbol-o"))
    
  })
  
  
  output$Culture <- renderInfoBox({
    x <- input$Location
    color <- 'green'
    if (x == "Paris 19e ARRON 75019") value <- paste(data[6,2] , "10", sep="/"); v <- as.numeric(data$PARIS.19E.ARRONDISSEMENT..75019.[6])
    if (x == "Paris 10e ARRON 75010") value <- paste(data$PARIS.10E.ARRONDISSEMENT..75010.[6], "10", sep="/"); v <- as.numeric(data$PARIS.10E.ARRONDISSEMENT..75019.[6])
    if (x == "Paris 11e ARRON 75011") value <- paste(data$PARIS.11E.ARRONDISSEMENT..75011..[6], "10", sep="/"); v <- as.numeric(data$PARIS.11E.ARRONDISSEMENT..75011..[6])
    if (x == "Paris 15e ARRON 75015") value <- paste(data$PARIS.15E.ARRONDISSEMENT..75015.[6], "10", sep="/"); v <- as.numeric(data$PARIS.15E.ARRONDISSEMENT..75015.[6])
    if (x == "Paris 16e ARRON 75016") value <- paste(data$PARIS.16E.ARRONDISSEMENT..75016.[6], "10", sep="/"); v <- as.numeric(data$PARIS.16E.ARRONDISSEMENT..75016.[6])
    if (x == "Paris 12e ARRON 75012") value <- paste(data$X.PARIS.12E.ARRONDISSEMENT..75012.[6], "10", sep="/"); v <- as.numeric(data$X.PARIS.12E.ARRONDISSEMENT..75012.[6])
    if (x == "Paris 13e ARRON 75013") value <- paste(data[6,8], "10", sep="/"); v <- as.numeric(data$X.PARIS.13E.ARRONDISSEMENT..75013.[6])
    if (x == "Paris 14e ARRON 75014") value <- paste(data$X.PARIS.14E.ARRONDISSEMENT..75014.[6], "10", sep="/"); v <- as.numeric(data$X.PARIS.14E.ARRONDISSEMENT..75014.[6])
    if (v < 5) color <- "red"
    
    infoBox(value = value, title = 'Culture', color = color,  icon = icon("film"))
    
  })
  
  output$actualities <- renderPlotly({
    p <- plot_ly(rssfinale, labels = ~polarity_label, values = ~id, type = 'pie') %>%
      layout(title = 'Sécurité selon les actualités',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p
    
  })
}


shinyApp(ui, server)
