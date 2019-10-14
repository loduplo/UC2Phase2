## app.R ## 
## USECASE2 : Smart Charging phase 2 - 15/10/2019
library("shiny")
library("shinydashboard")
library("leaflet")
library("lubridate")
library("dplyr")
library("zoo")
library("ggplot2")
source("global.R")
#palette de couleurs
library("RColorBrewer")
library("shinyWidgets") # sliderTextInput
library("ggrepel") # timeline
library("ggthemes")    # calendrier has a clean theme for ggplot2
period <- as.factor(c("0h-2h","2h-4h","4h-6h","6h-8h","8h-10h","10h-12h","12h-14h","14h-16h","16h-18h","18h-20h","20h-22h","22h-24h"))

ui <- dashboardPage(
  dashboardHeader(title = "UC2: Bornes IRVE"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Carte interactive", tabName = "carto", icon = icon("map")),
      
      menuItem("Recharges et signal", tabName = "histofacet", icon = icon("th")),
      
      menuItem("Signal et charge", tabName = "charge", icon = icon("dashboard")),
      selectInput(inputId = "ville",
                  label = paste("Choisissez la ville (",nbVilles,")",""),
                  choice = listVilles)
      # selectInput(inputId = "station",
      #             label = paste("Choisissez la station (",nbstations,")",""),
      #             choice = listStations)
    )
  ),
  dashboardBody(
    
    tabItems(
      # First tab content
      tabItem(tabName = "carto",
              fluidRow(
                box(title="Carte des bornes IRVE avec le signal", width=12,status="primary",solidHeader=TRUE,
                  leafletOutput("map",height=500),
                  column(4, sliderTextInput(
                    inputId = "periodtext", 
                    label = "Choisissez la periode :", 
                    grid = TRUE, 
                    width=1200,
                    force_edges = TRUE,
                    choices = c("0h-2h","2h-4h","4h-6h","6h-8h","8h-10h","10h-12h","12h-14h","14h-16h","16h-18h","18h-20h","20h-22h","22h-24h"),
                    selected = "12h-14h",
                    animate = animationOptions(interval = 2500)
                  )),
                  #tableOutput(outputId = "table"),
                  column(12,uiOutput("locationid"))
                ),
                box(title="Time line du signal du jour pour la station choisie",width=12,status="primary",solidHeader=TRUE,
                    plotOutput("jourtimeline")
                ),
                box(title="Time line du signal historique enedis pour la station choisie",width=12,height=460,status="primary",solidHeader=TRUE,
                    plotOutput("histotimeline")
                ),
                box(title="Signal sur juin - juillet - aout 2019",width=12,height=500,status="primary",solidHeader=TRUE,
                    plotOutput("histocalendar")
                ),
                box(title="Signal sur septembre - octobre - novembre 2019",width=12,height=500,status="primary",solidHeader=TRUE,
                    plotOutput("histocalendar2")
                ),

                box(title="Calendrier janvier 2018 - mars 2019 par periode",width=12,height=500,status="primary",solidHeader=TRUE,
                    plotOutput("histocalendartrans")
                ),
                box(title="Transactions et Signal sur juin - juillet - aout 2019",width=12,height=500,status="primary",solidHeader=TRUE,
                    plotOutput("histocalendarquotidien")
                )
              )#fluidRow
      ),
      # Tab Content Historique facet :
      tabItem(tabName = "histofacet",
              fluidRow(
                column(3,selectInput(inputId = "varhist18",
                                     label = h4("choix variable"),
                                     choice = c("nbTransaction","Consommationkwh","DureeChargemin"))
                ),
                column(3,selectInput(inputId = "nbBornes",
                                     label = h4("choix top20 / toutes"),
                                     choice = c("top20","toutes"))
                ),
                box(title="Historique Signal/transactions Hiver-2018 - Hiver 2019",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("ggplotfacet2",height = 2000)
                ),
                box(title="Calendrier Historique Signal/transactions Hiver-2018 - Hiver 2019",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("ggplotfacetCal",height = 2000)
                )
              )#fluidRow
      ),
      # Tab Content Historique facet :
      tabItem(tabName = "charge",
              box(title="Carte des bornes en cours de charge", width=12,status="primary",solidHeader=TRUE,
                  leafletOutput("mapcharge",height=500))

              )
      )
  )
)

server <- function(input, output, session) {

  # Les bornes
  data <- reactive({
    x <- borneswithsignal
  })
  
  # le signal enedis
  enedisdata <- reactive({
    t <- signaldata
  })
  # 30/07/2019 : le signal enedis historique
  enedishisto <- reactive({
    t <- signalhisto
  })
  # 05/08/2019 : le signal enedis historique concat des jours
  enedisallhisto <- reactive({
    t <- signalallhisto
  })
  
  # 13/09/2019 : le signal historique par saison + transactions par mois
  saisonallhisto <- reactive({
    t <- transactionsBornehistoPeriodeAll
  })
  # 13/09/2019 : le signal quotidien par jour + transactions par jour
  enedisquotidien <- reactive({
    t <- transactionsSignalQuotidien
  })
  ############################################################################################################
  # VISU contextuelles de la cartographie
  # CLIC sur la carte => PDC
  # ATTENTION
  # input$<map_name>_marker_click. The map called map, so observing input$map_marker_click
  # id is layerId=~IDC from leaflet map - beware the same in proxy map
  observeEvent(input$map_marker_click, {

    click <- input$map_marker_click
    
    ## filter the data and output into a table
    output$table <- renderTable({
      data()[data()$IDC == click$id, ]
    })
  })
  # CLIC sur la carte => PDC
  #### 6/05 : clic sur la carte => recuperation de la Borne PDC
  id <- reactive({
    validate(
      need(!is.null(input$map_marker_click), "Please select a location from the map above")
    )
    input$map_marker_click$id
  })
  selectborne <- reactive({
    validate(
      need(!is.null(input$map_marker_click), "Please select a location from the map above")
    )
    # champ Station correspondant a IDC clique dans le fichier de carto des stations
    lerow <- filter(data(), IDC==id())
    return (lerow)
  })
  output$locationid <- renderUI({
    h4(paste("Vous avez choisi l\' IDC:", id(), ",  la station :", selectborne()$Station, "de la ville :", selectborne()$Commune))
  })

############################################################################################################
# TIMELINE DU SIGNAL DU JOUR
# 31/07/2019
  output$jourtimeline <- renderPlot({
  if (!is.null(id()))
  {
    # On filtre les 12 valeurs de signal de la journee (voir si on recherche une journee particuliere)
    signaljour <- select(filter(enedisdata(),IDC==id()),c("h02","h24","h46","h68","h810","h1012","h1214","h1416","h1618","h1820","h2022","h2224"))
    if (nrow(signaljour)==0)
    {
      sigJ = c(rep("Inconnu",12))
    } else {
      sigJ = signaljour %>% slice(1) %>% unlist(use.names = FALSE)
    }
    signalStation <- data.frame(
      date=(1:12),
      texte=c(0,1,0,0,1,0,0,1,0,0,1,0),
      trait=c(-1,0,1,-1,0,1,-1,0,1,-1,0,1),
      station=rep("station",12),
      libjour=c(rep("Nuit",3),rep("Matin",3),rep("Apres-midi",3),rep("Soir",3)),
      #jour=c(rep("J",12)),
      periode=c("0H-2H","2h-4h","4h-6h","6h-8h","8H-10h","10h-12h","12h-14h","14h-16h","16h-18h","18h-20h","20h-22h","22h-24h"),
      signal=as.character(sigJ))
    
    signalStation$periode <- factor(signalStation$periode,levels=c("0H-2H","2h-4h","4h-6h","6h-8h","8H-10h","10h-12h","12h-14h","14h-16h","16h-18h","18h-20h","20h-22h","22h-24h"))
    
    if (nrow(signaljour)==0)
    {
      signal_levels <- c("Inconnu")
      signal_colors <- c("darkgray")
      signalStation$signal <- factor(signalStation$signal,levels=c("Inconnu"))
    } else {
      signal_levels <- c("Energie neutre","local RTE", "Energie locale")
      signal_colors <- c("gray96", "#62A667","green")
      signalStation$signal <- factor(signalStation$signal,levels=c("Blanc","Vert","Vert2"))
    }
    #levels(signalStation$signal)
    
    # Affichage
    #theTitle = ("Timeline sur un jour")
    theTitle=paste(enedisdata()$date[1],": vous avez choisi l\'IDC", id(),",  la station :", selectborne()$Station, "de la ville :", selectborne()$Commune)
    
    timeline_station<-ggplot(signalStation,aes(x=date,y=0, col=signal, label=signal))+
      labs(col="signal",title = theTitle, size=5)+theme_classic()+
      scale_color_manual(values=signal_colors, labels=signal_levels, drop = FALSE)+
      # Plot horizontal black line for timeline : NON geom_hline(yintercept=0,color = "black", size=2) +
      # ligne horizontale avec taille de 30 => inutile
      geom_segment(aes(x=0,y=0,xend=13,yend=0),size=2, color="gray")+
      # Plot scatter points at zero and date
      geom_point(aes(y=0), size=20, shape=15)+#size=34 - shape=15
      # add vertical line for each saison
      geom_segment(data=signalStation[signalStation$trait == 1,],aes(y=-1.5,yend=2,x=date+0.5,xend=date+0.5), color='darkgray', size=1)+
      geom_segment(data=signalStation[signalStation$trait == -1,],aes(y=-1.5,yend=2,x=date-0.5,xend=date-0.5), color='darkgray', size=1)+
      # add text SAISON : Hiver 2018 - Printemps 2018 - Ete 2018- Automne 2018 - Hiver 2019
      ggrepel::geom_label_repel(data=signalStation[signalStation$texte == 1,],aes(label=libjour,y=2,x=date,fontface=2),size=8, fill="gray",color="white")+
      # TEXTE au milieu y=0 (au lieu de y=-1) Show text for each part of day
      geom_text(data=signalStation,aes(label=periode,y=0),size = 4,color="black")+
      # Show day
      #ggrepel::geom_label_repel(data=signalStation[signalStation$date == 1,],aes(label=jour,y=3,x=date,fontface=2),size=8, color="darkGray")+
      
      #geom_text(data=signalStation,aes(label=jour,y=1),size = 4, color="black")+
      # Don't show axes, appropriately position legend
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom",
            legend.title=element_text(size=20, color="white", face="bold"),#color="blue",face="bold"
            legend.text=element_text(size=15),
            legend.background = element_rect(fill="gray",
                                             size=0.5, linetype="solid")
      )
    
    visu<-timeline_station + coord_fixed(ratio = 0.8)
    return (visu)
  }
  })
  ############################################################################################################
  # TIMELINE DU SIGNAL HISTORIQUE
  # 30/07/2019
  
  output$histotimeline <- renderPlot({
    if (!is.null(id()))
    {
      # pour un id donne on filtre le signal historique
      # Hiver 2018
      signalH18 <- select(filter(enedishisto(),IDC==id()&date=="Hiver18"),c("h68","h810","h1012","h1214","h1416","h1618"))
      # Printemps 2018
      signalP18 <- select(filter(enedishisto(),IDC==id()&date=="Spring18"),c("h68","h810","h1012","h1214","h1416","h1618"))
      # Ete 2018
      signalE18 <- select(filter(enedishisto(),IDC==id()&date=="Ete18"),c("h68","h810","h1012","h1214","h1416","h1618"))
      # Automne 2018
      signalA18 <- select(filter(enedishisto(),IDC==id()&date=="Automn18"),c("h68","h810","h1012","h1214","h1416","h1618"))
      # Hiver 2019
      signalH19 <- select(filter(enedishisto(),IDC==id()&date=="Hiver19"),c("h68","h810","h1012","h1214","h1416","h1618"))
      
      if (nrow(signalH18)==0)
      {
        sigH18 = c(rep("Inconnu",6))
      } else {
        sigH18 = signalH18 %>% slice(1) %>% unlist(use.names = FALSE)
      }
      if (nrow(signalP18)==0)
      {
        sigP18 = c(rep("Inconnu",6))
      } else {
        sigP18 = signalP18 %>% slice(1) %>% unlist(use.names = FALSE)
      }
      if (nrow(signalE18)==0)
      {
        sigE18 = c(rep("Inconnu",6))
      } else {
        sigE18 = signalE18 %>% slice(1) %>% unlist(use.names = FALSE)
      }
      if (nrow(signalA18)==0)
      {
        sigA18 = c(rep("Inconnu",6))
      } else {
        sigA18 = signalA18 %>% slice(1) %>% unlist(use.names = FALSE)
      }
      if (nrow(signalH19)==0)
      {
        sigH19 = c(rep("Inconnu",6))
      } else {
        sigH19 = signalH19 %>% slice(1) %>% unlist(use.names = FALSE)
      }
      
      # on cree le dataframe correspondant
      signalenedis <- data.frame(
        date=(1:30),
        texte=c(0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0),
        trait=c(-1,0,0,0,0,1,-1,0,0,0,0,1,-1,0,0,0,0,1,-1,0,0,0,0,1,-1,0,0,0,0,1),
        libjour=c(rep("Hiver 2018",6),rep("Printemps 2018",6),rep("Ete 2018",6),rep("Automne 2018",6),rep("Hiver 2019",6)),
        jour=c(rep("H",6),rep("P",6),rep("E",6),rep("A",6),rep("H",6)),
        periode=c(rep(c("6h-8h","8H-10h","10h-12h","12h-14h","14h-16h","16h-18h"),5)),
        signal=c(as.character(sigH18),as.character(sigP18),as.character(sigE18),as.character(sigA18),as.character(sigH19)))
      
      if (nrow(signalH18)==0)
      {
        signal_levels <- c("Energie neutre", "local RTE","Energie locale","Inconnu")
        signal_colors <- c("gray96", "#62A667","green", "darkgray")
        signalenedis$signal <- factor(signalenedis$signal,levels=c("Blanc","Vert","Vert2","Inconnu"))
      } else {
        signal_levels <- c("Energie neutre","local RTE", "Energie locale")
        signal_colors <- c("gray96", "#62A667","green")
        signalenedis$signal <- factor(signalenedis$signal,levels=c("Blanc","Vert","Vert2"))
      }
      #levels(signalenedis$signal)
      signalenedis$periode <- factor(signalenedis$periode,levels=c("6h-8h","8H-10h","10h-12h","12h-14h","14h-16h","16h-18h"))
      
      # Affichage
      theTitle=paste("Vous avez choisi l\'IDC", id(),",  la station :", selectborne()$Station, "de la ville :", selectborne()$Commune)
      
      timeline_station<-ggplot(signalenedis,aes(x=date,y=0, col=signal, label=signal))+
        labs(col="signal",title = theTitle, size=5)+theme_classic()+
        scale_color_manual(values=signal_colors, labels=signal_levels, drop = FALSE)+
        # Plot horizontal black line for timeline : NON geom_hline(yintercept=0,color = "black", size=2) +
        # ligne horizontale avec taille de 30 => inutile
        # geom_segment(aes(x=-1,y=0,xend=30,yend=0),size=2, color="blue")+
        # Plot scatter points at zero and date
        geom_point(aes(y=0), size=19, shape=16)+#size=34 - shape=15
        # add vertical line for each saison
        geom_segment(data=signalenedis[signalenedis$trait == 1,],aes(y=-1.5,yend=2,x=date+0.5,xend=date+0.5), color='darkgray', size=1)+
        geom_segment(data=signalenedis[signalenedis$trait == -1,],aes(y=-1.5,yend=2,x=date-0.5,xend=date-0.5), color='darkgray', size=1)+
        # add text SAISON : Hiver 2018 - Printemps 2018 - Ete 2018- Automne 2018 - Hiver 2019
        ggrepel::geom_label_repel(data=signalenedis[signalenedis$texte == 1,],aes(label=libjour,y=2,x=date+0.5,fontface=2),size=8, fill="gray",color="white")+      
        # TEXTE au milieu y=0 (au lieu de y=-1) Show text for each part of day
        geom_text(data=signalenedis,aes(label=periode,y=0),size = 4,color="black")+
        # Show day
        #geom_text(data=signalenedis,aes(label=date,y=1),size = 4, color="black")+
        # Don't show axes, appropriately position legend
        theme(axis.line.y=element_blank(),
              axis.text.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.x =element_blank(),
              axis.ticks.x =element_blank(),
              axis.line.x =element_blank(),
              legend.position = "bottom",
              legend.title=element_text(size=20, color="white", face="bold"),#color="blue",face="bold"
              legend.text=element_text(size=15),
              legend.background = element_rect(fill="gray", 
                                               size=0.5, linetype="solid")
        )
      
      visu<-timeline_station + coord_fixed(ratio = 0.8)
      return (visu)
    }
  })
  ############################################################################################################
  # CALENDRIER SUR 3 MOIS DU SIGNAL QUOTIDIEN
  # 13/09/2019
  
  output$histocalendarquotidien <- renderPlot({
    periodes=c('0h-2h','2h-4h','4h-6h','6h-8h','8h-10h','10h-12h','12h-14h','14h-16h',"16h-18h","18h-20h","20h-22h","22h-24h")
    if (!is.null(id()))
    {
      # affichage pour une borne de l historique sur 3 mois
      # juin - juillet - aout
      signal_levels <- c("Blanc","nosignal","Vert","Vert2")
      signal_colors <- c("gray96","darkgray","#62A667", "green")
      
      ## AFFICHER le fichier des signal RTE constitue
      signalBorne <- filter(enedisquotidien(),Borne==selectborne()$CodePDC)
      vtitle=paste("Calendrier quotidien Juin-Juillet-Aout 2019 de la Borne ", selectborne()$CodePDC,",  la station :", selectborne()$Station, "de la ville :", selectborne()$Commune)
      visu <- ggplot(signalBorne,aes(x=periode,y=jour))+
        geom_tile(aes(fill=signal),color="black", size=0.1)+#quadrillage des periodes
        geom_text(aes(label=nbTransaction))+
        #scale_fill_manual(values=c("gray96","#62A667","green","darkgray"))+
        scale_fill_manual(values=signal_colors, labels=signal_levels, drop = FALSE)+
        scale_y_reverse(breaks=rev(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)))+
        #facet_grid(~mois,scales="free",space="free",nrow=2)+
        facet_wrap(~ mois,nrow=1)+
        labs(x="Periode",y="Jour",title=vtitle)+
        #theme_tufte()+
        theme_gray(base_size=12)+#taille de la fonte (10-14 ?)
        theme(legend.title=element_text(size=15),
              legend.text=element_text(size=15),
              panel.background=element_rect(fill="transparent",colour=NA),#supprime le quadrillage du fond
              #panel.grid.major = element_line(colour = "gray"),
              #panel.grid=element_blank(),
              #panel.border=element_blank(),
              axis.ticks=element_blank(),
              #axis.text=element_text(size=12),#ADD
              #strip.background=element_blank(),
              legend.position="top",
              legend.justification="right",
              legend.direction="horizontal",
              legend.key.size=unit(0.3,"cm"),
              legend.spacing.x=unit(0.2,"cm"))
      
      return (visu)
    }
  })
    
  ############################################################################################################
  # CALENDRIER SUR 3 MOIS DU SIGNAL QUOTIDIEN
  # 05/08/2019
  output$histocalendar <- renderPlot({
    periodes=c('0h-2h','2h-4h','4h-6h','6h-8h','8h-10h','10h-12h','12h-14h','14h-16h',"16h-18h","18h-20h","20h-22h","22h-24h")
    if (!is.null(id()))
    {
      # affichage pour une borne de l historique sur 3 mois
      # juin - juillet - aout
      ## AFFICHER le fichier des signal RTE constitue
      signalBorne <- filter(enedisallhisto(),IDC==id())
      
      # 14 variables Date - IDC + 12 periodes
      # la transformer en 12 lignes avec Date -IDC + periode / valeur 
      signalBorne <- signalBorne %>% gather(periodes,key=period,value=signal)
      
      signalBorne$signal <- factor(signalBorne$signal,levels=c("Blanc","Vert","Vert2","Indefini"))
      signalBorne$period <- factor(signalBorne$period,levels=periodes)
      
      # signal pour les 3 mois
      signalStandard <- data.frame(date=rep(seq(from=as.POSIXct("2019-06-01"), to=as.POSIXct("2019-08-31"), by="day"),12),
                                   period =as.factor(c(rep("0h-2h",92),rep("2h-4h",92),rep("4h-6h",92),rep("6h-8h",92),
                                                       rep("8h-10h",92),rep("10h-12h",92),rep("12h-14h",92),rep("14h-16h",92),
                                                       rep("16h-18h",92),rep("18h-20h",92),rep("20h-22h",92),rep("22h-24h",92))))
      signalStandard$date <- lubridate::as_date(signalStandard$date)
      # ordonner la periode
      signalStandard$period <- factor(signalStandard$period,levels=periodes)
      # tri par date
      signalStandard <- arrange(signalStandard,date)  
      # ajouter les valeurs de signalBorne que l on a
      calendar <- left_join(signalStandard,signalBorne,by=c("date","period"))
      # mettre des valeurs par defaut pour tous les champs
      calendar$signal <- replace(calendar$signal,is.na(calendar$signal),"Indefini")
      calendar$IDC <- replace(calendar$IDC,is.na(calendar$IDC),id())
      
      #ajouter le jour et le mois
      calendar$mois <- lubridate::month(calendar$date)
      calendar$mois <- factor(calendar$mois,levels=c("6","7","8"))
      levels(calendar$mois)<-c("Juin","Juillet","Aout")
      calendar$jour <- lubridate::day(calendar$date)
      levels(calendar$jour)
      # ordonner la periode
      calendar$period <- factor(calendar$period,levels=periodes)
      calendar$signal <- factor(calendar$signal,levels=c("Blanc","Vert","Vert2","Indefini"))

      signal_levels <- c("Blanc","Vert","Vert2","Indefini")
      signal_colors <- c("gray96", "#62A667", "green", "darkgray")
      #scale_color_manual(values=signal_colors, labels=signal_levels, drop = FALSE)+
      
      # Affichage
      theTitle=paste("Signal de la Borne avec l\'IDC", id(),",  la station :", selectborne()$Station, "de la ville :", selectborne()$Commune)
      
      visu <- ggplot(calendar,aes(x=period,y=jour))+
        geom_tile(aes(fill=signal),color="white", size=0.1)+#quadrillage des periodes
        #geom_text(aes(label=period))+
        #scale_fill_manual(values=c("gray96","#62A667","green","darkgray"))+
        scale_fill_manual(values=signal_colors, labels=signal_levels, drop = FALSE)+
        scale_y_reverse(breaks=rev(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)))+
        #facet_grid(~mois,scales="free",space="free",nrow=2)+
        facet_wrap(~ mois,nrow=1)+
        labs(x="Periode",y="Jour",title=theTitle)+
        #theme_tufte()+
        theme_gray(base_size=12)+#taille de la fonte (10-14 ?)
        theme(legend.title=element_text(size=15),
              legend.text=element_text(size=15),
              panel.background=element_rect(fill="transparent",colour=NA),#supprime le quadrillage du fond
              #panel.grid.major = element_line(colour = "gray"),
              #panel.grid=element_blank(),
              #panel.border=element_blank(),
              axis.ticks=element_blank(),
              #axis.text=element_text(size=12),#ADD
              #strip.background=element_blank(),
              legend.position="top",
              legend.justification="right",
              legend.direction="horizontal",
              legend.key.size=unit(0.3,"cm"),
              legend.spacing.x=unit(0.2,"cm"))
      
      return (visu)
    }
  })
  # SORTIR LES ZONES COMMUNES A FAIRE
  # fnvisu <- function(dataframe) {
  # }  
    
  output$histocalendar2 <- renderPlot({
    periodes=c('0h-2h','2h-4h','4h-6h','6h-8h','8h-10h','10h-12h','12h-14h','14h-16h',"16h-18h","18h-20h","20h-22h","22h-24h")
    if (!is.null(id()))
    {
      # affichage pour une borne de l historique sur 3 mois
      # juin - juillet - aout
      ## AFFICHER le fichier des signal RTE constitue
      signalBorne <- filter(enedisallhisto(),IDC==id())
      
      # 14 variables Date - IDC + 12 periodes
      # la transformer en 12 lignes avec Date -IDC + periode / valeur 
      signalBorne <- signalBorne %>% gather(periodes,key=period,value=signal)
      
      signalBorne$signal <- factor(signalBorne$signal,levels=c("Blanc","Vert","Vert2","Indefini"))
      signalBorne$period <- factor(signalBorne$period,levels=periodes)
      
      # signal pour les 3 mois septembre - octobre - novembre ICI
      signalStandard <- data.frame(date=rep(seq(from=as.POSIXct("2019-09-01"), to=as.POSIXct("2019-11-30"), by="day"),12),
                                   period =as.factor(c(rep("0h-2h",91),rep("2h-4h",91),rep("4h-6h",91),rep("6h-8h",91),
                                                       rep("8h-10h",91),rep("10h-12h",91),rep("12h-14h",91),rep("14h-16h",91),
                                                       rep("16h-18h",91),rep("18h-20h",91),rep("20h-22h",91),rep("22h-24h",91))))
      signalStandard$date <- lubridate::as_date(signalStandard$date)
      # ordonner la periode
      signalStandard$period <- factor(signalStandard$period,levels=periodes)
      # tri par date
      signalStandard <- arrange(signalStandard,date)  
      # ajouter les valeurs de signalBorne que l on a
      calendar <- left_join(signalStandard,signalBorne,by=c("date","period"))
      # mettre des valeurs par defaut pour tous les champs
      calendar$signal <- replace(calendar$signal,is.na(calendar$signal),"Indefini")
      calendar$IDC <- replace(calendar$IDC,is.na(calendar$IDC),id())
      
      #ajouter le jour et le mois ICI
      calendar$mois <- lubridate::month(calendar$date)
      calendar$mois <- factor(calendar$mois,levels=c("9","10","11"))
      levels(calendar$mois)<-c("Septembre","Octobre","Novembre")
      calendar$jour <- lubridate::day(calendar$date)
      levels(calendar$jour)
      # ordonner la periode
      calendar$period <- factor(calendar$period,levels=periodes)
      calendar$signal <- factor(calendar$signal,levels=c("Blanc","Vert","Vert2","Indefini"))
      
      signal_levels <- c("Blanc","Vert","Vert2","Indefini")
      signal_colors <- c("gray96", "#62A667", "green", "darkgray")
      #scale_color_manual(values=signal_colors, labels=signal_levels, drop = FALSE)+
      
      # Affichage
      theTitle=paste("Signal de la Borne avec l\'IDC", id(),",  la station :", selectborne()$Station, "de la ville :", selectborne()$Commune)
      
      visu <- ggplot(calendar,aes(x=period,y=jour))+
        geom_tile(aes(fill=signal),color="white", size=0.1)+#quadrillage des periodes
        #geom_text(aes(label=period))+
        #scale_fill_manual(values=c("gray96","#62A667","green","darkgray"))+
        scale_fill_manual(values=signal_colors, labels=signal_levels, drop = FALSE)+
        scale_y_reverse(breaks=rev(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)))+
        #facet_grid(~mois,scales="free",space="free",nrow=2)+
        facet_wrap(~ mois,nrow=1)+
        labs(x="Periode",y="Jour",title=theTitle)+
        #theme_tufte()+
        theme_gray(base_size=12)+#taille de la fonte (10-14 ?)
        theme(legend.title=element_text(size=15),
              legend.text=element_text(size=15),
              panel.background=element_rect(fill="transparent",colour=NA),#supprime le quadrillage du fond
              #panel.grid.major = element_line(colour = "gray"),
              #panel.grid=element_blank(),
              #panel.border=element_blank(),
              axis.ticks=element_blank(),
              #axis.text=element_text(size=12),#ADD
              #strip.background=element_blank(),
              legend.position="top",
              legend.justification="right",
              legend.direction="horizontal",
              legend.key.size=unit(0.3,"cm"),
              legend.spacing.x=unit(0.2,"cm"))
      
      return (visu)
    }
  })
  ############################################################################################################
  # CALENDRIER HISTORIQUE SUR 15 MOIS AVEC NB de transactions
  # 13/09/2019
  
  output$histocalendartrans <- renderPlot({
    #input$varhist18 # nbTransaction - Consommationkwh - DureeChargemin
    if (!is.null(id()))
    {
      # affichage pour une borne de l historique sur 15 mois
      # janvier 2018 a mars 2019
      ## AFFICHER le fichier des signal RTE constitue
      data <- filter(saisonallhisto(),Borne==selectborne()$CodePDC)
      
      vtitle=paste("Calendrier historique de la Borne ", selectborne()$CodePDC,",  la station :", selectborne()$Station, "de la ville :", selectborne()$Commune)
      #vtitle=paste("Calendrier historique de la Borne avec l\'IDC", id(),",  la station :", selectborne()$Station, "de la ville :", selectborne()$Commune)
      # vtitle = paste("Calendrier",input$varhist18,": consolidation mois par Borne pour",input$nbBornes)
      # CALENDRIER de la borne choisie
      ggplot(data,aes(x=Month,y=periodeJour))+
        geom_tile(aes(fill=signal),color="black", size=0.1)+#quadrillage des periodes
        geom_text(aes_string(label=input$varhist18))+
        scale_fill_manual(values=c("gray96","darkgray","#62A667","green"), 
                          labels=c("Blanc","nosignal","Vert","Vert2"), drop = FALSE)+
        scale_x_date(date_breaks = "1 months",date_labels = "%b\n%Y",expand = c(0,0))+
        #facet_grid(~Borne,scales="free",space="free")+ # ILLISIBLE
        #facet_wrap(~ BorneVille,ncol=3)+
        labs(title = vtitle,x="Mois",y="Periode")+
        #theme
        theme_gray(base_size=12)+#taille de la fonte (10-14 ?)
        theme(legend.title=element_text(size=15),
              legend.text=element_text(size=15),
              panel.background=element_rect(fill="transparent",colour=NA),#supprime le quadrillage du fond
              #panel.grid.major = element_line(colour = "gray"),
              #panel.grid=element_blank(),
              #panel.border=element_blank(),
              axis.ticks=element_blank(),
              #axis.text=element_text(size=12),#ADD
              #strip.background=element_blank(),
              legend.position="top",
              legend.justification="right",
              legend.direction="horizontal",
              legend.key.size=unit(0.3,"cm"),
              legend.spacing.x=unit(0.2,"cm"))
  }
  })
  
############################################################################################################
# 9/08/2019 : CARTE AVEC SIGNAL ENEDIS
# Visualisation de toutes les entites
  unite18 <- eventReactive(
    {
      input$varhist18
    }, 
    {
      if(input$varhist18 == "Consommationkwh"){
        unite <- "kwh"
      } else if(input$varhist18 == "DureeChargemin"){
        unite <- "minutes"
      } else if(input$varhist18 == "nbTransaction"){
        unite <- "nombre"
      }
      return (unite)
    })
  tempshist18 <- eventReactive(
    {
      input$temps
    },
    {
      if(input$temps == "mois")
      {
        vartemps <- "Month"
      }
      else
      {
        vartemps <- "week"
      }
      return (vartemps)
    })
  # 12/08/2019 : le signal historique + transactions par mois
  # toutes les bornes ou le top 20
  datahisto18 <- eventReactive(
    {
      input$nbBornes
    }, 
    {
        if(input$nbBornes == "toutes")
        {
          data <- transactionsBornehistoMoisAll
        }
        else if(input$nbBornes == "top20")
        {
          data <- transactionsBornehistoMois
        }
      return (data)
    })
# visualisation des mois histo 2018 + Hiver2019
# en colonne par mois avec repartition par signal
 # echelle Y FIXE 
  output$ggplotfacet2 <- renderPlot({
    input$varhist18 # nbTransaction - Consommationkwh - DureeChargemin
    input$nbBornes # toutes les bornes ou top 20
    vtitle = paste(input$varhist18,": consolidation mois par Borne pour",input$nbBornes)
    # Echelle commune
    # VISU BORNE
    ggplot(datahisto18(), aes_string("Month",input$varhist18)) +
      geom_col(aes(fill=signal),colour="#62A667")+ 
      scale_x_date(date_breaks = "1 months",date_labels = "%m\n%y",expand = c(0,0))+
      scale_fill_manual(values=c("white","darkgray","#62A667","green"), 
                        labels=c("Blanc","nosignal","Vert","Vert2"), drop = FALSE)+
      facet_wrap( ~BorneVille,ncol=4)+
      labs(title = vtitle, x = input$temps, y=unite18())
  })
  # visualisation des mois histo 2018 + Hiver2019
  # en calendrier par mois avec repartition par periode + signal
  # echelle Y FIXE 
  output$ggplotfacetCal <- renderPlot({
    input$varhist18 # nbTransaction - Consommationkwh - DureeChargemin
    input$temps #Mois ou Semaine
    input$nbBornes # toutes les bornes ou top 20
    vtitle = paste("Calendrier",input$varhist18,": consolidation mois par Borne pour",input$nbBornes)
    # VISU 20 BORNES TOP
    data <- transactionsBornehistoPeriode
    # ordonner la periode du jour
    #   periodes=c('0h-2h','2h-4h','4h-6h','6h-8h','8h-10h','10h-12h','12h-14h','14h-16h',"16h-18h","18h-20h","20h-22h","22h-24h")
    periodes=c("h68","h810","h1012","h1214","h1416","h1618","nuit")
    data$periodeJour <- factor(data$periodeJour,levels=periodes)
    # CALENDRIER du TOP 20
    ggplot(data,aes(x=Month,y=periodeJour))+
      geom_tile(aes(fill=signal),color="black", size=0.1)+#quadrillage des periodes
      geom_text(aes_string(label=input$varhist18))+
      scale_fill_manual(values=c("gray96","darkgray","#62A667","green"), 
                        labels=c("Blanc","nosignal","Vert","Vert2"), drop = FALSE)+
      scale_x_date(date_breaks = "1 months",date_labels = "%b\n%Y",expand = c(0,0))+
      #facet_grid(~Borne,scales="free",space="free")+ # ILLISIBLE
      facet_wrap(~ BorneVille,ncol=3)+
      labs(title = vtitle,x="Mois",y="Periode")+
      #theme
      theme_gray(base_size=12)+#taille de la fonte (10-14 ?)
      theme(legend.title=element_text(size=15),
            legend.text=element_text(size=15),
            panel.background=element_rect(fill="transparent",colour=NA),#supprime le quadrillage du fond
            #panel.grid.major = element_line(colour = "gray"),
            #panel.grid=element_blank(),
            #panel.border=element_blank(),
            axis.ticks=element_blank(),
            #axis.text=element_text(size=12),#ADD
            #strip.background=element_blank(),
            legend.position="top",
            legend.justification="right",
            legend.direction="horizontal",
            legend.key.size=unit(0.3,"cm"),
            legend.spacing.x=unit(0.2,"cm"))
  })
############################################################################################################
# CARTE AVEC SIGNAL ENEDIS
# le signal existe de 0h a 24h
# periode RTE : 0h-24 - enrichie par ENEDIS de 6h a 18h

 output$map <- renderLeaflet({
      #data = data()
      
      # Signal Enedis :
      #   Blanc pour Normal
      #   Vert pour local dep - RTE
      #   Vert2 pour Local DPE - ENEDIS
      #pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=data()$h1214)
      pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=c("Blanc","Vert","Vert2"))
      
      m <- leaflet(data = data()) %>%
        addTiles() %>% addMarkers(~lon, ~lat, icon = ~myIcons[type]) %>%
        addTiles() %>% 
        #addLegend(position="topleft",pal=pal, values= ~h68,title = "Signal Enedis 6h-8h",opacity=3) %>%
        addLegend(position="topleft",pal=pal, values= c("Blanc","Vert","Vert2"),title = "Signal Enedis 6h-8h",opacity=3) %>%
        addCircleMarkers(layerId=~IDC,
                         lng = ~lon,lat = ~lat,
                         color = "gray",
                         stroke="FALSE",
                         fillOpacity=3,
                         fillColor = ~ pal(h68),
                         label = paste(data()$Commune,":",data()$Station),
                         labelOptions = labelOptions(noHide=F,direction='top', textsize='15px'),
                         #clusterOptions = markerClusterOptions(),
                         # Station;Commune;Adresse;Identifiant;Type;StationCode;Connecteur;ChargeBoxIdentity;CodePDC;PDL_IDC;
                         # INT Numero;CodeInsee
                         popup = ~paste("<b>","Commune :","</b>",as.character(data()$Commune),"</br>",
                                        "<b>","Ruralite :","</b>",as.character(data()$Ruralite),"</br>",
                                        "<b>","Nom de la Station :","</b>",as.character(data()$Station),"</br>",
                                        #"<b>","Date Consuel :","</b>",as.character(data()$DateConsuel),"</br>",
                                        "<b>","Code PDC :","</b>",as.character(data()$CodePDC),"</br>",
                                        #"<b>","ID station :","</b>",as.character(data()$Identifiant),"</br>","</br>",
                                        "<b>","Code Station :","</b>",as.character(data()$StationCode),"</br>",
                                        "<b>","Connecteurs :","</b>",as.character(data()$Connecteur),"</br>",
                                        "<b>","Type :","</b>",as.character(data()$Type),"</br>",
                                        "<b>","Adresse de la Station :","</b>",as.character(data()$Adresse),"</br>","</br>",
                                        "<b>","ChargeBoxIdentity :","</b>",as.character(data()$ChargeBoxIdentity),"</br>",
                                        "<b>","IDC :","</b>",as.character(data()$IDC),"</br>","</br>",
                                        "<b>","Numero :","</b>",as.character(data()$Numero),"</br>",
                                        "<b>","Code INSEE :","</b>",as.character(data()$CodeInsee),"</br>","</br>",
                                        "<b>","SIGNAL DU : ",as.character(data()$date),"</br>",
                                        "<b>","Nuit : 0h-2h : ",as.character(data()$h02),"2h-4h : ",as.character(data()$h24),"4h-6h : ",as.character(data()$h46),"</br>",
                                        "<b>","Matin: 6h-8h : ",as.character(data()$h68),"8h-10h : ",as.character(data()$h810),"10h-12h : ",as.character(data()$h1012),"</br>",
                                        "<b>","Aprem: 12h-14h : ",as.character(data()$h1214),"14h-16h : ",as.character(data()$h1416),"16h-18h : ",as.character(data()$h1618),"</br>",
                                        "<b>","Soir : 18h-20h : ",as.character(data()$h1820),"20h-22h : ",as.character(data()$h2022),"22h-24h : ",as.character(data()$h2224),"</br>")
        )
      m
    })
  # Use a separate observer to recreate the legend as needed.
  observe({
    input$periodtext
    
    #12 periodes pour le signal : 6 RTE et 6 ENEDIS
    listperiod=c("h02","h24","h46","h68","h810","h1012","h1214","h1416","h1618","h1820","h2022","h2224")
    
    proxy <- leafletProxy("map", data = data())
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    proxy %>% clearMarkers() %>% clearControls() %>% clearShapes() 
    
    # PB domain ne fnb pas + 2 valeurs de input$period
    # period=listperiod[1]
    # title <- paste("Periode ",period)
    # pal = leaflet::colorFactor(palette=c("white","green"), domain=data()[[period]])
    # proxy %>% addLegend(position = "topleft",
    #                     pal = pal, values = ~period,title = title,opacity=2)
    # colorpal=~pal(period)
    # val=~period
    # cercle=10
    # signal_colors <- c("gray96", "#62A667","green", "darkgray")
    #les periodes "6h-8h","8h-10h","10h-12h","12h-14h","14h-16h","16h-18h"
   
    if (input$periodtext=="0h-2h") {
      pal = leaflet::colorFactor(palette=c("white","#62A667"), domain=c("Blanc","Vert"))
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~h02,title = "Periode 0h-2h",opacity=2)
      colorpal=~pal(h02)
      val=~h02
      cercle=10
    } 
    else if (input$periodtext=="2h-4h") {
      pal = leaflet::colorFactor(palette=c("white","#62A667"), domain=data()$h24)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~h24,title = "Periode 2h-4h",opacity=2)
      colorpal=~pal(h24)
      val=~h24
      cercle=10
    }
    else if (input$periodtext=="4h-6h") {
      pal = leaflet::colorFactor(palette=c("white","#62A667"), domain=data()$h46)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~h46,title = "Periode 4h-6h",opacity=2)
      colorpal=~pal(h46)
      val=~h46
      cercle=10
    }
    else if (input$periodtext=="6h-8h") {
      pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=data()$h68)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~h810,title = "Periode 6h-8h",opacity=2)
      colorpal=~pal(h68)
      val=~h68
      cercle=10
    }
    else if (input$periodtext=="8h-10h") {
      pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=data()$h810)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~h810,title = "Periode 8h-10h",opacity=2)
      colorpal=~pal(h810)
      val=~h810
      cercle=10
    }
    else if (input$periodtext=="10h-12h")
    {
      pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=data()$h1012)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~h1012,title = "Periode 10h-12h",opacity=2)
      colorpal=~pal(h1012)
      val=~h1012
      cercle=10
    }
    else if (input$periodtext=="12h-14h")
    {
      pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=data()$h1214)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~h1214,title = "Periode 12h-14h",opacity=2)
      colorpal=~pal(h1214)
      val=~h1214
      cercle=10
    }
    else if (input$periodtext=="14h-16h")
    {
      pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=data()$h1416)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~h1416,title = "Periode 14h-16h",opacity=2)
      colorpal=~pal(h1416)
      val=~h1416
      cercle=10
    }
    else if (input$periodtext=="16h-18h")
    {
      pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=data()$h1618)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~h1618,title = "Periode 16h-18h",opacity=2)
      colorpal=~pal(h1618)
      val=~h1618
      cercle=10
    }
    else if (input$periodtext=="18h-20h")
    {
      pal = leaflet::colorFactor(palette=c("white","#62A667"), domain=data()$h1820)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~h1820,title = "Periode 18h-20h",opacity=2)
      colorpal=~pal(h1820)
      val=~h1820
      cercle=10
    }
    else if (input$periodtext=="20h-22h")
    {
      pal = leaflet::colorFactor(palette=c("white","#62A667"), domain=data()$h2022)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~h2022,title = "Periode 20h-22h",opacity=2)
      colorpal=~pal(h2022)
      val=~h2022
      cercle=10
    }
    else if (input$periodtext=="22h-24h")
    {
      pal = leaflet::colorFactor(palette=c("white","#62A667"), domain=data()$h2224)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~h2224,title = "Periode 22h-24h",opacity=2)
      colorpal=~pal(h2224)
      val=~h2224
      cercle=10
    }
    # Remove any existing circle
    proxy %>% addCircleMarkers(layerId= ~IDC,
    #proxy %>% addMarkers(~lon, ~lat, icon = ~myIcons[type]) %>% addCircleMarkers(layerId= ~IDC,
                       lng = ~lon,lat = ~lat, fillOpacity=3,
                       color = "gray",
                       stroke="FALSE",
                       radius = cercle,
                       fillColor = colorpal, #Type
                       label = paste(data()$Commune,":",data()$Station),
                       labelOptions = labelOptions(noHide=F,direction='top', textsize='15px'),
                       popup = ~paste("<b>","Commune :","</b>",as.character(data()$Commune),"</br>",
                                      "<b>","Ruralite :","</b>",as.character(data()$Ruralite),"</br>",
                                      "<b>","Nom de la Station :","</b>",as.character(data()$Station),"</br>",
                                      #"<b>","Date Consuel :","</b>",as.character(data()$DateConsuel),"</br>",
                                      "<b>","Code PDC :","</b>",as.character(data()$CodePDC),"</br>",
                                      #"<b>","ID station :","</b>",as.character(data()$Identifiant),"</br>","</br>",
                                      "<b>","Code Station :","</b>",as.character(data()$StationCode),"</br>",
                                      "<b>","Connecteur :","</b>",as.character(data()$Connecteur),"</br>",
                                      "<b>","Type :","</b>",as.character(data()$Type),"</br>",
                                      "<b>","Adresse de la Station :","</b>",as.character(data()$Adresse),"</br>","</br>",
                                      "<b>","ChargeBoxIdentity :","</b>",as.character(data()$ChargeBoxIdentity),"</br>",
                                      "<b>","IDC :","</b>",as.character(data()$IDC),"</br>","</br>",
                                      "<b>","Numero :","</b>",as.character(data()$Numero),"</br>",
                                      "<b>","Code INSEE :","</b>",as.character(data()$CodeInsee),"</br>","</br>",
                                      "<b>","SIGNAL DU : ",as.character(data()$date),"</br>",
                                      "<b>","Nuit : 0h-2h : ",as.character(data()$h02),"2h-4h : ",as.character(data()$h24),"4h-6h : ",as.character(data()$h46),"</br>",
                                      "<b>","Matin: 6h-8h : ",as.character(data()$h68),"8h-10h : ",as.character(data()$h810),"10h-12h : ",as.character(data()$h1012),"</br>",
                                      "<b>","Aprem: 12h-14h : ",as.character(data()$h1214),"14h-16h : ",as.character(data()$h1416),"16h-18h : ",as.character(data()$h1618),"</br>",
                                      "<b>","Soir : 18h-20h : ",as.character(data()$h1820),"20h-22h : ",as.character(data()$h2022),"22h-24h : ",as.character(data()$h2224),"</br>")
                        )
 
  })

  ###########################################################################################
  # VISU CARTO : les bornes avec le logo en charge a l heure 
  output$mapcharge <- renderLeaflet({

    # Signal Enedis :
    #   Blanc pour Normal
    #   Vert pour local dep - RTE
    #   Vert2 pour Local DPE - ENEDIS
    #pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=c("Blanc","Vert","Vert2"))
    #trouver la periode qui correspond a lheure en cours
    lheure <- lubridate::hour(lubridate::floor_date(lubridate::now(),unit="hour"))
    laperiode <- data.frame(
      heure=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
      periode=c(rep("h02",2),rep("h24",2),rep("h46",2),rep("h68",2),rep("h810",2),rep("h1012",2),
                rep("h1214",2),rep("h1416",2),rep("h1618",2),rep("h1820",2),rep("h2022",2),rep("h2224",2))
    )
    periode <- laperiode$periode[laperiode$heure==lheure]
    #periode <- select(filter(heureperiode, heure==lheure),c("periode"))
    
    pal = leaflet::colorFactor(palette=c("white","#62A667","green","darkgray"), domain=c("Blanc","Vert","Vert2","NA"))
    #pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=borneswithsignal[[periode]])
    if (periode=="h02") {
      pal = leaflet::colorFactor(palette=c("white","#62A667"), domain=c("Blanc","Vert"))
      colorpal=~pal(h02)
      title = "Periode 0h-2h"
    } 
    else if (periode=="h24") {
      #pal = leaflet::colorFactor(palette=c("white","#62A667"), domain=data()$h24)
      colorpal=~pal(h24)
      title = "Periode 2h-4h"
    }
    else if (periode=="h46") {
      #pal = leaflet::colorFactor(palette=c("white","#62A667"), domain=data()$h46)
      colorpal=~pal(h46)
      title = "Periode 4h-6h"
    }
    else if (periode=="h68") {
      #pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=data()$h68)
      colorpal=~pal(h68)
      title = "Periode 6h-8h"
    }
    else if (periode=="h810") {
      #pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=data()$h810)
      colorpal=~pal(h810)
      title = "Periode 8h-10h"
    }
    else if (periode=="h1012")
    {
      #pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=data()$h1012)
      colorpal=~pal(h1012)
      title = "Periode 10h-12h"
    }
    else if (periode=="h1214")
    {
      #pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=data()$h1214)
      colorpal=~pal(h1214)
      title = "Periode 12h-14h"
    }
    else if (periode=="h1416")
    {
      #pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=data()$h1416)
      colorpal=~pal(h1416)
      title = "Periode 14h-16h"
    }
    else if (periode=="h1618")
    {
      #pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=data()$h1618)
      colorpal=~pal(h1618)
      title = "Periode 16h-18h"
    }
    else if (periode=="h1820")
    {
      #pal = leaflet::colorFactor(palette=c("white","#62A667"), domain=data()$h1820)
      # proxy %>% addLegend(position = "topleft",
      #                     pal = pal, values = ~h1820,title = "Periode 18h-20h",opacity=2)
      # 
      colorpal=~pal(h1820)
      title = "Periode 18h-20h"
    }
    else if (periode=="h2022")
    {
      #pal = leaflet::colorFactor(palette=c("white","#62A667"), domain=data()$h2022)
      colorpal=~pal(h2022)
      title = "Periode 20h-22h"
    }
    else if (input$periodtext=="22h-24h")
    {
      #pal = leaflet::colorFactor(palette=c("white","#62A667"), domain=data()$h2224)
      colorpal=~pal(h2224)
      title = "Periode 22h-24h"
    }
    pal = leaflet::colorFactor(palette=c("white","#62A667","green"), domain=c("Blanc","Vert","Vert2"))
    theTitle = paste("Signal Enedis ",title)
    m <- leaflet(data = data()) %>%
      addTiles() %>% addMarkers(~lon, ~lat, icon = ~myIcons[type],label = paste(data()$Commune,":",data()$Station),
                                labelOptions = labelOptions(noHide=F,direction='top', textsize='15px')) %>%
      addLegend(position="topleft",pal=pal, values= c("Blanc","Vert","Vert2"),title = theTitle,opacity=3) %>%
      addCircleMarkers(layerId=~IDC,
                       lng = ~lon,lat = ~lat,
                       color = "gray",
                       stroke="FALSE",
                       fillOpacity=3,
                       fillColor = colorpal,
                       label = paste(data()$Commune,":",data()$Station),
                       labelOptions = labelOptions(noHide=F,direction='top', textsize='15px'))
  })
  ######################################################################################################
  #add a rectangle to display the selected station
  # observe({
  #   input$station
  #   laStation<-filter(df,Station==input$station)
  #   laStation <- laStation[1,]
  #   
  #   proxy <- leafletProxy("map", data = df)
  #   # Remove any existing legend, and only if the legend is enabled, create a new one.
  #   proxy %>% clearShapes() %>%  
  #     addRectangles(
  #       lng1=laStation$lon-0.02, lat1=laStation$lat-0.02,
  #       lng2=laStation$lon+0.02, lat2=laStation$lat+0.02,
  #       color="gray",
  #       fill = FALSE,
  #       highlightOptions = highlightOptions(color = "grey", weight = 2,
  #       bringToFront = TRUE)
  #     )
  # })
  ######################################################################################################
  #add a rectangle to display the selected town
  observe({
    input$ville
    laVille <- filter(df, Commune==input$ville)
    laVille <- laVille[1,]
    
    proxy <- leafletProxy("map", data = df)
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    proxy %>% clearShapes() %>%  
      addRectangles(
        lng1=laVille$lon-0.02, lat1=laVille$lat-0.02,
        lng2=laVille$lon+0.02, lat2=laVille$lat+0.02,
        color="green",
        fill = FALSE,
        highlightOptions = highlightOptions(color = "grey", weight = 2,
                                            bringToFront = TRUE)
      )
  })

}
shinyApp(ui, server)