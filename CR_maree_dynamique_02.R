# Author : Morgane Amelot
# Date: 08/03/2025

library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
library(ggthemes)
library(paletteer)
library(DT)
library(vegan)
library(reshape2)
library(picante)
library(knitr)
library(shinyFiles)
library(purrr)

# charge la base de données
#setwd("/media/pacman/My Passport/THESE/ifr3m1/Documents/git/these_celtic/COURS/BIO_MOL/essai_CR_maree_interractif/App_try")
dd <- read.csv("bdd_maree_all_year.csv", sep=",") # plutot que la base de donnees une base de reference avec l'ensemble des variables possibles

# conditionne les listes deroulantes disponibles de maniere indépendante a la base de donnees à charger plus loin 
levels<-c("Phylum","Classe","Ordre")
Phylums<-c("Tous",unique(dd$Phylum))
Classes<-c("Tous",unique(dd$Classe))
Ordres<-c("Tous",unique(dd$Ordre))
Annees<-c("Toutes",unique(dd$Annee))

# Ajout des variables environnement et methodes
enviro<-c("Tous",unique(dd$Milieu))
method<-c("Toutes",unique(dd$Methode))

# Ajout du type de courbe de rarefaction et du type d'indice de diversite 
rarefaction<-c("collector","random","exact","coleman","rarefaction")
#diversite<-c("Simpson","Shannon","Eveness")

# selectionne les palettes de couleurs
pal1<-paletteer_d("tvthemes::WaterTribe") 
pal2<-paletteer_d("tidyquant::tq_light") 


dd$file_names2<-paste0("www2/",dd$file_names)
#files <-unique(dd$file_names2)





# la première section conditionne les paramêtre graphique de la page ainsi que que les variables changeant en fonction des selections de l'utilisateur
ui<-fluidPage(
  tags$head(
    tags$meta(name="author", content="Morgane Amelot"),
    tags$meta(name="creation_date", content="08/03/2024")),
  
  theme = bslib::bs_theme(bootswatch = "darkly"),
  tabsetPanel(
    tabPanel("Espèce", # premier panel permettant la visualisation des donnees par groupe taxonomique 
  div( # fenêtre menu deroulants
    style = "display:flex; align-items:flex-start",
    wellPanel( #~~ Sidebar ~~#
      style = "overflow-y: auto; position:fixed; width:30vh; top:10vh; bottom:0", # selectionne la position et taille de la fenêtre de menus deroulants
     # sidebar( # conditionne les menus deroulant disponible
        selectInput("vvar", "Niveau taxonomique (a remplir systématiquement)",selected="Phylum",choices=levels),
        selectInput("wvar", "Phylums (selectionner seulement si le niveau taxonomique est Phylum)", selected="Tous",choices=Phylums),
        selectInput("xvar", "Classes (selectionner seulement si le niveau taxonomique est Classe)", selected="Tous",choices=Classes),
        selectInput("yvar", "Ordres (selectionner seulement si le niveau taxonomique est Ordre)", selected="Tous",choices=Ordres),
        selectInput("zvar", "Annees", selected="Toutes",choices=Annees),
        selectInput("avar", "Environnement", selected="Tous",choices=enviro),
        selectInput("bvar", "Methodes", selected="Toutes",choices=method),
        fileInput("file1", "Choose CSV File", accept = ".csv"), # charge le fichier d'entree
        checkboxInput("header", "Header", TRUE),
        downloadButton('download',"Telecharger la liste d'espece")
       #)
      )
    ),
    div( #~~ Main panel ~~# fenêtre principale
      titlePanel("Compte rendu marees"), # titre fenêtre principale
      style = "flex-grow:1; resize:horizontal; overflow: hidden; position:relative; margin-left: 310px", # selectionne taille et localisation de la fenêtre princiaple 
      plotOutput("plot1"),DT::dataTableOutput("table2")
    )
  ), 
  
  tabPanel("Espèce image", # premier panel permettant la visualisation des donnees par groupe taxonomique 
           div( # fenêtre menu deroulants
             style = "display:flex; align-items:flex-start",
             wellPanel( #~~ Sidebar ~~#
               style = "overflow-y: auto; position:fixed; width:30vh; top:15vh; bottom:0", # selectionne la position et taille de la fenêtre de menus deroulants
             #sidebar( # conditionne les menus deroulant disponible
                 fileInput("file2", "Charger la liste d'espece", accept = ".csv"), # charge le fichier d'entree
                 checkboxInput("header", "Header", TRUE)
              # )
             )
           ),
           div( #~~ Main panel ~~# fenêtre principale
             titlePanel("Compte rendu marees"), # titre fenêtre principale
             style = "flex-grow:1; resize:horizontal; overflow: hidden; position:relative; margin-left: 310px", # selectionne taille et localisation de la fenêtre princiaple 
             uiOutput('images')
           )
  ),
  
tabPanel("Courbe de rarefaction", # second panel pour les courbes de rarefaction
            div( # fenêtre menu deroulants
  style = "display:flex; align-items:flex-start",
  wellPanel( #~~ Sidebar ~~#
    style = "overflow-y: auto; position:fixed; width:30vh; top:10vh; bottom:5vh", # selectionne la position et taille de la fenêtre de menus deroulants
    #sidebar( # conditionne les menus deroulant disponible
      selectInput("cvar", "Calcul rarefaction",selected="random",choices=rarefaction),
      selectInput("zzvar", "Annees", selected="Toutes",choices=Annees),
      selectInput("aavar", "Environnement", selected="Tous",choices=enviro),
      selectInput("bbvar", "Methodes", selected="Toutes",choices=method),
      actionButton("capture",
                   "capture value")
    #)
  )), div( #~~ Main panel ~~# fenêtre principale
  titlePanel("Test echantillonage"), # titre fenêtre principale
  style = "flex-grow:1; resize:horizontal; overflow: hidden; position:relative; margin-left: 310px", # selectionne taille et localisation de la fenêtre princiaple 
  plotOutput("plot2")
)),

tabPanel("Indice de diversite", # troisieme panel pour les indices de diversite
               div( # fenêtre menu deroulants
                  style = "display:flex; align-items:flex-start",
                   wellPanel( #~~ Sidebar ~~#
                    style = "overflow-y: auto; position:fixed; width:30vh; top:10vh; bottom:5vh", # selectionne la position et taille de la fenêtre de menus deroulants
               # sidebar( # conditionne les menus deroulant disponible
                    #   selectInput("dvar", "indice de diversité",selected="Nbr",choices=diversite),
                       selectInput("zzzvar", "Annees", selected="Toutes",choices=Annees),
                       selectInput("aaavar", "Environnement", selected="Tous",choices=enviro),
                       selectInput("bbbvar", "Methodes", selected="Toutes",choices=method),
                    #)
                 )), div( #~~ Main panel ~~# fenêtre principale
                  titlePanel("Test echantillonage"), # titre fenêtre principale
                 style = "flex-grow:1; resize:horizontal; overflow: hidden; position:relative; margin-left: 310px", # selectionne taille et localisation de la fenêtre princiaple 
                 DT::dataTableOutput("table1"))
)
))

# la deuxième section conditionne l'utilisation des valeurs entrées par l'utilisateur ainsi que les graphiques a sortir et autres éléments 

server <- function(input, output) { 
  thematic::thematic_shiny() # theme de la shiny app
  
  # charger et conditionner les donnees
vari<-reactive({input$vvar}) # valeurs dynamiques issues des menus deroulants (type de variables)
vari2<-reactive({input$wvar})# valeurs dynamiques issues des menus deroulants (variables phylum)
vari3<-reactive({input$xvar})# valeurs dynamiques issues des menus deroulants (variables classe)
vari4<-reactive({input$yvar})# valeurs dynamiques issues des menus deroulants (variables ordre)

select_annee<-reactive({input$zvar}) # conditionne l'année comme pouvant varier aussi 
select_annee2<-reactive({input$zzvar}) # conditionne l'année comme pouvant varier aussi 
select_annee3<-reactive({input$zzzvar}) # conditionne l'année comme pouvant varier aussi 

vari5<-reactive({input$avar})
vari5_b<-reactive({input$aavar})# valeurs dynamiques issues des menus deroulants (variables environnement)
vari5_c<-reactive({input$aaavar})# valeurs dynamiques issues des menus deroulants (variables environnement)

vari6<-reactive({input$bvar})
vari6_b<-reactive({input$bbvar})# valeurs dynamiques issues des menus deroulants (variables methode)
vari6_c<-reactive({input$bbbvar})# valeurs dynamiques issues des menus deroulants (variables methode)

vari7<-reactive({input$cvar})# valeurs dynamiques issues des menus deroulants (variables environnement)
vari8<-reactive({input$dvar})# valeurs dynamiques issues des menus deroulants (variables methode)

data <- eventReactive(input$file1, {
  read.csv(input$file1$datapath) # lit la base de données sélectionnée 
})

data2 <- eventReactive(input$file2, {
  read.csv(input$file2$datapath) # lit la base de données sélectionnée 
})


####### Manipulation de donnees necessaire a l'obtention des premiers tableaux et graphs ####
######  Cette section correspond à la visualisation des espece au cours du temps par taxa/methodologie/milieu/annee

sell1<- reactive({ # reactive variables permettant de faire la selection
  #de la base de données en fonction du niveau de groupement choisi ainsi que de la sous sélection si retenue (spécifique phylum, classe, ordre)
  if (vari()=="Phylum") {
    sell<-vari2()
  } else {if (vari()=="Classe") {
    sell<-vari3()
  }else {if (vari()=="Ordre") {
    sell<-vari4()
  }}}
  return(sell)
})

col1<- reactive({ # selectionne le numéro de la colonne sur laquelle faire les filtres en fonction
  if (vari()=="Phylum") {
    col<-1
  } else {if (vari()=="Classe") {
   col<-4
  }else {if (vari()=="Ordre") {
    col<-5
  }}}
  return(col)
})


bdd_g1<- reactive({ # check si "Tous" selectionné on ignore les éléments précédents et on prend l'ensemble de la base de données
  if (sell1()=="Tous") {
    dd_g1<-data()
  } else {dd_g1<-data() %>% filter(.[[col1()]]==sell1())}
  return(dd_g1)
})
  

  
bdd_g2<- reactive({ # ignore l'année si on conditionne notre variables sur toutes aussi 
    if (select_annee()!="Toutes") {
      dd_g2<-bdd_g1() %>% filter(Annee %in% select_annee())
    } else {dd_g2<-bdd_g1()}
    return(dd_g2)
  })
  

bdd_g3<- reactive({ # ignore le milieu si on conditionne notre variables sur tous aussi 
  if (vari5()!="Tous") {
    dd_g2<-bdd_g2() %>% filter(Milieu %in% vari5())
  } else {dd_g2<-bdd_g2()}
  return(dd_g2)
})

bdd_g4<- reactive({ # ignore la methode si on conditionne notre variables sur toutes aussi 
  if (vari6()!="Toutes") {
    dd_g2<-bdd_g3() %>% filter(Methode %in% vari6())
  } else {dd_g2<-bdd_g3()}
  return(dd_g2)
})


   dd_graph<-reactive({dd_bg2<-bdd_g4() %>% # sort finalement la base de données que l'on souhaite utliser pour le graphique 
    group_by(Annee,vari(),Etage)%>%
    summarise (taxa_sum=length(unique(Nom_latin)))
  return(dd_bg2)
  })
  
   # sort le tableau avec le nom des espèce et l'étage en fonction de la seléction en dessous de notre graphique 
   dd_table<-reactive({dd_bg3<-bdd_g4()
   dd_bg3<-dd_bg3[,c("Nom_latin","Substrat","Etage","Annee")]
   colnames(dd_bg3)<-c("Espece","Substrat","Etage", "Annee")
   dd_bg4<-dd_bg3 %>% group_by(Espece, Etage, Substrat)%>% summarise(n=length(unique(Annee)))
   return(dd_bg4)
   })
   output$table2 <- DT::renderDataTable(DT::datatable({   dd_table() }))
  output$plot1<- renderPlot({ # établi la structure du graphique 
   
    v_factor_levels <- c("supra L","supra-médio L","médio L","médio L-infra L","infra L","infra-subtidal","subtidal","")
   ggplot(dd_graph(), aes(x=Annee, y=taxa_sum, fill=factor(Etage, levels = v_factor_levels)))+geom_col()+theme_economist()+ylab("Nombre de taxons par année")+
     xlab("Années")+ theme(text=element_text(size=16))+#xlim(min(select_annee)-1,max(select_annee)+1)+
     labs(fill='Etage')+scale_fill_manual(values = pal1, breaks=c("supra L","supra-médio L","médio L","médio L-infra L","infra L","infra-subtidal","subtidal",""))
    
  
   # ggplot(dd3, aes(x=Annee, y=taxa_sum, fill=as.factor(Annee)))+geom_col()+theme_minimal()+ylab("Nombre de taxons par année")+
     #xlab("Années")+#xlim(min(select_annee)-1,max(select_annee)+1)+
    
     #labs(fill='Années')
#    hist(dd3$taxa_sum)
    
   
  })
  
  output$download <- downloadHandler(
    filename = function(){"liste_espece.csv"}, 
    content = function(fname){
      write.csv(dd_table(), fname)}
  )
   ######################### ajout d'image correspondant a la liste presente dans le tableau 
   # la mise à jour n'est faites qu'a chaqu fois qu'un bouton reactif est pressé 
  
  files <- reactive({ dd_use<-data2()
  dd_species<-unique(dd_use[,c("Espece")])
  file_names<-unique(dd$file_names2[dd$Nom_latin %in% dd_species])
  return(file_names) })
  

  output$images <- renderUI({
    
   paths <- files()
  # paths<-file_names
    
    paths %>% map(
      function(path){
        renderImage(
          list(
            src = path
            , alt = "Can't show file!"
            , width = 500
            , height = 300
          )
          , deleteFile = FALSE)
      }
    )
  })

   
  ##################### Cette section correspond aux manipulation de donnees necessaire à l'obtention des courbes de rarefaction ######

  bdd_an_cou<- reactive({ # ignore l'année si on conditionne notre variables sur toutes aussi 
    if (select_annee2()!="Toutes") {
      dd_c2<-data() %>% filter(Annee==select_annee2())
    } else {dd_c2<-data()}
    return(dd_c2)
  })

  dd_courbe<-reactive({
  dd_cc<-dcast(bdd_an_cou(), Id_Ech_Annee ~ Nom_latin, value.var = "Counts")
  dd_dd<-dd_cc
  dd_dd<-dd_dd[,2:ncol(dd_dd)]
  dd_dd[is.na(dd_dd)]<-as.numeric(0)
  return(dd_dd)
  })
  
  accum1<-reactive({
  dd_ee<-specaccum(dd_courbe(), method = vari7(), permutations = 100,  #random 
                    conditioned =TRUE, gamma = "jack1",  w = NULL)
  return(dd_ee)}) 
  
  output$plot2<- renderPlot({plot(accum1())# établi la structure du graphique 
  })
  
  ############# Cette section correspond aux manipulation de donnees necessaire a l'obtention des indices de diversité 
  dd_d1<-reactive({dd_bb<-data()
  # dd_bb$Counts<-1
  return(dd_bb)
  })
  
  bdd_d2<- reactive({ # ignore l'année si on conditionne notre variables sur toutes aussi 
    if (select_annee3()!="Toutes") {
      dd_d2<-dd_d1() %>% filter(Annee %in% select_annee3())
    } else {dd_d2<-dd_d1()}
    return(dd_d2)
  })
  
  
  bdd_d3<- reactive({ # ignore le milieu si on conditionne notre variables sur tous aussi 
    if (vari5_b()!="Tous") {
      dd_d2<-bdd_d2() %>% filter(Milieu %in% vari5_b())
    } else {dd_d2<-bdd_d2()}
    return(dd_d2)
  })
  
  bdd_d4<- reactive({ # ignore la methode si on conditionne notre variables sur toutes aussi 
    if (vari6_b()!="Toutes") {
      dd_d2<-bdd_d3() %>% filter(Methode %in% vari6_b())
    } else {dd_d2<-bdd_d3()}
    return(dd_d2)
  })
  
  dd_d3<-reactive({
    dd_cc<-dcast(bdd_d4(), Id_Ech_Annee ~ Nom_latin, value.var = "Counts")
    dd_dd<-dd_cc
    dd_dd<-dd_dd[,2:ncol(dd_dd)]
    dd_dd[is.na(dd_dd)]<-as.numeric(0)
    return(dd_dd)
  })
  
  
   dd_cu1<-reactive({alpha<-specnumber(dd_d3())
  return(alpha)})
  dd_cu2<-reactive({simpson<-diversity(dd_d3(), index='simpson')
                return(simpson)})
  dd_cu3<-reactive({shannon<-diversity(dd_d3(), index='shannon')
                return(shannon)})
  
  dd_index<-reactive({data.frame(ab=dd_cu1(), simpson=dd_cu2(),shannon=dd_cu3())})
  output$table1 <- DT::renderDataTable(DT::datatable({   dd_index() }))
  
  
}




shinyApp(ui, server)
