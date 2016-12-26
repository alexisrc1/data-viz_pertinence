#chargement des packages necessaires
library(shiny)
library(plotly)
library(prim)
library(evtree)

shinyUI(fluidPage(
  titlePanel("PRIM pour HUTCHINSON"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'cliquez pour choisir un fichier (.csv)',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain', 
                  '.csv',
                  '.tsv')
      ),
      
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      
#affichage des sliders dans le panel de gauche, le nom des sliders correspond aux noms donnes dans le "server.R"      
      
      uiOutput("slider_reponse"),

      uiOutput("slider_colonne"),
      
      
      uiOutput("slider_explic")
    ),

#Panel central

    mainPanel(
      #affichage dans le "mainPanel"
      
      tableOutput("contents"),
      
      #affichage dans les onglets
      tabsetPanel(type="tab",
                  tabPanel("summary",verbatimTextOutput("sum")),
                  tabPanel("colonne selectionnee",sidebarPanel(tableOutput("f")),mainPanel(plotlyOutput("e_sans_couleur_histo"),plotlyOutput("e_boite"))),
                  tabPanel("Prim",
                           sidebarPanel(sliderInput("alpha","critere de pelage (augmenter progressivement)",min=0,max=1,value=0.05,step=0.05),
                                        sliderInput("beta","quelle population minimale souhaitez-vous pour la boite finale?",min=0,max=1,value=0.1)),
                           mainPanel(verbatimTextOutput("prim"),plotOutput("affiche_prim"))),
                  tabPanel("visualisation des resultats",
                           numericInput("num_box","quelle boite voulez-vous etudier?",1),
                           uiOutput("slider_visual"),
                           verbatimTextOutput("var_pel"),
                           verbatimTextOutput("taille_boite"),
                           plotOutput("e"),plotlyOutput("h")),
                  tabPanel("correlations ?",uiOutput("slider_abscisse"),uiOutput("slider_ordonnee"),plotlyOutput("g")),
                  
                  tabPanel("information variables qualitatives",verbatimTextOutput("quali"),plotOutput("e_boite1"))
                           
      )
    )
  )
))




