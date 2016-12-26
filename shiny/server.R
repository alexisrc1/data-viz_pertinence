#package installe
library(shiny)
library(plotly)
library(prim) 
library(evtree)

options(shiny.maxRequestSize = 23*1024^2) 
#si le fichier est trop lourd et n'est pas supporte par Shiny, mettre une puissance plus elevee

#ouverture du fichier
shinyServer(function(input, output) {
  data<-reactive({
    inFile<-input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header = input$header,sep = ";")   
    #NB : la fonction reactive permet de creer des variables reutilisables ailleurs dans le code
  })#pour la variable "data", elle est reutilisee ailleurs sous la forme data(), il ne faut pas oublier de mettre les parentheses
  
  
  #variable "contents" contenant la premiere ligne du tableau  
  output$contents <- renderTable({
    head(data(),1)
  })
  
  #variable "sum" contenant le summary devant etre affiche
  output$sum<-renderPrint({
    if (is.null(data()))
      return("merci de choisir un tableau")
    for (i in names(data())){
      if(class(data()[,i])=="factor"){
        print(c(paste("cette variable n'est pas quantitative :",i)))
        
      }
      else{
        affiche_moy<-paste(paste("moyenne de",i),round(mean(data()[,i],na.rm=T),6))
        affiche_max<-paste(paste("valeur maximum de",i),round(max(data()[,i],na.rm=T),6))
        affiche_min<-paste(paste("valeur minimum de",i),round(min(data()[,i],na.rm = T),6))
        print(c(affiche_moy,affiche_min,affiche_max))
      }
    }
  })
  
  #variable indiquant les variables non qualitatives et affichant la repartition de celles-ci
  output$quali<-renderPrint({
    for (i in names(data())){
      if(class(data()[,i])=="factor"){
        print(c(paste("cette variable n'est pas quantitative :",i)))
        print(table(data()[,i]))
      }
    }
  })
  
  #les Sliders de l'interface (en premier le nom de la variable dans laquelle est stockee la valeur prise par le slider, puis le texte)
  output$slider_colonne <- renderUI({
    selectInput("var", "quelle colonne visualiser ?",choices=c(names(data())),selectize = TRUE)
  }) #je l'ai appele slider_... alors que ce sont des combos box avec le nom des colonnes du tableau sur lequel on travaille
  
  output$slider_explic <- renderUI({
    selectInput("var_explic", "quelles colonnes pour les variables explicatives ?",choices=c(names(data())),selectize = TRUE,multiple = TRUE)
  })
  
  output$slider_abscisse <- renderUI({
    selectInput("var1", "quelle colonne en abscisse ?",choices=c(as.character(input$var_explic)),selectize = TRUE)
  })
  #la var1 est le nom de la variable : c'est la colonne choisie en abscisse 
  
  output$slider_ordonnee <- renderUI({
    selectInput("var2", "quelle colonne en ordonnee ?",choices=c(as.character(input$var_explic)),selectize = TRUE)
  })
  
  output$slider_reponse <- renderUI({
    selectInput("reponse", "variable reponse",choices=c(names(data())),selectize = TRUE)
  })
  
  output$slider_visual<-renderUI({
    selectInput("var_visual","variables dans PRIM",choices=c(as.character(input$var_explic)),selectize = TRUE)
  })
  
 
  
  
  #algorithme de PRIM
  
  fctprim<-reactive({
    prim.box(x=na.omit(data()[,as.character(input$var_explic)]),y=na.omit(data()[,as.character(input$reponse)]),peel.alpha=input$alpha,mass.min = input$beta,threshold=c(0.5,0.5),threshold.type=0)
  })
  #creation de la variable fctprim() qui utilise des variables definies precedemment, le na.omit permet d'enlever les valeurs manquantes
  #on remarque l'utilisation de la syntaxe input$... pour faire intervenir les variables contenues dans les sliders
  
  #variable "prim" dans laquelle les boites de l'algo sont stockees
  output$prim<-renderPrint({
    summary(fctprim(),print.box=T) 
  })
  
  #variable dans laquelle les graphiques sont stockees
  output$affiche_prim<-renderPlot({
    plot(fctprim()) 
  })
  
  #variable qui permet de savoir quelles variables donnees en entree de l'algorithme de PRIM ont ete effectivement pelees 
  output$var_pel<-renderPrint({
    for (i in names(as.data.frame(fctprim()$box[[input$num_box]]))){
      if(min(data()[,i],na.rm=T)<=fctprim()$box[[input$num_box]][1,i] | max(data()[,i],na.rm=T)>=fctprim()$box[[input$num_box]][2,i]){
        print(c(paste("cette variable a une influence:",i)))
        print(sprintf("PRIM travaille sur %1.0f lignes du fichier, regle : entre %.3f et %.3f",nrow(data()[!is.na(data()[,i]),]),
                      max(min(data()[,i],na.rm=T),round(fctprim()$box[[input$num_box]][1,i],6)),min(max(data()[,i],na.rm = T),round(fctprim()$box[[input$num_box]][2,i],6))))
      }
      
    }
  })
  
 
  
  
  output$taille_boite<-renderPrint({
    data2<-data()
    for (i in names(as.data.frame(fctprim()$box[[input$num_box]]))){
      data2<-subset(data2,fctprim()$box[[input$num_box]][2,i]>=data2[,i] & fctprim()$box[[input$num_box]][1,i]<=data2[,i])
    }
    print(sprintf("la boite %1.0f contient %1.0f points et sa moyenne est %.3f",input$num_box,nrow(data2),mean(data2[,input$reponse],na.rm=T)))
   
    }) #affichage du resultat donnee par la boite visualisee 
  
  
  
  minbox<-reactive({
    fctprim()$box[[input$num_box]][1,input$var_visual]
  })
  maxbox<-reactive({
    fctprim()$box[[input$num_box]][2,input$var_visual]
  }) #minbox et maxbox sont, pour une variable donnee, les limites de la boite choisie (boite 1,2,3...).
  #on choisit la boite que l'on veut dans l'interface (menu deroulant) (variable :"num_box")
  
  
  #variables pour l'onglet "correlations?", chacune correspond au slider de l'onglet correlation
  x<-reactive({
    data()[,input$var1]
  })
  y<-reactive({
    data()[,input$var2]
  })
  z<-reactive({
    data()[,input$reponse]
  })
  
  #tracer les points 1 et 0 : il s'agit du dessin des boites, avec des points de couleur differentes (une couleur pour 0 et une pour 1)
  output$g<-renderPlotly({
    x<-list(title=input$var1)
    y<-list(title=input$var2)
    p<-plot_ly(data = data(),x=x(),y=y(),mode="markers",color=z(),colors = c("blue","red"))
    p<-layout(p,title="correlations entre variables",xaxis=x,yaxis=y,shapes=list(
      list(type="rect",fillcolor="blue",line=list(color="black"),opacity=0.2,x0=max(fctprim()$box[[input$num_box]][1,input$var1],min(x(),na.rm=T)),x1=min(max(x(),na.rm=T),fctprim()$box[[input$num_box]][2,input$var1]),xref="3",y0=max(min(y(),na.rm=T),fctprim()$box[[input$num_box]][1,input$var2]),y1=min(max(y(),na.rm=T),fctprim()$box[[input$num_box]][2,input$var2]),yref="1")))
    p
  })  #affichage du graphe
  
  
  
  
  #les histogrammes
  
  histo<-reactive({hist(data()[,as.character(input$var_visual)],breaks=100) #choix de la variable (slider sur le cote)
  })
  bor<-reactive({ifelse(data()[,input$var_visual]<minbox()|data()[,input$var_visual]>=maxbox(),"hors de la boite ","dans la boite ") #condition sur le coloriage des traits
  })
  bor1<-reactive({ifelse(histo()$mids>maxbox()|histo()$mids<minbox(),"red","green") #condition sur le coloriage des barres de l'histogramme
  })
  output$e<-renderPlot({
    plot(histo(),col=bor1(),main="histogramme bicolore",xlab=input$var_visual)
  })
  
  
  output$e_sans_couleur_histo<-renderPlotly({
    if (is.null(data()))
      return(NULL)
    x<-list(title=input$var)
    plot_ly(x=data()[,input$var],color=as.character(data()[,input$reponse]),type = "histogram") %>% layout(title="histogramme des proportions",xaxis=x)
  }) #histogramme du second onglet, pour la variable choisie dans le menu de gauche
  
  
  
  output$e_boite<-renderPlotly({
    x<-list(title=input$var)
    plot_ly(x=data()[,input$var],color=as.character(data()[,input$reponse]),type="box") %>% layout(title="boites-moustaches pour 0 et 1",xaxis=x)
  }) #"boite a moustaches"
  
  
  #arbre de decision, ne s'en servir que pour les variables qualitatives avec peu de modalites pour que l'arbre soit lisible
  output$e_boite1<-renderPlot({
    fit<-evtree(na.omit(data()[,input$reponse])~na.omit(data()[,input$var]),data=data())
    plot(fit)
  }) 
  
  
  output$f<-renderTable({
    a=as.character(input$var)
    data()[a]}) #affichage de la colonne choisie
  
  
  output$h<-renderPlotly({
    x<-list(title=input$var_visual)
    y<-list(title="")
    plot_ly(histo(),x=as.character(c(1:nrow(data()))),y=data()[,as.character(input$var_visual)],color=bor(),type="bar") %>% layout(title="analyse ligne par ligne",xaxis=x,yaxis=y) 
  })
})#affichage de l'histogramme dans l'onglet des resultats de l'algorithme de PRIM