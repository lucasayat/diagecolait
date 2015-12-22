
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

library(shiny)
library(shinythemes)
library(knitr)
library(markdown)
library(plotrix)
library(shinyTable)
library(gridExtra)

#farm<-readRDS("data/farm.RDS")
posys<-readRDS("ref/posys14")
posat<-readRDS("ref/posat14")

# initialize data with colnames
# df <- data.frame(matrix(c("point","observ","objectif","commentaires"), 1, 4),stringsAsFactors=FALSE)
# colnames(df) <- c("point", "obs","obj","com")

shinyUI(navbarPage("",theme = shinytheme("united"),                          
          tabPanel(h3("Accueil"),
            #h4(tags$img(src="logoreso.jpg", width="100px")),     
             headerPanel("",                        
                         tags$head(                            
                          tags$title(""),                                                      
                          includeCSS("www/style.css"),
                          includeScript("js/verifich1.js"),
                          includeScript("js/verifich2.js"),
                          includeScript("js/newinst.js"),
                          includeScript("js/eliminst.js"),
                          includeScript("js/exemple.js")
                         )),            
                                     
           fluidRow(
                 column(1,    
                        tags$img(src="logoreso.png", width="120px",align="left"), br(),            
                         tags$img(src="logoIE.png", width="100px",align="left"),br(),br(),
                         tags$img(src="logoCA.png", width="100px", align="left")),
                     
                column(6,
                  h1("Diagnostic d'Exploitation Laitière",align="center",style="color:green"),
                  
                br(), 
       fluidRow(column(3),
                column(6,
      h3("Exploitation :",style="color:blue", align="center"),    
        h3(textInput("ferme",label="",value="KINEXISTEPA"), align="center"),
        br(),
        h4("",textOutput("system"),align="center"),
        br(),
        h4( "",textOutput("atel"),align="center"),
      br(),br(),
      h5(dateInput(inputId="debex",label="Date de debut d'exercice",
                   format="dd-mm-yyyy",language="fr",value="2013-01-01"),align="center"),
      h5(textInput("tec",label="Réalisé par :",value=""),align="center"), 
      h5(dateInput(inputId="datereal",label="Le:",format="dd-mm-yyyy",language="fr"),align="center")),
        column(3)
           )),  
        column(4,
               wellPanel(
                h4("Mode saisie ou reprise de données:"),br(),
                h5("1) Fichier csv (séparateur=" , span("virgule",style="color:red"), "decimales=",
                   span("point)",style="color:red")),
               fileInput('recupdon', "Sauvegardes de cette appli ou CSV de fichier OpenOffice",
                         accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
               h5("2) Fichier csv (sép. = ", span("point virgule,",style="color:red"), 
                    "dec. =", span("virgule)",style="color:red")),
               fileInput('recupms', "Exemple, sauvegarde CSV de fichier Excel",
                         accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
               h5("3) Fichier exemple :",
              # actionButton("exemple","Telecharger un fichier exemple.",icon = icon("ambulance"))
          tags$input(type = "button",id = "exemple" ,value="Telecharger un fichier exemple.",icon = icon("ambulance")))   
               ),
          em("Réalise par JL Reuillon Institut de l'élevage, avec l'aide des chambres d'agriculture du Centre et de l'Allier,
                et la contribution financière du CASDAR MAAF. Logiciel sous licence GPL v3.")
           ))),           
     tabPanel(h3("Repères"), 
              #icon=icon("fa fa-flag fa-size=1px"),
              fluidRow(
                column(3,
               wellPanel(
             h6(numericInput("TUMOE","Exploitants",value=1),"UMO",align="right"),
             h6(numericInput("TUMOS","Salariés",value=0),"UMO",align="right"),               
             h6(numericInput("SAU","SAU ",value = 1),"ha",align="right"), 
             h6(numericInput("SFP","SFP",value = 1),"ha",align="right"),
              tags$h6(textOutput("infocv")),          
             h6(numericInput("ugb", "Nbre d'UGB",  value=1),align="right"),
              h6(numericInput("vl", "dont nbre VL", value=1),align="right"),      
             h6(numericInput("va", "dont nbre de VA.", value=0),align="right"),   
               h6(numericInput("eg", "dont nbre de JB",  value=0),align="right"),   
            h6(numericInput("stofou", "Fourrages rec.",  value=1),"TMS/an",align="right"),   
             h6(numericInput("stoma", "Dont mais",  value=0),"TMS/an",align="right"),   
             h6(numericInput("laiprod", "Lait produit",  value=1),"Litres", align="right"),    
              h6(numericInput("laiven", "Lait vendu", value=1),"Litres",align="right"))),                         
             column(4,
            h5(textOutput("alerte1"),style="color:red"),
             h5(textOutput("alerte2"),style="color:red"),    
             wellPanel(h4("Choix du système d'exploitation:",style="color:green"),
            radioButtons ("sys",label="",choices=colnames(posys)[3:8],selected="Lait_CV"),
            em("A partir des renseignements structures, proposition de classement automatique dans le graphique ci-dessous")),
             plotOutput("plot0",width="400px",height="400px")),
            column(5,
                   br(),
            wellPanel(h4("Choix du type d'atelier lait:",style="color:green"),
                radioButtons("blat",label="",choices=colnames(posat)[3:8],selected="plaine_MDS"),
              em("A partir des renseignements structures, proposition de classement automatique dans le graphique ci-dessous")), 
            plotOutput("plot3",width="500px",height="400px"))                 
               )),
         tabPanel(h3("Exploitation"),
        h3("Résultats économiques de l'exploitation",style="color:green")  ,           
        fluidRow(
          column(3,
                 wellPanel(                
                                      numericInput("prilait","Prix du lait (Euros/1000L)",min=100,max=500,value=100,step=1),
                                      numericInput("pvbl","Viande BL (Euros/1000L)",value=1),
                                      numericInput("autprod","Autres produits hors aides",
                                                   value=1),    
                                      numericInput("aid","Aides totales",value=1,min=0,max=150000,step=10),                                
                                      h4("__________________________",style="color:red"),
                                      numericInput("chop","Charges oper.",value=1),                               
                                      h4("__________________________",style="color:red"),
                                      numericInput("chst","Ch.struct.(hors amort, FF)",value=1),
                                     br(),
                                      numericInput("salex","Dont salaires et CS:",value=0),
                                      numericInput("foncier","Dont fermages et IF:",value=0),
                                     h4("__________________________",style="color:red"),
                                     numericInput("msa","Charges soc. exploit.:",value=0),
                                      numericInput("amort","Amortissements totaux:",value=0),                                      
                                      numericInput("annu","Annuites",value=0,min=0,max=200000,step=10),                       
                                      numericInput("fin","Frais financiers",value=0,min=0,max=100000,step=10),
                                     h4("__________________________",style="color:red"),
                                     selectInput ("MSAC",h5("Choix de regle de calcul MSA", style="color:green"),
                                                   c ("Automatique 25% du RCAI)"="MSAA",
                                                      "Saisie manuelle"="MSAM"),selected="MSAA")),
                                     h5(textOutput("msaret"),style="color:blue")),                   
                  
                      column(4,         
                                      plotOutput("plot11",width="400px",height="600px"),
                             h4(textOutput("rcaumo"))),
                            
                                      
                 column(5,
                                      plotOutput("plot12",width="500px",height="80px"),
                                      br(),
                                      plotOutput("plot13",width="500px",height="80px"),
                                      br(),
                                      plotOutput("plot131",width="500px",height="80px"),
                                       br(),
                                      plotOutput("plot14",width="500px",height="80px"),br(),br(),br(),
                        wellPanel(
                        h5("Simulation d'évolution des charges"),
                        sliderInput("indichop","Opérationnelles (%)",value=0,min=-10,max=10,step=1),
                        sliderInput("indistruc","Structurelles (%)",value=0,min=-10,max=10,step=1)))                               
                    )),   
   tabPanel(h3("Atelier lait"),
    tabsetPanel(type="pills",
    tabPanel(title="Coût de production",             
             fluidRow(
               column(3,
                      wellPanel( 
                  h5("Système alimentaire (Euros/1000l)",style="color:green"),             
           h6(numericInput("CP_alim","Achats d'aliments",value=1),align="right"),                            
          
           h6(numericInput("CP_surf","Appro des surfaces",value=1),align="right"),     
          h6(numericInput("CP_meca","Mecanisation",value=1),align="right"),   
          h6(numericInput("amort_meca","Dont amortissements",value=1),align="right"),
             h6(numericInput("CP_fon","Foncier",value=1),align="right"),
              h6(numericInput("rem_fon","Dt remun. fonc. propr.",value=0),align="right"),
             h5("Logement (Euros/1000l)",style="color:green"), 
             h6(numericInput("CP_bat","Batiments et installations",value=0),align="right"),
             h6(numericInput("amort_bat","Dont amortissements",value=0),align="right"),  
            h5("Services (Euros/1000l)",style="color:green"),
             h6(numericInput("CP_fel","Frais d'elevage",value=1),align="right"),                            
           h6(numericInput("CP_gest","Frais divers de gestion",value=1),align="right"),
              h5("Capital (Euros/1000l)",style="color:green"),
              h6(numericInput("CP_cap","Capital",value=0),align="right"),                                
              h6(numericInput("rem_cap","Dt remuneration du capital propre",value=0),align="right")      
              )),                            
          column(4,   
                 plotOutput("plot15",width="450px",height="700px")),                          
          column(3 ,
                 wellPanel(
                h5("Travail exploitants (SMIC/UMO)",style="color:green"),
                   numericInput("travex","",min=0,max=5,value=1.5,step=0.5),
                br(),
                h6( textOutput("CPremunex"),style="color:green"),
                h6( textOutput("CPsal"),style="color:green"),                
                numericInput("annuBL","Annuites BL",min=0,max=200,value=1,step=1),
                         h5("Produits (Euros/1000l)",style="color:green"),   
                        em(" Le prix du lait et le produit viande sont defini dans l'onglet -Exploitation-"),
                numericInput("CP_autprod","Autres produits",min=0,max=100,value=0,step=10),
                        br(),  
                   numericInput("CP_aid","Aides affectees au troupeau laitier",min=0,max=200,value=1,step=1),
                       br(),
                  h5("Prix de revient et de fonctionnement pour:",textOutput("remunex"),style="color:blue"), 
                    h4( textOutput("prirev"),br(),br(),                         
                    textOutput("prif"),style="color:red")))                
                )),   
    
    tabPanel("Travail",
             fluidRow(
               column(3,
                      wellPanel( 
                        img(src="trav.jpg", width="300px"),br(),br(),
                      h4(numericInput("umoexp", "UMO exploitant BL", min=0,max=5,step=0.1, value=1),align='right'),
                      h4(numericInput("umosal","UMO salarie BL", min=0,max=5,step=0.1, value=0),align='right'),
                      h4(numericInput("salait","Salaires pour BL (Euros/an)", min=0,max=100000,step=100, value=0)),
                        sliderInput("umoben","UMO benevole BL", min=0,max=5,step=0.1,value=0)
                     )),                               
                  column(9,
                    plotOutput("plot2",width="600px",height="100px"),   
                     plotOutput("plot1",width="600px",height="480px"))
                     )),    
    
    tabPanel("Produits",
             fluidRow(
               column(5,
                      wellPanel(
                       
                        h4(textOutput('prod'),style="color:red"),  
                     plotOutput("plot40",width="450px",height="100px"),                                        
                        h4("-------------------------Approfondir ------------------------",style="color:red",align="center"),
                        plotOutput("plot50",width="450px",height="80px"),
                        br(),
                        numericInput("TB","TB",value=0),
                        numericInput("TP","TP",value=0),
                        br(),
                        numericInput("cels","Nbre mois > 250 000 cellules",value=-1), 
                        h4("--------------------Produits joints:Viande-------------------------",style="color:red",align="center"),
                        plotOutput("plot4",width="450px",height="400px"),                   
                        h4("------------------------Aides------------------------",style="color:red",align="center"),             
                        plotOutput("plot41",width="450px",height="400px"))), 
               column(7,
                     h3("Produits",style="color:red",img(src="lait.jpg", width="150px")),
                     h5("Donnees complementaires si remplies",align="left"),
                     plotOutput("barprod",width="500px",height="300px"),
                    br(), br(),
                  # h6("Autres facteurs  explicatifs:",style="color:green"),
                   htable("infoprod", colHeaders="provided"),
                   br(),                 
                  
                   h5("Conseils:"),
                   tags$textarea(id="conprod", rows=6, cols=300, "")
                  
                      ))),
    
    tabPanel("Alimentation",
             fluidRow(
               column(5,
      wellPanel(
      h4(textOutput('sysal'),style="color:red",
         img(src="alim.jpg", width="150px")),
      plotOutput("plot70",width="450px",height="100px"),
      h5("-------------------------------Cout alimentaire ------------------------------",style="color:red"),
      plotOutput("plot71",width="450px",height="80px"),
      h5("---------------------------Approfondir -----------------------------------------",style="color:plum"),   
      numericInput("CP_conca","Achat de concentres (Euros/1000l)",value=1),br(),
      numericInput("con","Concentre g/l",value=1),br(),
      numericInput("charg","Chargement (UGB/ha)",value=1),    
      h5("-----------------------------Mecanisation -----------------------------------",style="color:red"),       
      plotOutput("plot72",width="450px",height="80px"),
      h5("---------------------------Approfondir ---------------------------------------",style="color:plum"),
      numericInput("wtiers","Travaux par tiers (Euros/1000L)",value=0),
      numericInput("carbu","Carburants lubrifiants  (Euros/1000L)",value=0),
      numericInput("repa","Reparations du materiel (Euros/1000L)",value=0),
      numericInput("amortmat","Amortissement du materiel (Euros/1000L)",value=0),                  
      h5("------------------------------Foncier ----------------------------------",style="color:red"),
      plotOutput("plot73",width="450px",height="80px")),
      br(),br(),
      #h6("Autres facteurs explicatifs:",style="color:green"),
      htable("infosalim", colHeaders="provided")
      ),   
         column(7,
                plotOutput("radaralim",width="600px",height="600px"),
                br(),
                plotOutput("baralim",width="500px",height="600px")
            
         ))), 
                 
    tabPanel("Logement",
             fluidRow(
               column(5,         
                      wellPanel(
                        h4(textOutput("coubat"),style="color:red",
                           img(src="bat.jpg", width="100px")),  
                        plotOutput("plot75",width="450px",height="100px"),
                        h5("-------------------------------Approfondir --------------------------",style="color:plum"),               
                        numericInput("eau","Eau",value=0),br(),
                        numericInput("edf","EDF",value=0),br(),
                        numericInput("enbat","Entretien batiment",value=0),br(),
                        numericInput("amobat","Amortissement batiments",value=0))
                     
                  
                      ),
               column(7,
                      plotOutput("barbat",width="500px",height="400px"),
                      br(),br(),
                     # h6("Autres facteurs explicatifs:",style="color:green"),
                      htable("infobat", colHeaders="provided")
                      # tableOutput("infobat")
               ))), 
    tabPanel("Services et capitaux",
             fluidRow(
               column(5,  
                      wellPanel(
                        h4(textOutput('couserv'),style="color:red",img(src="serv.jpg", width="150px")),    
                        plotOutput("plot80",width="450px",height="100px"),
                        h5("-----------------------------Approfondir -------------------------",style="color:plum"),
                        numericInput("fel","Frais d'elevage",value=0),br(),
                        numericInput("veto","Frais veterinaires",value=0)),                            
                      wellPanel(
                        h4(textOutput('coucap'),style="color:red",img(src="cap.jpg", width="100px")),    
                        plotOutput("plot90",width="450px",height="100px"),
                        h5("-------------approfondir si besoin -----------------",style="color:plum"),
                        numericInput("ff","Frais financiers",value=10))
                        
                      ), 
               column(7,
                      plotOutput("barsercap",width="500px",height="400px"),
                      br(),br(),
                     # h6("Autres facteurs explicatifs:",style="color:green"),
                      htable("infosercap", colHeaders="provided")
                        
                    ))))), 
   
#     tabPanel("Radars",
#              fluidRow(
#                column(3,  
#                       wellPanel(
#                         selectInput ("varset",h4("Variables a analyser", style="color:green"),
#                                      choices= c("Produits"="prod",
#                                                 "Systeme alimentaire"="salim",
#                                                 "Cout du logement" ="bat",         
#                                                 "Cout des services"="sercap"),
#                                                   ,selected="prod"))),
#                       
#                   column(6,
#                  plotOutput("radar",width="600px",height="600px"))))),
#      

    
#     tabPanel(h3("Outils"),
#              tabsetPanel(
             navbarMenu(h3(""),
                        icon=icon("fa fa-wrench fa-2x"), 
               tabPanel(title="Instantanes",
                        tags$input(type = "button",id = "newinst" ,value="Prendre un instantane"),
                        tags$input(type = "button",id = "eliminst" ,value="Supprimer un instantane"),
                        downloadButton("recupinst","sauvegarder tableau en csv"),
                        br(),br(),br(),
                        tableOutput("instant")
               ),
               
               tabPanel("Editions et sauvegardes",
                        fluidRow(
                          column(3,
                                 wellPanel(        
             h5("Edition fichier PDF du Diagnostic de l'exploitation",style="color:blue",
              downloadButton('report1',"Diagnostic de l'exploitation"),align="right"),
             br(),br(),
             h5("Edition PDF des points de vigilance et conseils",style="color:blue"),
             downloadButton('recompdf', 'Edition des points de vigilance'))
             ),
             column(3,
                    wellPanel( 
                      h5("Sauvegarde fichier CSV",style="color:red"),
                      downloadButton("savecsv","Sauvegarde CSV des données"),
                      
                      br(),br(),
                      h5("Sauvegarde csv des recommandations",style="color:red"),
                      downloadButton('recomcsv', 'Fichier des recommandations'))),
                        
             column(2,
                    wellPanel(  
             h5("Edition d'une fiche structure pour positionnement du systeme 
                de l'exploitation et de l'atelier lait ",style="color:blue"),
             br(),
             downloadButton('report2',"Typo (vierge)"),align="right")),
                    column(3,
                    wellPanel( 
             h5("Edition de fiches reperes exploitation et atelier",style="color:blue"), 
          
             h6("Choix du systeme d'exploitation",style="color:green",selectInput ("blansys",label="",choices=colnames(posys)[3:8],selected="Lait_CV")),
             br(),
             
             downloadButton('report3',"Edition de la fiche exploitation"),
             br(),
             br(),
             h6("Choix du type d'atelier",style="color:green",selectInput("blanblat",label="",choices=colnames(posat)[3:8],selected="plaine_MDS")),
            br(),
           
            downloadButton('report4',"Edition de la fiche atelier lait"),
            br(),
            br(),
              downloadButton('report5',"Edition donnees complementaires atelier")))        
            ))),
             
       tabPanel(em(textOutput("nom"),style="color:white;"),
                fluidRow(
                  column(5,
                         wellPanel(
                           h2("Exploitation"),
                           tableOutput("vartab1"))), 
                  column(4,
                         wellPanel(
                           h2("Atelier lait"),        
                           tableOutput("vartab2")))))
                

  
))
        

