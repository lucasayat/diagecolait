
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

#### Diag BL version 1.4 du 05/01/2015
#  rm(list=ls())
#     setwd("G:/Documents/AR/diaginosys")
# library(shinyapps)
#    
# shiny::runApp("~/Documents/AR/diaginosys")

SMIC<-13545### SMIC net 2014
posys<-readRDS("ref/posys14")
posat<-readRDS("ref/posat14")
cplnat<-readRDS("ref/cplnat14")

source("www/plotdiag.R")

inst <-readRDS("data/inst.RDS")
write.csv(inst,file="data/inst.csv",row.names=FALSE)
# cplnat13<-readRDS("ref/cplnat13")
# cplnat<-readRDS("ref/cplnat14")
vardiap<-c( "STRUC_NOMEXP" , "STRUC_DEXERCI", "STRUC_NOMDEP" , "STRUC_LIBCT",              
             "STRUC_TATBL", "STRUC_MOFAMI" ,  "STRUC_MOSAL" ,  "STRUC_MOBENE" ,             
             "FONC1_TSAU" ,    "FONC1_HASFP"   , "SYSFOU_QDTOT" ,  "SYSFOU_QRMAIS" ,             
            "FONC1_TOTUGB" ,  "FONC1_NBVL",   "FONC1_NBVA" ,   "FONC1_JBFININT",             
                "DTBL_QLAITP",   "ECOPA_AIDTOT" ,  "ECOPA_TOTPRO",  "ECOCO_TOTCO" ,               
 "ECOCSTB_TCSHAF" ,             "ECOCSTB_SAL" ,                "ECOCSTB_FONCIER",             "ECOCSTB_AEMPLMT",            
 "ECOCSTB_TOTAM",               "ECOCSTB_MSA" ,                "ECOCSTB_FFLMT" ,      "DTBLCP_BL_LAITCOM"  ,        
 "DCOMP_MOFAMCLEBL"  ,          "DCOMP_MOSALCLEBL" ,           "DTBLCP_BLCP_PRODLAIT"  ,      "DTBLCP_BLCP_PRODVIANDE" ,    
 "DTBLCP_BLCP_AUTPROD" ,        "DTBLCP_BLCP_AIDES" ,          "DTBLCP_BLCP_APPANI",          "DTBLCP_BLCP_APPANI_CONCA" ,  
 "DTBLCP_BLCP_APPSURF",         "DTBLCP_BLCP_FELEVA",          "DTBLCP_BLCP_MECA" ,           "DTBLCP_BLCP_MECA_AMORT" ,    
 "DTBLCP_BLCP_BATI",            "DTBLCP_BLCP_BATI_AMORT" ,     "DTBLCP_BLCP_FGEN" ,           "DTBLCP_BLCP_FONCAP_FERMAREL",
 "DTBLCP_BLCP_FONCAP_AMORT",    "DTBLCP_BLCP_FONCAP_FERMACAL", "DTBLCP_BLCP_FONCAP_FF",       "DTBLCP_BLCP_FONCAP_REM",     
"DTBLCP_BLCP_TRAV_REMTE" ,     "DTBLCP_BLCP_TRAV_SAL" ,       "DTBLCP_BLCP_AMORT" ,          "DTBLCP_BLCP_CHARSUP",        
 "DTBLCP_BLCP_ANNUITES" ,       "DTBL_TBMOY"  ,                "DTBL_TPMOY"  ,                "DTBL_QCL",                   
 "DTBLCP_BLCP_MECA_TPT"  ,      "DTBLCP_BLCP_MECA_CARBULUB" ,  "DTBLCP_BLCP_MECA_ENTMAT",     "DTBLCP_BLCP_BATI_EAU",       
 "DTBLCP_BLCP_BATI_ELECGAZ" ,   "DTBLCP_BLCP_BATI_ENTLOC" ,    "DTBLCP_BLCP_FELEVA_VETO" ,    "DTBLCP_BLCP_FELEVA_REPIDECP")
q<- data.frame(Point = c("Point de vigilance"),
               Exploitation = c("Aujourd'hui"), 
               Objectif = c(""),
                Commentaires =c(""),
               stringsAsFactors = FALSE)
saveRDS(q,"data/recomprod")
saveRDS(q,"data/recomalim")
saveRDS(q,"data/recombat")
saveRDS(q,"data/recomsercap")

shinyServer(function(input, output, clientData, session) {

 
  farm<-reactive ({ 
    input$recupdon
    input$recupms
    
    if(!is.null(input$ex))
    {
      if(input$ex == "on")
      {
        newfarm<-read.csv("data/exemple.csv",stringsAsFactors=FALSE)
        return(newfarm)
        
      }}
       
    
    if(!is.null(input$recupdon))
    {
    inFile <- input$recupdon
       
    if (is.null(inFile))
      
      return(NULL)
      
    newfarm<-read.csv(inFile$datapath)

   virgul<-FALSE
   #farm<-readRDS("data/farm.RDS")
   if (identical(colnames(newfarm),vardiap)==FALSE )
    { mes1<-"Fichier non conforme !  
             Erreurs possibles: 
          1- Format du fichier csv:  Separateurs des champs (;) au lieu de (,) 
             verifier et modifier
          2- Nom des variables non conformes: Modifier le nom des variables,
             qui doivent etre identiques en nombre et noms au dossier:
             (exemple.csv) a telecharger sur outils/sauvegardes et a
               ouvrir avec  OpenOffice.
          3- Changer et reessayer."
     session$sendCustomMessage(type='verif1', mes1)
      }else{
        for (i in 6:length(newfarm))
        {
          virg<-grep(",",newfarm[,i])
          if(length(virg)>0) { if (virg>0) virgul<-TRUE }
          
        }
        
   
     if ( virgul == TRUE)
     
      { mes2<-"Le delimitateur des decimal parait etre la (,)
              il faut le remplacer par des (.) 
               puis reessayer"
     session$sendCustomMessage(type='verif2', mes2)
      
      }else{
     return(newfarm)

    }}}

  if(!is.null(input$recupms))
        {
   inFile <- input$recupms
   if (is.null(inFile))
     return(NULL)
   
    newfarm<-read.csv2(inFile$datapath)
   virgul<-FALSE
   #farm<-readRDS("data/farm.RDS")
   if (identical(colnames(newfarm),vardiap)==FALSE )
   { mes1<-"Fichier non conforme !  
     Erreurs possibles: 
     1- Format du fichier csv:  Separateurs des champs (,) au lieu de (;) 
      il faut le remplacer par des (;), à partir d'un fichier Excel
         sauvegarder sous CSV separateurs points virgules.
     2- Délimitateur de décimales est le point. Le remplacer par des vigules.
     3- Nom des variables non conformes: Modifier le nom des variables,
     qui doivent etre identiques en nombre et noms au dossier:
     (exemple.csv) a telecharger sur outils/sauvegardes et a
     ouvrir avec  OpenOffice.
     4- Changer et reessayer."
     session$sendCustomMessage(type='verif1', mes1)
       
     }else{
       # saveRDS(newfarm, file="data/farm.RDS") 
       return(newfarm)
       #      inst <-cbind(readRDS("data/inst.RDS")$x,tabini(farm=newfarm))
       #      write.csv(inst,file="data/inst.csv",row.names=FALSE) 
      }}
     
   })
  
##########nouvelles ferme

observe({
  farm<-farm()
  if(!is.null(farm()))
  {
    updateTextInput(session,"ferme",label="",value=farm$STRUC_NOMEXP) 
    updateTextInput(session,"tec",label="",value=farm$STRUC_NOMDEP)
    updateDateInput(session,"debex",value=farm$STRUC_DEXERCI)
    updateNumericInput(session,"TUMOE",value=farm$STRUC_MOFAMI)
    updateNumericInput(session,"TUMOS",value=farm$STRUC_MOSAL)              
    updateNumericInput(session,"SAU",value = round(farm$FONC1_TSAU,0)) 
     updateNumericInput(session,"SFP",value = round(farm$FONC1_HASFP,0))        
    updateNumericInput(session,"ugb",   value=farm$FONC1_TOTUGB)
     updateNumericInput(session,"vl" , value=farm$FONC1_NBVL)     
      updateNumericInput(session,"va",  value=farm$FONC1_NBVA)   
    updateNumericInput(session,"eg",  value=farm$FONC1_JBFININT)  
     updateNumericInput(session,"stofou",   value=farm$SYSFOU_QDTOT)  
     updateNumericInput(session,"stoma",  value=farm$SYSFOU_QRMAIS)  
     updateNumericInput(session,"laiprod",  value=farm$DTBL_QLAITP)   
    updateNumericInput(session,"laiven", value=farm$DTBLCP_BL_LAITCOM) 
 
    updateNumericInput(session,"prilait",value=round(farm$DTBLCP_BLCP_PRODLAIT,0))
    updateNumericInput(session,"pvbl",value=round(farm$DTBLCP_BLCP_PRODVIANDE,0))
    updateNumericInput(session,"autprod",
        value=round((farm$ECOPA_TOTPRO-farm$ECOPA_AIDTOT-(farm$DTBLCP_BLCP_PRODLAIT+farm$DTBLCP_BLCP_PRODVIANDE)*farm$DTBLCP_BL_LAITCOM/1000),0))   
    updateNumericInput(session,"aid",value=round(farm$ECOPA_AIDTOT,0))                                
    updateNumericInput(session,"chop",value=round(farm$ECOCO_TOTCO,0))
    updateNumericInput(session,"chst",value=round(farm$ECOCSTB_TCSHAF,0))
    updateNumericInput(session,"salex",value=round(farm$ECOCSTB_SAL,0))
    updateNumericInput(session,"foncier",value=round(farm$ECOCSTB_FONCIER,0))
    updateNumericInput(session,"msa",value=farm$ECOCSTB_MSA)
    updateNumericInput(session,"amort",value=round(farm$ECOCSTB_TOTAM,0))                                      
    updateNumericInput(session,"annu",value=round(farm$ECOCSTB_AEMPLMT,0))                      
    updateNumericInput(session,"fin",value=round(farm$ECOCSTB_FFLMT,0))
    
    
   
    updateNumericInput(session,"CP_alim","Achats d'aliments",value=round(farm$DTBLCP_BLCP_APPANI,0))                          
    updateNumericInput(session,"CP_surf","Appro des surfaces",value=round(farm$DTBLCP_BLCP_APPSURF,0))    
    updateNumericInput(session,"CP_meca","Mecanisation",value=round(farm$DTBLCP_BLCP_MECA,0))  
  updateNumericInput(session,"amort_meca","Dont amortissements",value=round(farm$DTBLCP_BLCP_MECA_AMORT,0))
    updateNumericInput(session,"CP_fon","Foncier",value=round(farm$DTBLCP_BLCP_FONCAP_FERMAREL+farm$DTBLCP_BLCP_FONCAP_AMORT+farm$DTBLCP_BLCP_FONCAP_FERMACAL,0))
    updateNumericInput(session,"rem_fon","Dt remun. fonc. propr.",value=round(farm$DTBLCP_BLCP_FONCAP_FERMACAL,0))
    updateNumericInput(session,"CP_bat","Batiments et installations",value=round(farm$DTBLCP_BLCP_BATI,0))
    updateNumericInput(session,"amort_bat","Dont amortissements",value=round(farm$DTBLCP_BLCP_BATI_AMORT,0))  
   updateNumericInput(session,"CP_fel","Frais d'elevage",value=round(farm$DTBLCP_BLCP_FELEVA,0))                           
    updateNumericInput(session,"CP_gest","Frais divers de gestion",value=round(farm$DTBLCP_BLCP_FGEN,0))
  updateNumericInput(session,"CP_cap","Capital",value=round(farm$DTBLCP_BLCP_FONCAP_FF+farm$DTBLCP_BLCP_FONCAP_REM,0))                                
  updateNumericInput(session,"rem_cap","Dt remuneration du capital propre",value=round(farm$DTBLCP_BLCP_FONCAP_REM,0))     
  
  
updateNumericInput(session,"travex",value=1.5)          
updateNumericInput(session,"annuBL","Annuites BL",value=round(farm$DTBLCP_BLCP_ANNUITES,0))
updateNumericInput(session,"CP_autprod","Autres produits",value=farm$DTBLCP_BLCP_AUTPROD)  
updateNumericInput(session,"CP_aid","Aides affectees au troupeau laitier",value=round(farm$DTBLCP_BLCP_AIDES,0))

updateSliderInput(session,"umoexp", value=farm$DCOMP_MOFAMCLEBL) 
updateSliderInput(session,"umosal", value=farm$DCOMP_MOSALCLEBL)
updateSliderInput(session,"salait", value=round(farm$DTBLCP_BLCP_TRAV_SAL*farm$DTBLCP_BL_LAITCOM/1000,0))


updateNumericInput(session,"TB",value=farm$DTBL_TBMOY)
updateNumericInput(session,"TP",value=farm$DTBL_TPMOY)


updateNumericInput(session,"CP_conca",value=round(farm$DTBLCP_BLCP_APPANI_CONCA),0)
updateNumericInput(session,"con",value=farm$DTBL_QCL)
updateNumericInput(session,"charg",value=farm$FONC1_TOTUGB/farm$FONC1_HASFP)    
updateNumericInput(session,"wtiers",value=farm$DTBLCP_BLCP_MECA_TPT)
updateNumericInput(session,"carbu",value=farm$DTBLCP_BLCP_MECA_CARBULUB)
updateNumericInput(session,"repa",value=farm$DTBLCP_BLCP_MECA_ENTMAT)
updateNumericInput(session,"amortmat",value=farm$DTBLCP_BLCP_MECA_AMORT)        

updateNumericInput(session,"eau",value=farm$DTBLCP_BLCP_BATI_EAU)
updateNumericInput(session,"edf",value=farm$DTBLCP_BLCP_BATI_ELECGA)
updateNumericInput(session,"enbat",value=farm$DTBLCP_BLCP_BATI_ENTLOC)
updateNumericInput(session,"amobat",value=farm$DTBLCP_BLCP_BATI_AMORT)

updateNumericInput(session,"fel",value=farm$DTBLCP_BLCP_FELEVA_REPIDECP)
updateNumericInput(session,"veto",value=farm$DTBLCP_BLCP_FELEVA_VETO)                          
updateNumericInput(session,"ff",value=round(farm$ECOCSTB_FFLMT,0))

  }

  
})


#########
   
   
   observe ({       
     inFile <- input$recupfarm
     if (is.null(inFile))
       return(NULL) 
     farm<-read.csv2(inFile$datapath)
     saveRDS(farm, file="data/farm.RDS")
     
     
   })
   
    reperex <- reactive({
        which(colnames(posys) == input$sys)
    })
    
    output$system <- renderText({     
        sys <- sys(input$sys)
        return(sys)
        
    })
    
      
    reperat <- reactive({
     
        which(colnames(posat) == input$blat)
    })
    
    output$nom <- renderText({
         sys<-sys(input$sys)
         atel<-atel(input$blat)
        paste(input$ferme, "-", sys, "- ", atel)
    })
    
    
    output$atel <- renderText({
        atel <- atel(input$blat)
        return(atel)
        
    })
    
    
    
    CP_travex <- reactive({
        travex <- 1000 * (input$travex * input$umoexp * SMIC * 1.3)/input$laiven
        return(travex)
    })
    
    CP_sal <- reactive({
        sale <- input$salait * 1000/input$laiven
        return(sale)
    })
    
    msa <- reactive({
        
        msacal<-ifelse(rcai()*0.25<3500*input$TUMOE,3500*input$TUMOE,rcai()*0.25)
        msa <- switch(input$MSAC,
                      MSAA = msacal, 
                      MSAM = as.numeric(gsub(",", ".", input$msa)))
        msa
    })
    
    output$msaret<-renderText({
      msaret<-paste("Montant MSA retenu :",round(msa(),0),"Euros")
      return(msaret)
    })
   
    rcai <- reactive({
        
        prodlait <- input$prilait * input$laiven/1000
        prodviabl <- input$pvbl * input$laiven/1000
        aid <- input$aid
        autprod <- input$autprod
        prod <- prodlait + prodviabl + autprod + aid
        ebe <- prod - input$chop - input$chst
        rcai <- ebe - input$amort - input$fin
        rcai
    })
    
   output$rcaumo <- renderText({
     rcaumo<-paste("RCAI :",round(rcai()/input$TUMOE)," Euros/UMO exploitant")
     return(rcaumo)
   
     })
   
    output$infomo <- renderText({
        print(" ")
    })
    
    output$infocv <- renderText({
        CV <- as.integer(input$SAU - input$SFP)
        ifelse(CV < 0, "Attention SFP > à SAU !", paste("Cultures de vente = ", CV, " ha "))
    })
    
    
    output$info <- renderText({
        paste(paste("Lait produit par an = ", as.integer(input$vl * input$rdt)), " Litres ", 
            paste("Lait vendu par an = ", as.integer(input$laiven), "Litres"))
        
    })
    
    
    output$alerte1 <- renderText({
        ifelse(input$laiven > input$laiprod, "Lait vendu > lait produit !", "")
    })
    
    
    output$alerte2 <- renderText({
        
        ifelse(input$vl > input$ugb, "Plus de VL que d'UGB !", " ")
        
    })
    
    output$infolait <- renderText({
        PCLAI <- as.integer(100 * input$laiven/input$laiprod)
        ifelse(input$laiven > input$laiprod, "Attention lait vendu > produit!", paste(PCLAI, 
            "% de lait vendu /lait produit"))
    })
    
    output$CPsal <- renderText({
        sal <- paste("Salaire BL:", round(1000 * input$salait/input$laiven, 0), "Euros/1000l")
        sal
    })
    
    output$remunex <- renderText({
        remunex <- paste(input$travex, " SMIC/UMO")
        remunex
    })
    
    output$CPremunex <- renderText({
        CPremunex <- paste("Travail expl.:", round(1000 * input$travex * SMIC * 1.3 * input$umoexp/input$laiven, 
            0), "Euros/1000l")
        CPremunex
    })
    
    
    output$prirev <- renderText({
        CPBL <- input$CP_alim + input$CP_surf + input$CP_fel + input$CP_meca + input$CP_bat + 
            input$CP_gest + input$CP_fon + input$CP_cap + CP_travex() + CP_sal()
        PRIREV <- CPBL - input$pvbl - input$CP_aid - input$CP_autprod
        prirev <- paste("Prix de revient: ", round(PRIREV, 0), " Euros/1000l")
        prirev
    })
    
    
    output$prif <- renderText({
        CPBL <- input$CP_alim + input$CP_surf + input$CP_fel + input$CP_meca + input$CP_bat + 
            input$CP_gest + input$CP_fon + input$CP_cap + CP_travex() + CP_sal()
        amort_meca <- as.numeric(gsub(",", ".", input$amort_meca))
        CP_bat <- input$CP_bat
        amort_bat <- input$amort_bat
        rem_fonc <- input$rem_fon
        rem_cap <- as.numeric(gsub(",", ".", input$rem_cap))
        PRIF <- CPBL - input$pvbl - input$CP_aid - -input$CP_autprod - amort_meca - amort_bat - 
            rem_fonc - rem_cap + input$annuBL-input$CP_cap+input$rem_cap
        prif <- paste("Prix de fonct. : ", round(PRIF, 0), " Euros/1000l")
        prif
    })
    
    output$aid <- renderText({
        aid <- paste("Aides affectées au lait: ", input$CP_aid, " Euros/1000l")
        return(aid)
    })
    
    
    
    var <- reactive({
        data.frame(variable = c("Nom exploitation", "Date début compta","Technicien", "Systeme d'exploitation", 
            "Type d'atelier", "UMO exploitant", "UMO salaries", "UMO benevoles BL", "SAU", "SFP", 
            "Fourrages conserves(TMS/an)", "Mais recolte(TMS)", "UGB", "VL", "Nombre de VA", 
            "Nbre de JB vendus/an", "Lait produit", "Aides totales", "Produit Brut", "Charges variables", 
            "Charges de str. hors amort. et FF", "Salaires expl.", "Foncier", "Annuites", 
            "amortissements", "MSA", "Frais financiers", "lait commercialise", "UMO exploitant BL", 
            "UMO salarie BL", "prix du lait", "produit viande", "Autre produits BL", "aides BL", 
            "Aliments achetes","Dont concentres", "Charges surfaces", "Frais d'elevage", "Mecanisation", "dt amortissements meca", 
            "Batiments et installations", " dt amortissements bat", "Frais de gestion", "Fermages", 
            "Amort ameliorations foncier", "remun foncier propre", "Frais financiers", "remun capital propre", 
            "Travail exploitant", "Salaires BL", "Amortissements BL", "Charges suppletives BL", 
            "Annuites BL","TB","TP","Concentre VL g/l","Travaux par tiers","carburants lubrifiants","Entretien materiel",
             "eau","edf","Entretien batiment","Frais veto","Frais d'elevage"),
            data = c(input$ferme, as.character(input$debex),input$tec, input$sys, input$blat, 
            input$TUMOE, round(input$TUMOS,1), input$umoben, input$SAU, input$SFP, round(input$stofou,0), round(input$stoma,0), 
            round(input$ugb,1), input$vl, input$va, input$eg, round(input$laiprod,0), input$aid, round((input$prilait * 
                input$laiven/1000 + input$pvbl * input$laiven/1000 + input$autprod + input$aid), 0),
            input$chop, input$chst, input$salex, input$foncier, input$annu, input$amort, round(msa(), 
                0), input$fin, input$laiven, input$umoexp, input$umosal, input$prilait, input$pvbl, 
            input$CP_autprod, input$CP_aid, input$CP_alim, input$CP_conca,input$CP_surf, input$CP_fel, input$CP_meca, 
            input$amort_meca, input$CP_bat, input$amort_bat, input$CP_gest, input$CP_fon - input$rem_fon, 
            0, input$rem_fon, input$CP_cap - input$rem_cap, input$rem_cap, round(CP_travex(), 
                0), round(CP_sal(), 0), input$amort_meca + input$amort_bat, round(input$rem_fon + 
                input$rem_cap + CP_travex(), 0), input$annuBL,input$TB,input$TP, round(input$con,0),round(input$wtiers,0), 
             round(input$carbu,0), round(input$repa,0),round(input$eau,0),round(input$edf,0), 
            round(input$enbat,0), round(input$veto,0),round(input$fel,0))
             , stringsAsFactors = FALSE)
        
        })

   
   
    output$vartab1 <- renderTable({
        var()[1:27, ]
    },align='llr'
    )
    
    output$vartab2 <- renderTable({
        
        var()[28:length(rownames((var()))), ]
    },align='llr'
    )
    

    REMUN<-reactive({
      remun <- input$prilait + input$pvbl + input$CP_autprod + input$CP_aid - input$CP_alim - 
              input$CP_surf - input$CP_fel - input$CP_meca - input$CP_bat - input$CP_gest - input$CP_fon - 
              input$CP_cap
      return(remun)
    })
   
    output$plot1 <- renderPlot({
        
        par(bg = "snow", mar = c(5, 5, 3, 3), bty = "n")
        taille <- 1
        # remuneration du W total
       x<-REMUN()
        
        umoexp <- input$umoexp
        umosal <- input$umosal
        umoben <- input$umoben
        umorem <- umoexp + umosal
        
        y <- input$laiven/umorem/1000
        
     filex <- cplnat[which(cplnat$grobl == input$blat), ]
        
        x1 <- filex$remun_l
        y1 <- filex$lait_umo
        maxx <- max(x1, x * 1.2)
        maxy <- max(y1, y * 1.2)
        
        plot(x1, y1, ylim = c(0, maxy), xlim = c(0, maxx), pch = 19, cex = taille, col = "grey", 
            xlab = "Remuneration du travail (Euros/1000L)", ylab = "Lait livre en 1000L par UMO remunere", 
            font.lab = 4 , cex.lab = taille, cex.axis = 1, main = "Rémunération de la main d'oeuvre totale")
        
        grid(col = "plum", lty = "dotted", lwd = 1)
        
        
        MOtot <- umoexp + umosal + umoben
        
        if (umoben > 0) {
            
            text(maxx - 20, maxy, labels = "Main d'oeuvre benevole,", col = "red", font = 4, 
                pos = 2)
            text(maxx - 20, maxy - 20, labels = "non comptee en MO remuneree,", col = "red", 
                font = 4, pos = 2)
            text(maxx - 20, maxy - 40, labels = paste("represente", round(100 * (umoben/MOtot), 
                digits = 0), "% de la"), col = "red", font = 4, pos = 2)
            text(maxx - 20, maxy - 60, labels = "main d'oeuvre totale.", col = "red", font = 4, 
                pos = 2)
        }
        
        points(x, y, pch = 19, cex = taille * 2, col = "blue")
        
        lines(c(x, x), c(0, y), type = "l", lwd = 1, col = "green")
        text(x, 0, labels = round(x, digit = 0), cex = taille * 0.8, col = "darkgreen", font = 4)
        
        lines(c(x, 0), c(y, y), type = "l", lwd = 1, col = "green")
        text(0, y, labels = round(y, digit = 0), cex = taille * 0.8, col = "darkgreen", font = 4)
        
        Prodap <- seq(100, maxy, by = 10)
        
        Rem1 <- SMIC * 1.3/Prodap
        Rem1.5 <- 1.5 * 1.3 * SMIC/Prodap
        Rem2 <- 2 * 1.3 * SMIC/Prodap
        
        lines(Rem1, Prodap, type = "l", col = "red", lwd = 5)
        text(20, maxy, labels = "1", pos = 4, col = "brown4", cex = taille, font = 4)
        
        lines(Rem1.5, Prodap, type = "l", col = "orange", lwd = 5)
        text(35, maxy, labels = "1.5", pos = 4, col = "darkorange2", cex = taille, font = 4)
        
        lines(Rem2, Prodap, type = "l", col = "green", lwd = 5)
        text(55, maxy, labels = "2 SMIC/UMO", pos = 4, col = "darkgreen", cex = taille, font = 4)
        
     remun_UMOtot<-round(x * y/(SMIC * 1.3),digits=2)
      text(x, y, labels = paste(remun_UMOtot, "SMIC par UMO"), cex = taille, 
       col = "blue", font = 2, srt = 10, pos = 3)
        
    })
    
    output$plot2 <- renderPlot({
  
        
        xex<-REMUN()- 1000 * input$salait/input$laiven
        
        umoexp <- input$umoexp
        umosal <- input$umosal
        umoben <- input$umoben
        
        y <- input$laiven/1000

        par(bg = "snow", mar = c(2, 0, 2, 0), bty = "n")
       remex <- round(xex  * y/(SMIC * 1.3)/umoexp,digits=2)
        stripchart(1, pch = 1, cex = 0, vertical = F, cex.lab = 1, method = "stack", font = 2, 
            axes = T, xlim = c(0, 5), main = "Remuneration de la main d'oeuvre exploitant (SMIC/UMO expl.)")
        # points(remex,0.5,pch=19,col='blue',cex=3)
        arrows(remex, 1, remex, 0, lwd = 5, col = "blue")
        text(remex, 1, labels = paste(remex, "SMIC"),cex = 1, col = "blue", font = 2, pos = 3) 
       # text(remex, 1, labels = " par UMO exploitant",cex = 1, col = "blue", font = 2, pos = 3)
        
        
    })
    
    
    
    output$plot0 <- renderPlot({
        
        taille <- 1.2
        par(bg = "snow", mar = c(0, 0, 0, 0), bty = "n")
        
        
        plot(1, 1, xlim = c(-1, 3), ylim = c(0, 3), pch = 19, cex = 0, col = "blue", xlab = "", 
            ylab = "", font.lab = 4, cex.lab = taille * 0.8, cex.axis = 1, main = "", axes = F)
        
        grid(nx = 4, ny = 3, col = "plum", lty = "dotted", lwd = 1)
        sepx1 <- 1
        sepx2 <- 2
        sepy <- 1
        
        rect(0, 0, sepx1, sepy, col = "red", border = NA)
        rect(0, sepy, sepx1, 1.9, col = "cyan", border = NA)
        # rect(sepx2,0,3,sepy,col='orange',border=NA)
        rect(sepx1, sepy, sepx2, 1.9, col = "green", border = NA)
        rect(sepx1, 0, 3, sepy, col = "plum", border = NA)
        rect(sepx2, sepy, 3, 1.9, col = "yellow", border = NA)
        
        text(-1, 3, labels = "Cultures de vente", cex = 1.5, col = "blue", pos = 4)
        
        text(0, 2.7, labels = "Cult. vente <40ha  ", cex = 0.8, col = "blue", font = 2, pos = 4)
        text(0, 2.5, labels = "ou moins de", cex = 0.8, col = "blue", font = 2, pos = 4)
        text(0, 2.3, labels = "de 30% de la SAU", cex = 0.8, col = "blue", font = 2, pos = 4)
        
        text(sepx1, 3, labels = "Cult. vente > 40ha et ", cex = 0.8, col = "blue", font = 2, pos = 4)
        text(sepx1, 2.8, labels = "plus de 30% de la SAU", cex = 0.8, col = "blue", font = 2, pos = 4)
        text(sepx1, 2.6, labels = "et plus de 5000 litres ", cex = 0.8, col = "blue", font = 2, pos = 4)
        text(sepx1, 2.4, labels = "de lait par ha ", cex = 0.8, col = "blue", font = 2, pos = 4)
        text(sepx1, 2.2, labels = "de cult. de vente", cex = 0.8, col = "blue", font = 2, pos = 4)
        
        m <- 0.1
        text(sepx2 + m, 3, labels = "Cult. vente > 40 ha et", cex = 0.8, col = "blue", font = 2, 
            pos = 4)
        text(sepx2 + m, 2.8, labels = "plus de 30% de la SAU", cex = 0.8, col = "blue", font = 2, pos = 4)
        text(sepx2 + m, 2.6, labels = "et moins de 5000 litres", cex = 0.8, col = "blue", font = 2, 
            pos = 4)
        text(sepx2 + m, 2.4, labels = "de lait par ha", cex = 0.8, col = "blue", font = 2, 
            pos = 4)
        text(sepx2 + m, 2.2, labels = "de cult. de vente", cex = 0.8, col = "blue", font = 2, 
            pos = 4)
        
        
        text(-1, 2, labels = "Viande", cex = 1.5, col = "red", pos = 4)
        text(-1.1, 0.7, labels = "Plus de 5VA ou ", cex = 0.8, col = "red", font = 2, pos = 4)
        text(-1.2, 0.5, labels = "plus de 0.2JB/VL", cex = 0.8, col = "red", font = 2, pos = 4)
        
        text(-1.1, 1.6, labels = "Moins de 5VA et ", cex = 0.8, col = "red", font = 2, pos = 4)
        text(-1.2, 1.4, labels = "moins de 0.2JB/VL", cex = 0.8, col = "red", font = 2, pos = 4)
        
        text(0.1, 1.7, labels = "Lait_spec", pos = 4)
        text(1.1, 1.7, labels = "Lait_CV", pos = 4)
        text(2.1, 1.7, labels = "CV_lait", pos = 4)
        
        text(0.1, 0.7, labels = "Lait_viande", pos = 4)
        text(2, 0.7, labels = "Lait_viande_CV")
        # text(2,0.7,labels='CV_lait_viande',pos=4)
        
        
        cer <- input$SAU - input$SFP
        pcer <- 100 * cer/input$SAU
        laicer <- ifelse(input$SFP < input$SAU, input$laiprod/(input$SAU - input$SFP), 10000)
        jbvl <- input$eg/input$vl
        va <- input$va
        
        x <- ifelse(cer > 40, ifelse(pcer > 30, ifelse(laicer > 5000, 1.5, 2.5), 0.5), 0.5)
        y <- ifelse(va < 5, ifelse(jbvl < 0.2, 1.5, 0.5), 0.5)
        
        points(x, y, pch = 19, cex = 3, col = "blue")
        
    })
    
    output$plot3 <- renderPlot({
        
        taille <- 1.2
        par(bg = "snow", mar = c(5, 6, 0, 0), bty = "n")
        y <- input$stofou/input$ugb
        x <- 100 * input$stoma/input$stofou
        
        plot(x, y, xlim = c(-10, 100), ylim = c(2, 7), pch = 19, cex = taille * 1.5, col = "blue", 
            xlab = "Part de mais dans la ration (%)", ylab = "TMS fourrages stockes par UGB", 
            font.lab = 4, cex.lab = taille * 0.8, cex.axis = 1, main = "")
        
        grid(col = "plum", lty = "dotted", lwd = 1)
        sepx1 <- 20
        sepx2 <- 60
        sepy <- 4
        
        rect(0, 0, sepx1, sepy, col = "green", border = NA)
        rect(0, sepy, sepx1, 6.5, col = "green4", border = NA)
        rect(sepx1, 0, sepx2, sepy, col = "cyan", border = NA)
        text((sepx1 + sepx2)/2, 3, "MHP")
        rect(sepx1, sepy, sepx2, 6.5, col = "orange", border = NA)
        text((sepx1 + sepx2)/2, 5, "MHS")
        rect(sepx2, 0, 100, sepy, col = "plum", border = NA)
        rect(sepx2, sepy, 100, 6.5, col = "yellow", border = NA)
        text((100 + sepx2)/2, 5, "MDS")
        
        text(0, 7, labels = "Herbe", cex = 1.2, col = "blue", font = 2, pos = 4)
        text(sepx1, 7, labels = "Mais-herbe", cex = 1.2, col = "blue", font = 2, pos = 4)
        text(sepx2, 7, labels = "Mais dominant", cex = 1.2, col = "blue", font = 2, pos = 4)
        text(-10, 2, labels = "Paturage", cex = 1.2, col = "blue", font = 2, srt = 90, pos = 4)
        text(-10, 5, labels = "Stocks", cex = 1.2, col = "blue", font = 2, srt = 90, pos = 4)
        
        points(x, y, pch = 19, cex = 3, col = "red")
        
    })
    
    output$plot4 <- renderPlot({
        
        taille <- 2
        par(bg = "snow", mar = c(5, 5, 2, 2))
        filex <- cplnat[which(cplnat$grobl == input$blat), ]
        x <- filex$coprov
        xm <- mean(x)
        y <- filex$lait_vl
        ym <- mean(y)
        
        plot(x, y, ylim = c(0, max(y) * 1.1), xlim = c(0, max(x) * 1.1), pch = 19, cex = taille * 
            0.8, col = "grey", xlab = "Coproduit viande (Euros/1000l)", ylab = "Rendement laitier (L/VL/an)", 
            font.lab = 4, cex.lab = taille * 0.7, cex.axis = 0.8, main = "")
        
        grid(col = "plum", lty = "dotted", lwd = 1)
        
        x1 <- input$pvbl
        y1 <- input$laiprod/input$vl
        
        lines(c(x1, x1), c(0, y1), type = "l", lwd = 1, col = "darkgreen")
        text(x1, 0, labels = round(x1, digit = 0), cex = taille * 0.6, col = "darkgreen", font = 4)
        
        lines(c(x1, 0), c(y1, y1), type = "l", lwd = 1, col = "darkgreen")
        text(0, y1, labels = round(y1, digit = 0), cex = taille * 0.6, col = "darkgreen", font = 4, 
            pos = 4)
        text(x1, y1, labels = input$ferme, cex = 1, col = "darkgreen", srt = 45,font=4)
        
        lines(c(xm, xm), c(0, ym), type = "l", lwd = 1, col = "blue")
        text(xm, 500, labels = round(xm, digit = 0), cex = taille * 0.6, col = "blue", font = 4)
        
        lines(c(xm, 0), c(ym, ym), type = "l", lwd = 1, col = "darkgreen")
        text(0, ym + 500, labels = round(ym, digit = 0), cex = taille * 0.6, col = "blue", font = 4, 
            pos = 4)
        text(xm, ym, labels = "Moyenne du groupe", cex = 1, srt = 45, col = "blue",font=4)
    })
    
    output$plot41 <- renderPlot({
        
        taille <- 2
        par(bg = "snow", mar = c(5, 5, 2, 2))
        filex <- cplnat[which(cplnat$grobl == input$blat), ]
        x <- filex$aides
        xm <- mean(x)
        y <- as.numeric(filex$lait_surfalim)
        ym <- mean(y)
        
        plot(x, y, pch = 19, cex = taille * 0.8, col = "grey", xlim = c(0, max(x) * 1.1), ylim = c(0, 
            max(y) * 1.1), xlab = "Aides affectees a l'atelier lait (Euros/1000L)", ylab = "Lait par ha SFP (1000l/ha)", 
            font.lab = 4, cex.lab = taille * 0.7, cex.axis = 0.8, main = "")
        
        grid(col = "plum", lty = "dotted", lwd = 1)
        
        
        x1 <- input$CP_aid
        y1 <-  input$laiprod/input$SFP
        
        lines(c(x1, x1), c(0, y1), type = "l", lwd = 1, col = "darkgreen")
        text(x1, 0, labels = round(x1, digit = 0), cex = taille * 0.6, col = "darkgreen", font = 4)
        
        lines(c(x1, 0), c(y1, y1), type = "l", lwd = 1, col = "darkgreen")
        text(0, y1, labels = round(y1, digit = 0), cex = taille * 0.6, col = "darkgreen", font = 4, 
            pos = 4)
        text(x1, y1, labels = input$ferme, cex = 1, col = "darkgreen", srt = 45,font=4)
        
        lines(c(xm, xm), c(0, ym), type = "l", lwd = 1, col = "blue")
        text(xm, 5, labels = round(xm, digit = 0), cex = taille * 0.6, col = "blue", font = 4)
        
        lines(c(xm, 0), c(ym, ym), type = "l", lwd = 1, col = "darkgreen")
        text(0, ym + 5, labels = round(ym, digit = 0), cex = taille * 0.6, col = "blue", font = 4, 
            pos = 4)
        text(xm, ym, labels = "Moyenne du groupe", cex = 1, srt = 45, col = "blue",font=4)
        
    })
    
    
    output$plot11 <- renderPlot({
        
        par(bg = "snow", mar = c(3, 5, 0, 0))
        decalh <- 15
        
        prodlait <- input$prilait * input$laiven/1000
        prodviabl <- input$pvbl * input$laiven/1000
        aid <- input$aid
        autprod <- input$autprod
        prod <- prodlait + prodviabl + autprod + aid
        
        chop <- input$chop*(1+input$indichop/100)    
        chst <- (input$chst  - msa())*(1+input$indistruc/100)
       
        
        annu <- input$annu
        amort <- input$amort
        fin <- input$fin
        ebe <- EBE()
        
        pcebe <- ebe/prod
        
        dispo <- ebe - annu
        dispoumo <- dispo/input$TUMOE
        capital <- input$capital
        
        x <- matrix(c(prodlait, prodviabl, autprod, aid)/1000)
        coul1 <- c("blue", "orchid", "yellow", "cyan")
        bar1 <- barplot(x, col = coul1, xlim = c(0, 12), width = 2, ylim = c(0, 1.1*prod/1000), 
            cex.axis = 1, ylab = "1000 Euros", border = coul1)
        
        a1 <- prodlait/2000
        text(bar1, a1, labels = "Produit:", cex = 0.9, font = 2, col = "white")
        text(bar1, a1-decalh, labels = "lait :", cex = 0.9, font = 2, col = "white")
        text(bar1, a1 - 2*decalh, labels = paste(round(100 * prodlait/prod, 0),"%"), cex = 0.9, 
            font = 2, col = "white")
            text(bar1, a1 - 3*decalh, labels = "du PB", cex = 0.9, 
        font = 2, col = "white")
        
        a2 <- prodlait/1000 + (prodviabl/2000)
        text(bar1, a2, labels = "viande BL", cex = 0.7, font = 1)
        a3 <- (prodlait + prodviabl)/1000 + (autprod/2000)
        text(bar1, a3, labels = "Autres", cex = 0.9, font = 1)
        a4 <- (prodlait + prodviabl + autprod)/1000 + (aid/2000)
        text(bar1, a4, label = "Aides", cex = 0.9, font = 2)
        
        y <- matrix(c(ebe, chst, chop)/1000)
        coul2 <- c("green4", "orange", "salmon")
        
        bar2 <- barplot(y, col = coul2, add = T, width = 2, space = 1.5, axes = F, border = coul2)
        b1 <- ebe/2000
        text(bar2, b1, labels = "EBE:", cex = 0.9, font = 2, col = "white")
        text(bar2, b1 - decalh, labels = paste(round(100 * pcebe, 0), "%"), cex = 0.9, 
            font = 2, col = "white")
        text(bar2, b1 - 2*decalh, labels = "du pb", cex = 0.9, 
             font = 2, col = "white")
        b2 <- ebe/1000 + chst/2000
        text(bar2, b2, labels = "Ch.struct.", cex = 0.9, font = 2)
        b3 <- (ebe + chst)/1000 + chop/2000
        text(bar2, b3, label = "Ch. oper.", cex = 0.9, font = 2)
        
        z <- matrix(c(dispo, annu)/1000)
        coul3 <- c("skyblue", "tan")
        bar3 <- barplot(z, col = coul3, add = T, width = 4, space = 1.5, axes = F, border = coul3)
        
        text(bar3, (dispo + annu)/1000, labels = "annuites:", cex = 0.9, font = 2, pos = 1)
        text(bar3, (dispo + annu)/1000 - decalh, labels = paste(round(100 * annu/ebe, 0), "% de l'EBE"), 
            cex = 0.9, font = 2, pos = 1)
        text(bar3, dispo/2000, labels = "Revenu disponible:", cex = 0.9, font = 2)
        text(bar3, dispo/2000 - decalh, labels = paste(round(dispoumo, 0), "E/UMO expl."), 
            cex = 0.9, font = 2)

        
    })
    
     PROD <-reactive({
 
#      prodlait <- input$prilait * input$laiven/1000
#      prodviabl <- input$pvbl * input$laiven/1000
#      aid <- input$aid
#      autprod <- input$autprod
      prod<-input$prilait * input$laiven/1000+
          input$pvbl * input$laiven/1000+
           input$aid+input$autprod
     return(prod)
     
   })
    
    output$plot12 <- renderPlot({
      prod <- PROD()
      UMO<-input$TUMOE+input$TUMOS
      prod_umo<-(prod/UMO)/1000
 
      barex(a = reperex(), var = "pbumo", posex = prod_umo, rog = 1)
        
    })
    
   EBE<-reactive({
     prod <- PROD()
     chop <- input$chop*(1+input$indichop/100)
     chst <- (input$chst-input$msa + msa())*(1+input$indistruc/100)
     ebe <- prod - chop - chst
     return(ebe)
   })
   
    output$plot13 <- renderPlot({
                
        ebe2<-EBE ()+input$salex+msa()+input$foncier
        prod<-PROD()
        pcebe <- round(100 * ebe2/prod, 0)
        
        barex(a = reperex(), var = "ebep", posex = pcebe, rog = 1)
        
    })
    
    output$plot131 <- renderPlot({
        
        ebe2<-EBE ()+input$salex+msa()+input$foncier
       
        UMO<-input$TUMOE+input$TUMOS
        ebe_umo <- (ebe2/UMO)/1000
       
        barex( a = reperex(), var = "ebeumo", posex = ebe_umo, rog = 1)
        
    })
    
    output$plot14 <- renderPlot({
     
        ebe <- EBE()
        annu <- input$annu
        
        annu_ebe <- round(100 * annu/ebe)
      
        barex(a = reperex(), var = "anub", posex = annu_ebe, rog = 0)
        
    })
    
    
    
    output$plot15 <- renderPlot({
        
        par(bg = "snow", mar = c(3, 3, 2, 2))
        CP_alim <- input$CP_alim
        CP_surf <- input$CP_surf
        CP_fel <- input$CP_fel
        CP_meca <- input$CP_meca
        amort_meca <- input$amort_meca
        CP_bat <- input$CP_bat
        amort_bat <- input$amort_bat
        CP_gest <- input$CP_gest
        CP_fon <- input$CP_fon
        CP_cap <- input$CP_cap
        rem_fon <- input$rem_fon
        rem_cap <- input$rem_cap
        
        rem_travex <- CP_travex() 
        
        CPBL <- CP_alim + CP_surf + CP_fel + CP_meca + CP_bat + CP_gest + CP_fon + CP_cap + 
            CP_travex() + CP_sal()
        CP_amort <- amort_meca + amort_bat
        CP_chasup <- rem_cap + rem_fon + CP_travex()
        CP_chacour <- CPBL - CP_amort - CP_chasup
        
        salim <- CP_alim + CP_surf + CP_meca + CP_fon
        bat <- CP_bat
        servis <- CP_fel + CP_gest
        cap <- CP_cap
        wsal <-  CP_sal()
        wexp <- CP_travex() 
        
        prilait <- input$prilait
        provia <- input$pvbl + input$CP_autprod
        aid <- input$CP_aid
        
        PROD <- prilait + provia + aid
        
        
        x <- matrix(c(salim, bat, servis, cap, wsal,wexp))
        y <- matrix(c(aid, provia, prilait))
        
        coul3 <- c("green", "yellow", "lightblue", "plum", "salmon","tan")
        coul4 <- c("violet", "red", "blue")
        
        bar3 <- barplot(x, col = coul3, xlim = c(0, 12), width = 5, ylim = c(0, max(CPBL, PROD)))
        bar4 <- barplot(y, col = coul4, add = T, width = 5, space = 1.5)
        
        
        a3 <- salim/2
        text(bar3, a3, label = "Systeme alimentaire", cex = 1.5, font = 2, srt = 90)
        a4 <- salim + bat/2
        text(bar3, a4, label = "Batiments", cex = 0.9, font = 2)
        a5 <- salim + bat + servis/2
        text(bar3, a5, label = "Services", cex = 0.9, font = 2)
        a6 <- salim + bat + servis + cap/2
        text(bar3, a6, label = "Capital", cex = 0.9, font = 2)
        a7 <- salim + bat + servis + cap + wsal/2
        text(bar3, a7, label = "Salaires", cex = 0.9, font = 2)
        a8 <- salim + bat + servis + cap + wsal+ wexp/2
        text(bar3, a8, label = "Trav. expl.", cex = 0.9, font = 2)
        
        b3 <- aid/2
        text(bar4, b3, label = "Aides", cex = 1, font = 2, srt = 0)
        b4 <- aid + provia
        text(bar4, b4, label = "Viande et ", cex = 0.8, font = 2, srt = 0, pos = 1)
        text(bar4, b4 - 10, label = " autres produits", cex = 0.8, font = 2, srt = 0, pos = 1)
        text(bar4, aid, label = provia, cex = 1, font = 2, srt = 0, pos = 3)
        
        b5 <- aid + provia + prilait/2
      text(bar4, b5, label ="Prix du lait:", cex = 1, font = 2, col = "white")
      text(bar4, b5-20, label = prilait, cex = 1.5, font = 2, col = "white")   
      text(bar4, b5-40, label =  " Euros/1000l", cex = 1, font = 2, col = "white")
      
      
      prirev=round(CPBL-aid-provia,0)
      arrows(7,aid+provia,7,CPBL,col="blue",lwd=3,code=3,lty="solid")
      text(6.3,(aid+provia)*1.25,labels=paste("Prix de revient :",prirev,"Euros par 1000 litres"),
           srt=90,pos=4,col="blue",font=4,cex=1)
      
    })
    
    
    
    
    
    output$plot40 <- renderPlot({
      
        prodbl <- input$prilait + input$pvbl + input$CP_aid + input$CP_autprod
        barat(a = reperat(), var = "prodbl", posatex = prodbl, rog = 1)
        
    })
    
   output$prod <- renderText({     
     prodbl <- input$prilait + input$pvbl + input$CP_aid + input$CP_autprod
     paste("PRODUIT :",prodbl," Euros/1000L")      
   })
    
    output$plot50 <- renderPlot({
  
        barat(a = reperat(), var = "prilait", posat = input$prilait, rog = 1)
        
    })
    
   
    
    output$plot70 <- renderPlot({
       
        sali_ferme <- input$CP_alim + input$CP_surf + input$CP_meca + input$CP_fon
        barat(a = reperat(), var = "sali", posat = sali_ferme, rog = 0)
        
    })
   
   output$sysal <- renderText({
     
     sali_ferme <- input$CP_alim + input$CP_surf + input$CP_meca + input$CP_fon
       sysal<-paste("COUT DU SYSTEME ALIMENTAIRE :",sali_ferme," Euros/1000L")
     return(sysal)
   })
   
    
    output$plot71 <- renderPlot({
     
        cali_ferme <- input$CP_alim + input$CP_surf
        barat(a = reperat(), var = "cali", posat = cali_ferme, rog = 0)
        
    })
    
    output$plot72 <- renderPlot({
     
        meca_ferme <- input$CP_meca
        barat(a = reperat(), var = "meca", posat = meca_ferme, rog = 0)
        
    })
    
    output$plot73 <- renderPlot({
     
        fon_ferme <- input$CP_fon
        barat(a = reperat(), var = "fon", posat = fon_ferme, rog = 0)
        
    })
    
    output$plot75 <- renderPlot({
    
        bat_ferme <- input$CP_bat
        barat(a = reperat(), var = "bat", posat = bat_ferme, rog = 0)       
    })
    
   output$coubat <- renderText({
     
     bat_ferme <- input$CP_bat
    paste("BATIMENTS :",bat_ferme," Euros/1000L")      
   })
    
    output$plot80 <- renderPlot({
      
      serv_ferme <- input$CP_fel + input$CP_gest
        barat(a = reperat(), var = "serv", posat = serv_ferme, rog = 0)
        
    })
    
   
   
   output$couserv <- renderText({
     
     serv_ferme <- input$CP_fel + input$CP_gest
     paste("SERVICES :",serv_ferme," Euros/1000L")      
   })
   
    
    output$plot90 <- renderPlot({
     
      cap_ferme <- input$CP_cap
        barat(a = reperat(), var = "cap", posat = cap_ferme, rog = 0)
    })
    
 
   output$coucap <- renderText({     
     cap_ferme <- input$CP_cap
     paste("CAPITAL :",cap_ferme," Euros/1000L")      
   })
   
   
   output$barprod<-renderPlot({
     varprod <-data.frame(var = c( "TB", "TP", "cels"), 
                          ferme = c( input$TB, input$TP, input$cels), 
                          numrep = c( 31, 34, 37))
     
     
     barset(a=reperat(),don=varprod,inv="O")
   
     })
   

   
    
    output$infoprod <- renderHtable({ 
       prod<-readRDS("data/recomprod")
       if(is.null(input$infoprod))
       { 
         for (n in 1:9) 
         {
         w<-rep("",4)    
        prod<-rbind(prod,w) 
           tab<-prod
         } 
         }else{
           tab<-input$infoprod
         }
         return(tab)
       })


   
   output$baralim<-renderPlot({
     varalim <-data.frame(var = c( "Concentre (g/l)", 
                                  "Intrants surfaces (E/1000L)", "Chargement", "Travaux par tiers", "Carburant lubrifiant", 
                                  "Reparations du materiel", "Amortissement materiel"),
                          ferme = c(input$con, input$CP_surf, input$charg, input$wtiers, input$carbu, 
                                    input$repa, input$amortmat), 
                          numrep = c( 40, 43, 46, 49, 52, 55, 58)) 
    
     barset(a=reperat(),don=varalim,inv="N")
     
   })
   
   
    output$infosalim <- renderHtable({
    salim<-readRDS("data/recomalim")
        if(is.null(input$infosalim))
        { 
          for (n in 1:9) 
          {
            w<-rep("",4)    
            salim<-rbind(salim,w) 
            tab<-salim
          } 
        }else{
          tab<-input$infosalim
        }
        return(tab)
      })
    

   output$barbat<-renderPlot({
     varbat<-data.frame(var = c( "eau", "edf", "entretien", "amort."), 
                          ferme = c( input$eau, input$edf, input$enbat, input$amobat),
                          numrep = c( 61, 64, 67, 70))
    
     barset(a=reperat(),don=varbat,inv="N")
     
   })
   
    output$infobat <- renderHtable({
        bati<-readRDS("data/recombat")
        if(is.null(input$infobat))
        { 
          for (n in 1:9) 
          {
            w<-rep("",4)    
           bati<-rbind(bati,w) 
            tab<-bati
          } 
        }else{
          tab<-input$infobat
        }
        return(tab)
      })
   
   output$barsercap<-renderPlot({
     varsercap<-data.frame(var = c("Frais d'elevage", "Frais veto", 
                                "Frais de gestion", "Frais financiers"), 
                        ferme = c(input$fel, input$veto, input$CP_gest, 
                                  input$ff), 
                        numrep = c(73, 76, 79, 82))
  
     barset(a=reperat(),don=varsercap,inv="N")
     
   })
   
   
   output$infosercap <- renderHtable({
       serca<-readRDS("data/recomsercap")
       if(is.null(input$infosercap))
       { 
         for (n in 1:9) 
         {
           w<-rep("",4)    
           serca<-rbind(serca,w) 
           tab<-serca
         } 
       }else{
         tab<-input$infosercap
       }
       return(tab)
     })
   
   
   output$radar <- renderPlot({
    
     prodbl <- input$prilait + input$pvbl + input$CP_aid + input$CP_autprod
     
     varalibat <- switch(input$varset,
                         prod = data.frame(var = c("produit BL","Prix du lait", "TB", "TP", "cels"), 
                                           ferme = c(prodbl,input$prilait, input$TB, input$TP, 8-input$cels), 
                                           numrep = c(1,4, 31, 34, 37)), 
                         salim = data.frame(var = c("Cout alimentaire", "Concentre (g/l)", 
                                                    "Intrants surfaces (E/1000L)", "Chargement", "Travaux par tiers", "Carburant lubrifiant", 
                                                    "Reparations du materiel", "Amortissement materiel", "Foncier"),
                                            ferme = c(input$CP_alim + input$CP_surf, input$con, input$CP_surf, input$charg, input$wtiers, input$carbu, 
                                                      input$repa, input$amortmat, input$CP_fon), 
                                            numrep = c(13, 40, 43, 46, 49, 52, 55, 58, 19)), 
                         bat = data.frame(var = c("Cout batiment", "eau", "edf", "entretien", "amort."), 
                                          ferme = c(input$CP_bat, input$eau, input$edf, input$enbat, input$amobat),
                                          numrep = c(22, 61, 64, 67, 70)), 
                         sercap = data.frame(var = c("Frais d'elevage", "Frais veto", 
                                                     "Frais de gestion", "Frais financiers"), 
                                             ferme = c(input$fel, input$veto, input$CP_gest, 
                                                       input$ff), 
                                             numrep = c(73, 76, 79, 82)))
     
     
     inv <- switch(input$varset, prod = "N", salim = "O", bat = "O", sercap = "O")
     
     radar(a = reperat(), don = varalibat, inv = inv)
     
   })
   
   output$radaralim <- renderPlot({
     
     prodbl <- input$prilait + input$pvbl + input$CP_aid + input$CP_autprod
     
     varalibat <-  data.frame(var = c("Cout alimentaire", "Concentre (g/l)", 
                                                    "Intrants surfaces (E/1000L)", "Chargement", "Travaux par tiers", "Carburant lubrifiant", 
                                                    "Reparations du materiel", "Amortissement materiel", "Foncier"),
                                  ferme = c(input$CP_alim + input$CP_surf, input$con, input$CP_surf, input$charg, input$wtiers, input$carbu, 
                                                      input$repa, input$amortmat, input$CP_fon), 
                                 numrep = c(13, 40, 43, 46, 49, 52, 55, 58, 19))
     
     radar(a = reperat(), don = varalibat, inv = "O")
     
   })
   
####### save recom


observe({
  input$infoprod
if(!is.null(input$infoprod))
{  
  tab<-input$infoprod
}else{
  tab<-readRDS("data/recomprod")
}
saveRDS(tab,"data/recomprod")
})

observe({
  input$infosalim
  if(!is.null(input$infosalim))
  {  
    tab<-input$infosalim
  }else{
    tab<-readRDS("data/recomalim")
  }
  saveRDS(tab,"data/recomalim")
})

observe({
  input$infobat
  if(!is.null(input$infobat))
  {  
    tab<-input$infobat
  }else{
    tab<-readRDS("data/recombat")
  }
  saveRDS(tab,"data/recombat")
})


observe({
  input$infosercap
  if(!is.null(input$infosercap))
  {  
    tab<-input$infosercap
  }else{
    tab<-readRDS("data/recomsercap")
  }
  saveRDS(tab,"data/recomsercap")
})


output$recompdf <- downloadHandler(
  filename = function() { "essai.pdf" },
  content = function(file) { 
     

    pdf(file)
    prod<-readRDS("data/recomprod")
    alim<-readRDS("data/recomalim")
    bati<-readRDS("data/recombat")
    cap<-readRDS("data/recomsercap")
    q<-rbind(prod,alim,bati,cap)
    q<-subset(q,Point!="")
    q<-subset(q, Point!="Point de vigilance")
    if(nrow(q)==0) return(NULL)
    table <- tableGrob(q,
                       gpar.coretext = gpar(fontsize = 10,col="blue",cex=1),
                       gpar.corefill = gpar(fill = "lightblue", alpha=1, col = "red"),
                        gpar.rowtext = gpar(col = "blue", cex = 1.2, fontface = "bold"), 
                     gpar.coltext = gpar(col = "red", cex = 1.2, fontface = "italic"),
                        show.rownames = FALSE,
                      h.even.alpha = 0.5,show.vlines = TRUE,padding.h = unit(8, "mm"))
    
    grid.newpage()
    h <- grobHeight(table)
    w <- grobWidth(table)
    title <- textGrob(input$ferme, y=unit(0.5,"npc") + 0.5*h, 
                      vjust=0, gp=gpar(fontsize=15))
    footnote <- textGrob(paste("Le ",input$datereal," par ",input$tec), 
                     x=unit(0.5,"npc") - 0.5*w,
                     y=unit(0.5,"npc") - 0.5*h, 
                     vjust=1, hjust=0,gp=gpar( fontface="italic"))
    gt <- gTree(children=gList(table, title,footnote))
    grid.draw(gt)    
    dev.off()

  },contentType = "application/pdf"
)



output$recomcsv <- downloadHandler(
  filename = function() { "essai.csv" },
  content = function(file) { 
    prod<-readRDS("data/recomprod")
    alim<-readRDS("data/recomalim")
    bati<-readRDS("data/recombat")
    cap<-readRDS("data/recomsercap")
    q<-rbind(prod,alim,bati,cap)
    q<-subset(q,Point!="")
    q<-subset(q, Point!="Point de vigilance")
    write.csv(q,file,row.names=F)
    
  })


 ############# instantanés  
# observe({
#   if(!is.null(farm()))
#   {
#     inst <-cbind(readRDS("data/inst.RDS"),tabini(farm())) 
#     write.csv(inst,file="data/inst.csv",row.names=FALSE) 
#   }
#   
# })

instant <- reactiveFileReader(1000, session, "data/inst.csv",read.csv)   

   observe({ 
     
     input$newi
     if(!is.null(input$newi)) {
       if(input$newi=="on" )
             {       
         if(!is.null(input$nomnewi)) {
          inst<-instant()
          if(!is.null(inst)){
          nom<-as.character(input$nomnewi)
           EBE_pb<-round(EBE()/PROD(),2)
           dispo_umoex<-round(((EBE()-input$annu))/input$TUMOE,0)
           laivendu <- input$laiven/1000
           remun<-REMUN()
           remunex<-REMUN()- 1000 * input$salait/input$laiven
           umoexp <- input$umoexp
           umosal <- input$umosal
           umoben <- input$umoben
           umorem<-umoexp+umosal
  
        colad<-data.frame(Inst=c(nom,input$TUMOE,input$TUMOS,umorem+umoben,umoben,umorem,umosal,
                                 round(input$prilait,0),input$aid,input$annu,
                                  EBE_pb,dispo_umoex,round(laivendu/(umorem+umoben),0),
                                 round(remun*laivendu/umorem/(SMIC*1.3),2),
                                  round(remunex *laivendu/(SMIC * 1.3)/umoexp,2)
                                   ))
        inst<-cbind(inst,colad)
        if (ncol(inst)==2) 
                {
           colnames(inst)[ncol(inst)]<-paste0("Instant_",0)
             }else{
          a<-as.numeric(substr(colnames(inst)[ncol(inst)-1],9,9))
          colnames(inst)[ncol(inst)]<-paste0("Instant_",a+1)
         }
        
         write.csv(inst,file="data/inst.csv",row.names=FALSE)
        
         finew="off"
          session$sendCustomMessage(type='oknew',finew)
         }}}} 
     
   })
    
 ######## supprime instantane
 observe({ 
   input$supelim
   inst<-isolate(instant())
   
   if(!is.null(input$supelim)) 
   {
     if (input$supelim != "off")
     {  
       numelim<-as.numeric(input$supelim)
       
       a<- which(colnames(inst) == paste0("Instant_",numelim))
       if(length(a)!=0)
       {
         if( a !=0)
       {
     
     
      inst<-inst[,-a]
      
      write.csv(inst,file="data/inst.csv",row.names=FALSE)
       
      mes<-paste("Instantane ",numelim," supprime")
      
      session$sendCustomMessage(type='okelim',mes)
       
       }}else{
      mes<-paste0("Instant_",numelim," n'existe pas!")
      
      session$sendCustomMessage(type='okelim',mes)
      
     }}
   }
 })


 
   output$instant <- renderTable({   
     input$newi
     input$supelim
     return(instant())
   },include.rownames = FALSE) 
    
############################################################
   

    output$report1 = downloadHandler(
      filename = "rep.pdf", 
      content = function(file) {
        # generate PDF
        out = knit2pdf("imp/rap1.Rnw", clean = TRUE)
        #file.rename(out, file) # move pdf to file for downloading
        # copy pdf to 'file'
        file.copy("rap1.pdf", file)
        
         file.remove('rap1.pdf', 'rap1.tex')
        
        # delete folder with plots
        unlink("figure", recursive = TRUE)
        
    }, contentType = "application/pdf")
    

    output$report2 = downloadHandler(filename = "rep.pdf", content = function(file) {
      # generate PDF
      out = knit2pdf("imp/rap2.Rnw", clean = TRUE)
      
      # copy pdf to 'file'
      file.copy("rap2.pdf", file)
      
      file.remove('rap2.pdf', 'rap2.tex')
      
      # delete folder with plots
      unlink("figure", recursive = TRUE)
      
    }, contentType = "application/pdf")
    
    output$report3 = downloadHandler(filename = "rep.pdf", content = function(file) {
      # generate PDF
      out = knit2pdf("imp/rap3.Rnw", clean = TRUE)
      
      # copy pdf to 'file'
      file.copy("rap3.pdf", file)
      
      file.remove('rap3.pdf', 'rap3.tex')
      
      # delete folder with plots
      unlink("figure", recursive = TRUE)
      
    }, contentType = "application/pdf")
    
    
    output$report4 = downloadHandler(filename = "rep.pdf", content = function(file) {
      # generate PDF
      out = knit2pdf("imp/rap4.Rnw", clean = TRUE)
      
      # copy pdf to 'file'
      file.copy("rap4.pdf", file)
      
        file.remove('rap4.pdf', 'rap4.tex')
      
      # delete folder with plots
      unlink("figure", recursive = TRUE)
      
    }, contentType = "application/pdf")
    
    
    output$report5 = downloadHandler(filename = "rep.pdf", content = function(file) {
      # generate PDF
      out = knit2pdf("imp/rap5.Rnw", clean = TRUE)
      
      # copy pdf to 'file'
      file.copy("rap5.pdf", file)
      
      # delete generated files
       file.remove('rap5.pdf', 'rap5.tex')
      
      # delete folder with plots
      unlink("figure", recursive = TRUE)
      
    }, contentType = "application/pdf")
    
    
    output$savexel <- downloadHandler(filename = function() {
        paste("data", Sys.Date(), ".xlsx")
    }, content = function(file) {
        wb <- createWorkbook()
        sheet <- createSheet(wb, sheetName = "farm")
        database <- var()[c(1:length(rownames(var()))), ]
        addDataFrame(database, sheet, row.names = FALSE, col.names = FALSE)
        saveWorkbook(wb, file)
    })

    output$savecsv<- downloadHandler(
      filename = function() { paste0("sauvegarde",Sys.Date(),'.csv') },
      content = function(file) {
         tab<-as.data.frame(t(as.matrix(var() )))
        colnames(tab)<-vardiap
        tab<-tab[2,]
        write.csv(tab,file,row.names=F)
        
      })
    




 output$recupinst<- downloadHandler(
   filename = function() { paste0("simuls",Sys.Date(),'.csv') },
   content = function(file) {
     write.csv(instant(),file,row.names=F)
     
   })
 
    
    })



 
