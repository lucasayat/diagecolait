

 SMIC<-13545  ### SMIC net 2014
 posys<-readRDS("ref/posys14")
 posat<-readRDS("ref/posat14")
 cplnat<-readRDS("ref/cplnat14")

references<-function(){
  library(xlsx)
  options(stringsAsFactors=FALSE)
 posys<-read.xlsx("G:/Documents/AR/diaginosys/ref/pos2014.xlsx", sheetName="posys")
 saveRDS(posys,"G:/Documents/AR/diaginosys/ref/posys14")
 posat<-read.xlsx("G:/Documents/AR/diaginosys/ref/pos2014.xlsx", sheetName="posat")
 saveRDS(posat,"G:/Documents/AR/diaginosys/ref/posat14")
}



sys<-function(x){
  sys<-switch(x,
         Lait_spec = "Lait specialise de plaine",
         Lait_CV = "Lait et cultures de vente", 
         CV_lait = "Cultures de vente et lait", 
         Lait_viande = "Lait de plaine et viande bovine",
         mont_MH="Montagne herbe et mais",
         mont_H="Montagne herbagere")
   
  return(sys)
}

atel<-function(x){
  atel <- switch(x, 
                 plaine_robot = "Lait de plaine avec robot",
                 plaine_MDS = "Lait de plaine Mais dominant stocks", 
                 plaine_MHS = "Lait plaine Mais-herbe stocks",
                 plaine_MHP = "Lait plaine Mais-herbe paturage",
                 montagne_PMH="Montagne herbe et mais",
                 montagne_MAH="Montagne herbagere")
  
  return(atel)
  
}


 barex <- function(a ,var,posex=NULL,rog=1)
 {
   
   if (var=="pbumo") {b<-1}
   if (var=="ebep") {b<-4}
   if (var=="ebeumo") {b<-7}
    if (var=="anub") {b<-10}
     
   par(bg="snow",mar=c(2,0,2,0),bty="n")
    
   med<-posys[b,a]
   q1<-med*(1-posys[b+1,a])
   q3<-med*(1+posys[b+1,a])
   inic<-max(0,med*(1-posys[b+2,a]))
   fin<-med*(1+posys[b+2,a])   
       
   coul1<-c("red","orange","yellow","green")
   coul2<-c("green","yellow","orange","red")
   ifelse(rog==1,coul<-coul1,coul<-coul2)   
   
   stripchart(1, pch=1,cex=0,vertical=F,
              cex.lab=1, method="stack",font=2,axes=T,
              xlim=c(inic,fin),
              main=posys[b,2])  
   
   rect(inic,0,q1,2,col=coul[1],border=NA)
   text(q1,0.5,labels=round(q1,0),pos=2)
   rect(q1,0,med,2,col=coul[2],border=NA)
   text(med,0.5,labels=round(med,0),pos=2)
   rect(med,0,q3,2,col=coul[3],border=NA)
   text(q3,0.5,labels=round(q3,0),pos=2)
    rect(q3,0,fin,2,col=coul[4],border=NA)   
   
   if(!is.null(posex)) {
     points(posex,1,pch=19,col="blue",cex=3)}
   
 } 
 
 ### barre avec 4 quartiles (rog <-sens rouge-orange-vert =1)
 
 barat <- function(a,var,posatex=NULL,rog=1)
 {
  
  # var<-"sali"
   # a<-4
   

       b<- switch(var,  
                  "prodbl"=1,
                "prilait"=4,
                  "provia"=7,
                  "sali"=10,
                  "cali"=13,
                "meca"=16,
                "fon"=19,
                 "bat"=22,
                 "serv"=25,
                  "cap"=28)
  
   par(bg="snow",mar=c(2,0,2,0),bty="n")
   
   med<-posat[b,a]
   q1<-med*(1-posat[b+1,a])
   q3<-med*(1+posat[b+1,a])
   inic<-max(0,med*(1-posat[b+2,a]))
   fin<-med*(1+posat[b+2,a])       
   
   coul1<-c("red","orange","yellow","green")
   coul2<-c("green","yellow","orange","red")
   ifelse(rog==1,coul<-coul1,coul<-coul2)   
   
   stripchart(1, pch=1,cex=0,vertical=F,
              cex.lab=1, method="stack",font=2,axes=T,xlim=c(inic,fin),
              main=posat[b,2])  
   
   rect(inic,0,q1,2,col=coul[1],border=NA)
   text(q1,0.5,labels=round(q1,0),pos=2)
   rect(q1,0,med,2,col=coul[2],border=NA)
   text(med,0.5,labels=round(med,0),pos=2)
   rect(med,0,q3,2,col=coul[3],border=NA)
   text(q3,0.5,labels=round(q3,0),pos=2)
   rect(q3,0,fin,2,col=coul[4],border=NA)   
   
   if(!is.null(posatex)) {
     points(posatex,1,pch=19,col="blue",cex=3)}
   
 } 
#################

barset<-function(a,don,inv="O",posatex=NULL) {
  
 # don<-data.frame(var = c( "TB", "TP", "Cellules"), 
  # ferme = c( 40, 33, 10), 
  # numrep = c( 31, 34, 37))
   # a<-3
  #  inv<-"O"

  data<-don   
  b<-data$numrep
  posatex<-data$ferme
  
  lb<-length(b)
  med=q1=q3=inic=fin<-rep(0,lb)

  par(bg="snow",mar=c(2,0,5,0),bty="n")
  c<-matrix(c(1:lb),lb,1)
  nf <- layout(c)
        
  coul1<-c("red","orange","yellow","green")
  coul2<-c("green","yellow","orange","red")
  ifelse(inv=="O",coul<-coul1,coul<-coul2)
  
  
  for (i in 1:lb)
  {
    x<-b[i]
    if(x==37) {coul<-coul2}
    med[i]<-posat[x,a]
    q1[i]<-med[i]*(1-posat[x+1,a])
    q3[i]<-med[i]*(1+posat[x+1,a])
    inic[i]<-max(0,med[i]*(1-posat[x+2,a]))
    fin[i]<-med[i]*(1+posat[x+2,a])
    
    stripchart(1, pch=1,cex=0,vertical=F,
    cex.lab=1.5, method="stack",font=2,axes=T,xlim=c(inic[i],fin[i]),
    main=posat[b[i],2],cex.main=2,cex.axis=1.5)  
    
    rect(inic[i],0,q1[i],2,col=coul[1],border=NA)
    text(q1[i],0.5,labels=round(q1[i],0),pos=2,cex=1.5)
    rect(q1[i],0,med[i],2,col=coul[2],border=NA)
    text(med[i],0.5,labels=round(med[i],0),pos=2,cex=1.5)   
    rect(med[i],0,q3[i],2,col=coul[3],border=NA)
    text(q3[i],0.5,labels=round(q3[i],0),pos=2,cex=1.5)
    rect(q3[i],0,fin[i],2,col=coul[4],border=NA) 
    
    
    if(!is.null(posatex[i])) {
      points(posatex[i],1,pch=19,col="blue",cex=3)}
  }
  

  
  }
  
#######################radar
 
 radar<-function(a,don,inv="O") {

# don<-data.frame(var = c("Prix du lait", "TB", "TP", "Cellules"), 
#  ferme = c(350, 40, 33, 10), 
 #  numrep = c(4, 31, 34, 37))
#  a<-3
 #  inv<-"N"

 
  data<-don   
  b<-data$numrep
  mil<-6
  
 
 lb<-length(b)
 bas<-c(rep(0,lb))
 haut<-c(rep(0,lb))
 
 for (i in 1:lb)
   {

 bas[i] <-c(posat[b[i],a]*(1-posat[b[i]+1,a]))
 haut[i]<-c(posat[b[i],a]*(1+posat[b[i]+1,a]))
   
 }
 

 par(bg="transparent")
 
 if (inv=="N") {
  data$bas<-haut
  data$haut<-bas
 data$moy<-(data$bas+data$haut)/2
 data$grad<-(data$haut-data$bas)/2
 data$ext<-data$moy+(mil*data$grad)
 data$cent<-data$moy-(mil*data$grad)
 data$coord<-((data$ferme-data$cent)/(data$ext-data$cent))*(mil*2)
 data$liminf<-c(rep((mil-1),length(data$var)))
 data$limsup<-c(rep((mil+1),length(data$var)))
 col1<-"green"
 col2<-"red"
 
 } else {
   data$bas<-bas
   data$haut<-haut
   data$moy<-(data$bas+data$haut)/2
   data$grad<-(data$haut-data$bas)/2
   data$cent<-data$moy+(mil*data$grad)
   data$ext<-data$moy-(mil*data$grad)
   data$coord<-((data$ferme-data$ext)/(data$cent-data$ext))*(mil*2)
   data$limsup<-c(rep((mil-1),length(data$var)))
   data$liminf<-c(rep((mil+1),length(data$var)))
   col1<-"red"
   col2<-"green"
 }
 
 
 data$coord<-ifelse(data$coord>0,data$coord,0)
 
 par(cex.axis=1.5)
 radial.plot(as.numeric(data$liminf), labels = data$var,show.grid.labels=0,label.prop=1.1,
             radlab=F,mar=c(rep(7,4)),
             rp.type="p",lwd=4, radial.lim=c(0,10), start = 1.57, 
             clockwise = TRUE, line.col = col1,xlab="")
 
 radial.plot(as.numeric(data$limsup), labels = data$var, 
             rp.type="p",lwd=4, radial.lim=c(0,10), start = 1.57, 
             clockwise = TRUE, line.col = col2,add=T)
 
 radial.plot(as.numeric(data$coord), labels = data$var, 
             rp.type="p", radial.lim=c(0,10), start = 1.57, 
             clockwise = TRUE, line.col = "blue",poly.col=rgb(0,0,1,0.5),add=T)
 
 
 #points(0,4,pch=16,cex=2)
 #points(0,-4,pch=16,cex=2)
 #points(4,4,pch=16,cex=2)
 }
 
 ###################
 
tabini<-function(farm=farm){
  farm<-farm
  prilait <- round(farm$DTBLCP_BLCP_PRODLAIT,0)
  prodviabl <- farm$DTBLCP_BLCP_PRODVIANDE * farm$DTBLCP_BL_LAITCOM/1000
  aid <- farm$ECOPA_AIDTOT
  autprod <- farm$DTBLCP_BLCP_AUTPROD
  prod <- farm$ECOPA_TOTPRO
  chop <- farm$ECOCO_TOTCO
  chst <- farm$ECOCSTB_TCSHAF-farm$ECOCSTB_MSA
  ebe <- prod - chop - chst
  nom<-"Initial"
  EBE_pb<-round(ebe/prod,2)
  dispo_umoex<-round((ebe-farm$ECOCSTB_AEMPLMT)/farm$STRUC_MOFAMI,0)
  laivendu <- farm$DTBLCP_BL_LAITCOM/1000
  remun<-farm$DTBLCP_BLCP_PRODLAIT+farm$DTBLCP_BLCP_PRODVIANDE+farm$DTBLCP_BLCP_AUTPROD +
       farm$DTBLCP_BLCP_AIDES-farm$DTBLCP_BLCP_APPANI-farm$DTBLCP_BLCP_APPSURF-
     farm$DTBLCP_BLCP_FELEVA-farm$DTBLCP_BLCP_MECA-farm$DTBLCP_BLCP_BATI-farm$DTBLCP_BLCP_FGEN-
    farm$DTBLCP_BLCP_FONCAP_FERMACAL
  remunex<-remun - farm$DTBLCP_BLCP_TRAV_SAL
  umoexp <- farm$DCOMP_MOFAMCLEBL
  umosal <- farm$DCOMP_MOSALCLEBL
  umoben <- 0
  umorem<-umoexp+umosal
  colad<-data.frame(Init=c(nom,umoexp,umosal,umorem+umoben,umoben,umorem,umosal,
                           prilait,aid,farm$ECOCSTB_AEMPLMT,
                           EBE_pb,dispo_umoex,round(laivendu/(umorem+umoben),0),
                           round(remun*laivendu/umorem/(SMIC*1.3),2),
                           round(remunex *laivendu/(SMIC * 1.3)/umoexp,2)
  ))
  return(colad)
  
}
 
 couprod<-function(){
   par(bg = "snow", mar = c(3, 3, 2, 2))
#    CP_alim <- 100
#    CP_surf <-50
#    CP_fel <- 15
#    CP_meca <- 52
#    amort_meca <- 35
#    CP_bat <- 35
#  CP_sal<-15
#    amort_bat <-20
#    CP_gest <- 25
#    CP_fon <-6
#    CP_cap <- 10
#    rem_fon <- 15
#    rem_cap <- 5
   
   CP_travex <- 120
   
   CPBL <- CP_alim + CP_surf + CP_fel + CP_meca + CP_bat + CP_gest + CP_fon + CP_cap + 
     CP_travex + CP_sal
   CP_amort <- amort_meca + amort_bat
   CP_chasup <- rem_cap + rem_fon + CP_travex
   CP_chacour <- CPBL - CP_amort - CP_chasup
   
   salim <- CP_alim + CP_surf + CP_meca + CP_fon
   bat <- CP_bat
   servis <- CP_fel + CP_gest
   cap <- CP_cap
   wsal <-  CP_sal
   wexp <- CP_travex
   
   prilait <- 350
   provia <- 25
   aid <- 15
   
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
   
   prirev=CPBL-aid-provia
   arrows(7,aid+provia,7,CPBL,col="blue",lwd=3,code=3,lty="solid")
   text(6.5,aid+provia,labels=paste("Prix de revient :",prirev,"Euros par 1000 litres"),
        srt=90,pos=4,col="blue")
 }
 