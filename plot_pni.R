##______________________________________________________________________________##
##  Function to plot Mann-Kendall, Pettitt, Sen, Sequential Mann-Kendall        ##
##  Pierre L'HERMITE - 2017-07-19 - fc.trace_MK_Sen_SQMK_Pettitt                ##
##______________________________________________________________________________##
##------------------------------------------------------------------------------##
#   Fonction : Trace deux graphiques, le premier avec les donnees tracees 
#              chronologiquement, avec les resultats de Mann-Kendall (tendance),
#              de Pettitt (rupture) et de la pente de Sen (regression).
#              Le second est le test sequentiel de Mann-Kendall.
##------------------------------------------------------------------------------##
#   Arguments : data [zoo] : vecteur contenant les donnees (annuel, mensuel,
#                            saisonnier, journalier avec la date %Y-%m-%d)
#               type_data [character] : indiquer quelle donnee est en entree
#                                       (temperature, debit...)
#               bv_nom [character] : indiquer le nom de l'entree etudiee 
#                                    (Maurets, 80114...)
#               nom_axex [character] : indiquer le titre de l'axe x 
#                                      (Annee, Mois...)
#               nom_axey [character] : indiquer le titre de l'axe y 
#                                     (Cumul mensuel des debits (mm) ou ...)
##------------------------------------------------------------------------------##
#   Sortie    : Deux graphiques
##------------------------------------------------------------------------------##
plot_pni <- function(data, type_data="", bv_nom="", nom_axex="Temps",
                     nom_axey="Donnees [unite]")
{
  ## Verification arguments d'entree
  if (!is.zoo(data)) { stop("Data must be a zoo"); return(NULL) }
  ## Verification de donnees
  Ind_run <- which(!is.na(coredata(data)))
  if(length(Ind_run) == 0) { stop("Data must have values"); return(NULL)}
  
  library(pheno)
  library(Kendall)
  library(trend)
  library(stats)
  
  datatest <- data[Ind_run]
  
  #extraire annee
  ystart<-as.numeric(format(start(datatest), format="%Y"))
  yend<-as.numeric(format(end(datatest), format="%Y"))
  
  #Test de Pettitt, de MK et de Sen
  Pettitt <- pettitt.test(coredata(datatest))
  pente <- sens.slope(coredata(datatest), conf.level = 0.95)
  MK <- mk.test(coredata(datatest))
  
  Rupy <- format(index(datatest[Pettitt$estimate]), format = "%Y-%m")
  
  if(MK$p.value <= 0.01){
    (signeMK="+++")
  } else if(MK$p.value >= 0.01 & MK$p.value <= 0.05){
    (signeMK="++")
  } else if(MK$p.value >= 0.05 & MK$p.value <= 0.1){
    (signeMK="+")
  } else {
    (signeMK="-")}
  
  if(Pettitt$p.value<=0.01){
    (signeP="+++")
  } else if(Pettitt$p.value>=0.01 & Pettitt$p.value<=0.05){
    (signeP="++")
  } else if(Pettitt$p.value>=0.05 & Pettitt$p.value<=0.1){
    (signeP="+")
  } else {
    (signeP="-")}
  
  #Modification pour le plot
  coredata(datatest)[is.na(coredata(datatest))] <- 100
  indexlabel <- format(index(datatest), format = "%Y-%m")
  datatest <- c(100, coredata(datatest), 100)
  
  #Plot avec la couleur selon la valeur
  plot(datatest, type="l", xaxt="n", ylim=c(round(min(coredata(datatest), na.rm=T),5), 
                                            round(max(coredata(datatest), na.rm=T),5)),
       xlab=nom_axex, ylab=nom_axey,
       main=paste0(type_data ," a ",bv_nom," de ",ystart," a ",yend,
                   " avec la regression de Sen en rouge 
                   et l'annee possible de rupture selon Pettitt en bleu"),
       cex.main=0.9, cex.lab=1, cex.axis=1, col ="cornflowerblue")
  grid()
  axis(1, at=seq(1, length(datatest), 60),
       labels=indexlabel[seq(1, length(datatest), 60)])
  datatest.pos <- ifelse(coredata(datatest) > 100, datatest, 100)
  datatest.neg <- ifelse(coredata(datatest) <= 100, datatest, 100)
  polygon(datatest.pos, col = "lightseagreen", border = NA)
  polygon(datatest.neg, col = "red3", border = NA)
  lines(datatest, col = "dark grey")
  abline(h = 100)
  legend(1,round(max(coredata(datatest), na.rm=T),5), 
         paste("Test de la pente de Sen\nY=",
               as.character(round(pente$estimates,4)), "*X"), bty="n", cex=0.6)
  abline(a=0, b=pente$estimates, col="mediumpurple3")
  legend((length(datatest)/3),round(max(coredata(datatest), na.rm=T),5), 
         paste("Test de Mann-Kendall\nTau=",
               as.character(round(as.numeric(MK$estimates[3]),3)), 
               "et pvalue=", as.character(round(MK$p.value,3)), "(",signeMK,")"),
         bty="n", cex=0.6)
  if(Pettitt$p.value < 0.1){
    abline(v = Pettitt$estimate, col="darkblue")
    legend((2*(length(datatest))/3),round(max(coredata(datatest), na.rm=T),5),
           bty="n", cex=0.6,
           paste("Test de rupture de Pettitt: pvalue=",
                 as.character(round(Pettitt$p.value,4)), 
                 "(", signeP, ")","\nAnnee de la rupture (bleu fonce) :", Rupy))
  }
  
  #test sequentiel de MK
  SQMK<-seqMK(coredata(datatest))
  
  #plot de SQMK
  plot(c(1:length(datatest)),SQMK$prog, type="l",
       ylim = c(round(min(SQMK$prog, SQMK$retr, na.rm=T),0)-2,
                round(max(SQMK$prog, SQMK$retr, na.rm=T),0)+2),
       ylab=c("u(t) [SU] et u'(t) [SU]"), 
       xlab=nom_axex , xaxt="n", cex.main=1, cex.lab=1, cex.axis=1,
       main=paste0("Test sequentiel de Mann-Kendall sur ", type_data ,
                   "\na ",bv_nom," de ",ystart," a ",yend))
  lines(c(1:length(datatest)),SQMK$retr, lty=2)
  axis(1, at=seq(1, length(datatest), 60), labels=indexlabel[seq(1, length(datatest), 60)])
  legend("topright", legend=c("u(t)", "u'(t)"), lty=1:2, bty="n",cex=1)
  abline(h=1.96, lty=3, col="darkblue")
  abline(h=-1.96, lty=3, col="darkblue")
  abline(h=0, col="gray64")
  
  return(NULL)
}
