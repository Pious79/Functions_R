##____________________________________________________________________________##
##  Function to plot Mann-Kendall, Pettitt, Sen, Sequential Mann-Kendall      ##
##  Pierre L'HERMITE - 2017-07-19 - plot_trend.R                              ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Fonction : Trace deux graphiques, le premier avec les donnees tracees 
#              chronologiquement, avec les resultats de Mann-Kendall (tendance),
#              de Pettitt (rupture) et de la pente de Sen (regression).
#              Le second est le test sequentiel de Mann-Kendall.
##----------------------------------------------------------------------------##
#   Arguments : data [zoo] : vecteur contenant les donnees (annuel, mensuel,
#                            saisonnier, journalier avec la date %Y-%m-%d)
#               trend [bool] : add results of different trend and breaing point
#                              tests
#               type_data [character] : indiquer quelle donnee est en entree
#                                       (temperature, debit...)
#               bv_nom [character] : indiquer le nom de l'entree etudiee 
#                                    (Maurets, 80114...)
#               nom_axex [character] : indiquer le titre de l'axe x 
#                                      (Annee, Mois...)
#               nom_axey [character] : indiquer le titre de l'axe y 
#                                     (Cumul mensuel des debits (mm) ou ...)
##----------------------------------------------------------------------------##
#   Sortie    : Deux graphiques
##----------------------------------------------------------------------------##
plot_trend <- function(data, trend = TRUE, type_data="", bv_nom="",
                       nom_axex="Temps", nom_axey="Donnees [unite]",
                       mid_value = 0)
{
  ## Verification arguments d'entree
  if (!is.zoo(data)) { stop("Data must be a zoo"); return(NULL) }
  ## Verification de donnees
  Ind_run <- which(!is.na(coredata(data)))
  if(length(Ind_run) == 0) { stop("Data must have values"); return(NULL)}
  
  datatest <- data[Ind_run]
  
  if (trend == TRUE){
    res_test <- mk_sen_pettitt(datatest)
  }
  #extraire annee
  ystart <- as.numeric(format(start(datatest), format="%Y"))
  yend <- as.numeric(format(end(datatest), format="%Y"))
  
  breaking_year <- format(index(datatest[res_test[[3]]$estimate]),
                          format = "%Y-%m")
  
  #Modification pour le plot
  coredata(datatest)[is.na(coredata(datatest))] <- mid_value
  indexlabel <- format(index(datatest), format = "%Y-%m")
  datatest <- c(mid_value, coredata(datatest), mid_value)
  
  #Plot avec la couleur selon la valeur
  plot(datatest, type = "l", xaxt = "n",
       ylim = c(round(min(coredata(datatest), na.rm=T),5),
                round(max(coredata(datatest), na.rm=T),5)),
       xlab = nom_axex, ylab = nom_axey,
       main = paste0(type_data ," a ",bv_nom," de ",ystart," a ",yend,
                     " avec la regression de Sen en rouge 
                   et l'annee possible de rupture selon Pettitt en bleu"),
       cex.main = 0.9, cex.lab = 1, cex.axis = 1)
  grid()
  axis(1, at = seq(1, length(datatest), 60),
       labels = indexlabel[seq(1, length(datatest), 60)])
  datatest.pos <- ifelse(coredata(datatest) > mid_value, datatest, mid_value)
  datatest.neg <- ifelse(coredata(datatest) <= mid_value, datatest, mid_value)
  polygon(datatest.pos, col = "lightseagreen", border = NA)
  polygon(datatest.neg, col = "red3", border = NA)
  lines(datatest, col = "dark grey")
  abline(h = mid_value)
  
  if (trend == TRUE){
    legend(1,round(max(coredata(datatest), na.rm = TRUE), 5), 
           paste("Test de la pente de Sen\nY=",
                 as.character(round(res_test[[5]], 4)), "*X"), bty = "n",
           cex = 0.6)
    abline(a = mid_value, b = res_test[[5]], col = "mediumpurple3")
    legend((length(datatest)/3),round(max(coredata(datatest), na.rm = TRUE),5), 
           paste("Test de Mann-Kendall\nTau =",
                 as.character(round(as.numeric(res_test[[1]]$estimates[3]),3)), 
                 "et pvalue =", as.character(round(res_test[[1]]$p.value,3)),
                 "(", res_test[[2]], ")"), bty = "n", cex = 0.6)
    if(res_test[[3]]$p.value < 0.1){
      abline(v = res_test[[3]]$estimate, col = "darkblue")
      legend((2*(length(datatest))/3),round(max(coredata(datatest),
                                                na.rm = TRUE),5),
             bty = "n", cex = 0.6,
             paste("Test de rupture de Pettitt: pvalue=",
                   as.character(round(res_test[[3]]$p.value,4)), 
                   "(", res_test[[4]], ")","\nAnnee de la rupture (bleu fonce) :",
                   breaking_year))
    }
  }
  
  #test sequentiel de MK
  sqmk <- seqMK(coredata(datatest))
  
  #plot de SQMK
  plot(c(1:length(datatest)),sqmk$prog, type="l",
       ylim = c(round(min(sqmk$prog, sqmk$retr, na.rm = TRUE),0)-2,
                round(max(sqmk$prog, sqmk$retr, na.rm = TRUE),0)+2),
       ylab = c("u(t) [SU] et u'(t) [SU]"), 
       xlab = nom_axex , xaxt = "n", cex.main = 1, cex.lab = 1, cex.axis = 1,
       main = paste0("Test sequentiel de Mann-Kendall sur ", type_data ,
                     "\na ",bv_nom," de ",ystart," a ",yend))
  lines(c(1:length(datatest)),sqmk$retr, lty = 2)
  axis(1, at = seq(1, length(datatest), 60),
       labels = indexlabel[seq(1, length(datatest), 60)])
  legend("topright", legend = c("u(t)", "u'(t)"), lty = 1:2, bty = "n",cex = 1)
  abline(h = 1.96, lty = 3, col = "darkblue")
  abline(h = -1.96, lty = 3, col = "darkblue")
  abline(h = 0, col = "gray64")
  
  return(NULL)
}
