##______________________________________________________________________________##
##  Function to filter data with Hanning filter order 2                         ##
##  Pierre L'HERMITE - 2017-07-19 - fc.Hanning                                  ##
##______________________________________________________________________________##
##------------------------------------------------------------------------------##
#   Fonction : Calcul les donnees avec le filtre passe bas d'Hanning d'ordre 2  ##
##------------------------------------------------------------------------------##
#   Arguments : data [zoo] : vecteur contenant les donnees journalieres,
#                            mensuelles, saisonnieres et annuelles, 
#                            avec la date au format %Y-%m-%d
##------------------------------------------------------------------------------##
#   Sortie    : Hanzoo [zoo] : vecteur contenant les donnees filtrees avec la 
#                              date au format %Y-%m-%d
##------------------------------------------------------------------------------##
#---------------------------------------------------------------------------------

hanning <- function(data){
  library(hydroTSM)
  ## Verification arguments d'entree
  if (!is.zoo(data)) { stop("Data must be a zoo"); return(NULL) }
  
  Ind_run <- which(!is.na(coredata(data)))
  #Creation et remplissage du vecteur par le filtre d'Hanning
  Han <- rep(NA, length(data))
  Han[Ind_run[1]] <- ((0.54*coredata(data[Ind_run[1]]))
                      + (0.46*coredata(data[Ind_run[2]])))
  Han[Ind_run[2]] <- ((0.25*coredata(data[Ind_run[1]]))
                      + (0.5*coredata(data[Ind_run[2]]))
                      + (0.25*coredata(data[Ind_run[3]])))
  
  for (i in c(3:(length(Ind_run)-2))){
    Han[Ind_run[i]] <- ((0.06*coredata(data[Ind_run[i-2]]))
                        + (0.25*coredata(data[Ind_run[i-1]]))
                        + (0.38*coredata(data[Ind_run[i]]))
                        + (0.25*coredata(data[Ind_run[i+1]]))
                        + (0.06*coredata(data[Ind_run[i+2]])))
  }
  
  Han[Ind_run[length(Ind_run)]-1] <- ((0.25*coredata(data[Ind_run[length(Ind_run)]-2]))
                                      + (0.5*coredata(data[Ind_run[length(Ind_run)]-1])) 
                                      + (0.25*coredata(data[Ind_run[length(Ind_run)]])))
  Han[Ind_run[length(Ind_run)]] <- ((0.54*coredata(data[Ind_run[length(Ind_run)]]))
                                    +(0.46*coredata(data[Ind_run[length(Ind_run)]-1])))
  
  Hanzoo <- zoo(as.numeric(Han), index(data))
  return(Hanzoo)
}