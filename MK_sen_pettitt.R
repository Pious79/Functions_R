##______________________________________________________________________________##
##  Function to calculate Mann-Kendall, Pettitt, Sen                            ##
##  Pierre L'HERMITE - 2017-07-31 - fc.trace_MK_Sen_Pettitt                     ##
##______________________________________________________________________________##
##------------------------------------------------------------------------------##
#   Fonction : Calcul les tests de Mann-Kendall, de Pettitt et de Sen pour
#              obtenir la tendance, la rupture et la pente de regression
##------------------------------------------------------------------------------##
#   Arguments : data [zoo] : vecteur contenant les donnees (annuel, mensuel,
#                            saisonnier, journalier avec la date %Y-%m-%d)
##------------------------------------------------------------------------------##
#   Sortie    : Res [list] : Resultats des differents tests :
#                            MK [] : recapitulatif des resultats de Mann-Kendall
#                            signeMK [vect] : contenant le signe de MK 
#                            correspondant à la valeur de la pvalue (- p>0.1,
#                            + 0.1>p>0.05, ++ 0.05>p>0.01, et +++ p<0.01)
#                            Pettitt [] : recapitulatif des resultats de Pettitt
#                            signeP [vect] : contenant le signe de Pettitt 
#                            correspondant à la valeur de la pvalue (- p>0.1,
#                            + 0.1>p>0.05, ++ 0.05>p>0.01, et +++ p<0.01)
#                            Pente [numeric] : pente obtenu par le test de Sens
##------------------------------------------------------------------------------##
#---------------------------------------------------------------------------------

fc.MK_Sen_Pettitt<-function(data)
{
  if(missing(data)) { print("Must have a data \n"); return()}
  
  library(zoo)
  library(pheno)
  library(Kendall)
  library(trend)
  library(stats)
  
  #na.index permet d'eliminer les NA dans le zoo de donnee
  na.index <- which(is.na(data))
  if(length(na.index) != 0){
    data<-data[-na.index]
  }else {data}
  
  #extraire annee
  ystart<-as.numeric(format(start(data), format="%Y"))
  yend<-as.numeric(format(end(data), format="%Y"))
  
  val=coredata(data)
  datats<-ts(val, start = 1 , end = length(val))
  
  #Test de Pettitt, de MK et de Sen
  Pettitt<-pettitt.test(datats)
  pente<-sens.slope(datats, level=0.95)
  MK<-mk.test(datats)
  
  if(MK$pvalue<=0.01){
    (signeMK="+++")
  } else if(MK$pvalue>=0.01 & MK$pvalue<=0.05){
    (signeMK="++")
  } else if(MK$pvalue>=0.05 & MK$pvalue<=0.1){
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
  
  Res<-list(MK, signeMK, Pettitt, signeP, pente )
  return(Res)
}