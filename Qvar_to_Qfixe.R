##______________________________________________________________________________##
##  Function to change variable time step to stated time step                   ##
##  Patrick ARNAUD - fc.Qver_to_Qfixe.R                                         ##
##______________________________________________________________________________##
##------------------------------------------------------------------------------##
#   Fonction : Passe les debits d'un pas de temps variable a fixe               ##
##------------------------------------------------------------------------------##
#   Arguments : f_var [character] : chemin contenant le tableau de donnee a 
#                                   pas de temps variable
#               f_sortie [character] : chemin pour enregistrer le nouveau 
#                                      tableau a pas fixe
#               entete [numeric] : nombre de ligne a passer pour debuter la 
#                                  lecture
#               format_date [character] : format de la date dans le fichier 
#                                         source
#               format_fixe [character] : format de la date dans le fichier 
#                                         sortie
#               narm [boolean] : Prise en compte des valeurs manquantes
##------------------------------------------------------------------------------##
#   Sortie    : [dataframe] : tableau contenant les debits a pas de temps fixe
#                             avec les dates qui est enregistre a f_sortie
##------------------------------------------------------------------------------##
#---------------------------------------------------------------------------------

#___________________________________________________________________________________________________
#
# Pour les pluies annuelles 	: format_fixe = "%Y"
# Pour les pluies mensuelles  : format_fixe = "%Y%m"
#___________________________________________________________________________________________________
qvar_to_qfixe <- function(f_var="",f_sortie="",entete=3,format_date = "%d/%m/%Y %H:%M",format_fixe="%Y%m%d%H",narm=TRUE) {
  tmp = read.table(f_var,sep=";",skip=entete)
  date_var = strptime(tmp[,2],"%d/%m/%Y %H:%M",tz="GMT") ; nvar = length(date_var)
  Qvar     = as.numeric(as.vector(tmp[,4]))
  Qvar[which(Qvar < 0)] = NA
  date_1mn = seq(date_var[1],date_var[nvar],by=60)                                                # creation de la chronique des dates a 1 minute                                                              
  Q1mn     = rep(0,length(date_1mn))
  deb      = date_1mn[1] ; ideb = 1
  for (i in 1:(nvar-1)) {
    fc.waitt(i,1,nvar-1,"calcul P1 min")
    duree =  seq(date_var[i],date_var[i+1],by=60)
    Q = (Qvar[i+1]-Qvar[i]) / length(duree)
    n1 = ideb + as.numeric(difftime(duree[1],deb,units="mins"))
    if (n1 !=ideb) message("Probleme ...")
    n2 = ideb + as.numeric(difftime(duree[length(duree)],deb,units="mins"))
    plage = n1:n2
    #plage =  which(date_1mn == duree[1]):which(date_1mn == duree[length(duree)])
    Q1mn[plage] = Qvar[i] + Q*((1:length(duree))-1)
    ideb = n2
    deb  = duree[length(duree)]
  }
  message("Aggregation sur le pas de temps fixe")
  #date_fixe = seq(min(date_1mn),max(date_1mn) + tfixe*60,paste(tfixe,"min"))
  date_fixe = format(date_1mn,format_fixe)
  Qfixe = split(Q1mn,date_fixe)                                                                   # on regroupe par pas de temps fixe
  Qfixe = lapply(Qfixe,mean,na.rm=narm)                                                            # on somme
  write(paste0("Date ; Debit"),file=f_sortie)
  write.table(cbind(as.numeric(names(Qfixe)[-1]),round(unlist(Qfixe)[-1],2)),sep=";",file =f_sortie,append=TRUE,row.names=FALSE,col.names=FALSE)	
}

