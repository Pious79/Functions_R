#_______________________________________________________________________________
# Affichage d'une commentaire texte de temporisation lors d'un calcul long
waitt <- function(i,b1,b2,titre="") {
  if (i == b1) message(paste(titre,"..."),appendLF = F)
  if (round((b2-b1)/10) > 0) {
    if (i%%round((b2-b1)/10) ==0) {
      message(paste(" /",round(i/(b2-b1+1)*100),"%"),appendLF = F)
    }
  }    
  if (i==b2) message("")
}

