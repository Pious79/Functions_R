#___________________________________________________________________________________________________
#
# Fonction de nettoyage des donnees "graphiquement"
# 	- val   = tableau de valeurs d'enregistrement : 
#						* la premier colonne contient les valeurs a tracer et critique
#						* les autres colonnes (facultatives) contiennent d'autres infos associees a l'enregistrement
#		- date  = dates des enregistrements
#		- P     = xts de pluie
#		- Q			= xts de debit
#___________________________________________________________________________________________________
	fc.nettoyage_graphique = function(val,date,P=numeric(0),Q=numeric(0)) {
		require(xts)		
		zone = carre = list()
		plage_NA = plage_moins = numeric(0)
		x = difftime(date,date[1],unit="hours")
		nb_date = length(date)
		retour = difftime(date[2:nb_date],date[1:(nb_date-1)],unit="hours")
		plage_retour = which(retour < 0)           # retour dans les dates
		if ( is.null(dim(val)[1])) y = val
		if (!is.null(dim(val)[1])) y = val[,1]
		# Donnees de controle
		if (length(Q) !=0) {
			x_Q = difftime(index(Q),date[1],unit="hours")
			plage = which(x_Q > min(x,na.rm=T) & x_Q < max(x,na.rm=T))	
			x_Q = x_Q[plage]
			y_Q = as.numeric(Q[plage])
		}
		if (length(P) !=0) {
			x_P = difftime(index(P),date[1],unit="hours")
			plage = which(x_P > min(x,na.rm=T) & x_P < max(x,na.rm=T))	
			x_P = x_P[plage]
			y_P = as.numeric(P[plage])
		}
		x_zoom_0 = x_zoom = range(x)
		if (length(Q) !=0) {y_zoom_0 = y_zoom = c(min(min(y,na.rm=T),min(y_Q,na.rm=T)),max(max(y,na.rm=T),max(y_Q,na.rm=T))) * 1.2}
		if (length(Q) ==0) {y_zoom_0 = y_zoom = c(min(y,na.rm=T),max(y,na.rm=T)* 1.2)}
		repeat {
			plot(x,y,type="l",main=date[1],xlab="durée (h)",ylab="Q (l/s)",xlim=x_zoom,ylim=y_zoom)
			if (length(Q) !=0) lines(x_Q,y_Q,col="red")
			if (length(P) !=0) ratio_P =y_zoom[2] / max(y_P,na.rm=T) / 4
			if (length(P) ==0) {y_P = 0 ; ratio_P = 0}
			if (length(P) !=0) lines(x_P,y_zoom[2]/1.1-y_P*ratio_P,axes=F,col=4,main="",auto.grid=FALSE)
			points(x[plage_moins],y[plage_moins],col="red",pch=20)
			points(x[plage_NA],y[plage_NA],col="gray",pch=20)
			points(x[plage_retour],y[plage_retour],col="yellow",pch="O")
			# Choix de l'action a faire ..
			loc = locator(n=2,type="p",pch="+",col="green")
			zone[[1]] = c(loc$x[1],loc$y[1],loc$x[2],loc$y[2])
			# Bouton d'actions
			zone[[2]] = c(x_zoom[1]+0.90*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.95*(y_zoom[2]-y_zoom[1]),x_zoom[1]+1.00*(x_zoom[2]-x_zoom[1]),y_zoom[1]+1.02*(y_zoom[2]-y_zoom[1])) # Supprimer
			zone[[3]] = c(x_zoom[1]+0.80*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.95*(y_zoom[2]-y_zoom[1]),x_zoom[1]+0.90*(x_zoom[2]-x_zoom[1]),y_zoom[1]+1.02*(y_zoom[2]-y_zoom[1])) # Mettre en NA
			zone[[4]] = c(x_zoom[1]+0.70*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.95*(y_zoom[2]-y_zoom[1]),x_zoom[1]+0.80*(x_zoom[2]-x_zoom[1]),y_zoom[1]+1.02*(y_zoom[2]-y_zoom[1])) # Valider
			zone[[5]] = c(x_zoom[1]+0.60*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.95*(y_zoom[2]-y_zoom[1]),x_zoom[1]+0.70*(x_zoom[2]-x_zoom[1]),y_zoom[1]+1.02*(y_zoom[2]-y_zoom[1])) # Reset
			zone[[6]] = c(x_zoom[1]+0.50*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.95*(y_zoom[2]-y_zoom[1]),x_zoom[1]+0.60*(x_zoom[2]-x_zoom[1]),y_zoom[1]+1.02*(y_zoom[2]-y_zoom[1])) # Sortie
			zone[[7]] = c(x_zoom[1]+0.30*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.95*(y_zoom[2]-y_zoom[1]),x_zoom[1]+0.40*(x_zoom[2]-x_zoom[1]),y_zoom[1]+1.02*(y_zoom[2]-y_zoom[1])) # ZOOM
			zone[[8]] = c(x_zoom[1]+0.20*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.95*(y_zoom[2]-y_zoom[1]),x_zoom[1]+0.30*(x_zoom[2]-x_zoom[1]),y_zoom[1]+1.02*(y_zoom[2]-y_zoom[1])) # ZOOM
			carre[[1]] = cbind(c(zone[[1]][1],zone[[1]][1],zone[[1]][3],zone[[1]][3],zone[[1]][1]),c(zone[[1]][2],zone[[1]][4],zone[[1]][4],zone[[1]][2],zone[[1]][2]))
			lines(carre[[1]],col="green")
			plage = which(x > zone[[1]][1] & x < zone[[1]][3] & y > zone[[1]][2] & y < zone[[1]][4])
			points(x[plage],y[plage],col="green",pch=20)
			for (i in 2:8) {
				carre[[i]] = cbind(c(zone[[i]][1],zone[[i]][1],zone[[i]][3],zone[[i]][3],zone[[i]][1]),c(zone[[i]][2],zone[[i]][4],zone[[i]][4],zone[[i]][2],zone[[i]][2]))
				lines(carre[[i]])
			}
			text(x_zoom[1]+0.95*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.98*(y_zoom[2]-y_zoom[1]),"Moins",col="red")
			text(x_zoom[1]+0.85*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.98*(y_zoom[2]-y_zoom[1]),"NA",col="gray")
			text(x_zoom[1]+0.75*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.98*(y_zoom[2]-y_zoom[1]),"Valid")
			text(x_zoom[1]+0.65*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.98*(y_zoom[2]-y_zoom[1]),"Reset")
			text(x_zoom[1]+0.55*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.98*(y_zoom[2]-y_zoom[1]),"Sortie")
			text(x_zoom[1]+0.35*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.98*(y_zoom[2]-y_zoom[1]),"ZOOM +",col="blue")
			text(x_zoom[1]+0.25*(x_zoom[2]-x_zoom[1]),y_zoom[1]+0.98*(y_zoom[2]-y_zoom[1]),"zoom -",col="blue3")
			loc = locator(1)
			if (loc$x > zone[[2]][1] & loc$x < zone[[2]][3] & loc$y > zone[[2]][2] & loc$y < zone[[2]][4]) { # Pour suppression
				plage_moins = c(plage_moins,which(x > zone[[1]][1] & x < zone[[1]][3] & y > zone[[1]][2] & y < zone[[1]][4]))
			}
			if (loc$x > zone[[3]][1] & loc$x < zone[[3]][3] & loc$y > zone[[3]][2] & loc$y < zone[[3]][4]) { # Pour mise en NA
				plage_NA = c(plage_NA,which(x > zone[[1]][1] & x < zone[[1]][3] & y > zone[[1]][2] & y < zone[[1]][4]))
			}
			if (loc$x > zone[[4]][1] & loc$x < zone[[4]][3] & loc$y > zone[[4]][2] & loc$y < zone[[4]][4]) { # Application des choix
				if (length(plage_NA) !=0) {
					y[plage_NA] = NA
					if ( is.null(dim(val)[1])) val[plage_NA] = NA
					if (!is.null(dim(val)[1])) val[plage_NA,1] = NA
				}
				if (length(plage_moins)!=0) {
					x = x[-plage_moins]
					y = y[-plage_moins]
					date = date[-plage_moins]
					if ( is.null(dim(val)[1])) val = val[-plage_moins]
					if (!is.null(dim(val)[1])) val = val[-plage_moins,]
					plage_NA = numeric(0)
					plage_moins = numeric(0)
				}
			}
			if (loc$x > zone[[5]][1] & loc$x < zone[[5]][3] & loc$y > zone[[5]][2] & loc$y < zone[[5]][4]) { # Annulation
				plage_NA = numeric(0)
				plage_moins = numeric(0)
			}
			if (loc$x > zone[[6]][1] & loc$x < zone[[6]][3] & loc$y > zone[[6]][2] & loc$y < zone[[6]][4]) { # Sortie
				break()
			}
			if (loc$x > zone[[7]][1] & loc$x < zone[[7]][3] & loc$y > zone[[7]][2] & loc$y < zone[[7]][4]) { # Pour le zoom +
				x_zoom = c(zone[[1]][1],zone[[1]][3])
				y_zoom = c(zone[[1]][2],zone[[1]][4])
			}
			if (loc$x > zone[[8]][1] & loc$x < zone[[8]][3] & loc$y > zone[[8]][2] & loc$y < zone[[8]][4]) { # Pour le zoom -
				x_zoom = x_zoom_0
				y_zoom = y_zoom_0
			}
		}
	  list(date,val)
	}

