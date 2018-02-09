#fonction raster_to_raster_2: transforme le format raster de la fonction fc.read.raster() en format raster de la fonction raster() 
## raster : raster au format lu par la fonction fc.read.raster
fc.raster_to_raster_2=function(raster) { 
require(raster)
raster$xll=raster$xll*1000 ; raster$yll=raster$yll*1000                         # Changement d'unite :km --> m
raster$dX=raster$dX*1000 ; raster$dY=raster$dY*1000                             # Changement d'unite :km --> m
xll=raster$xll ; XLL=xll+raster$nbX*raster$dX     # positions de la matrice
yll=raster$yll ; YLL=yll+raster$nbY*raster$dY
a=raster$val[,raster$nbY:1] # matrice des valeurs (lecture de la derniere colonne vers la premiere) 
sortie=raster(t(a),xmn=xll,xmx=XLL,ymn=yll,ymx=YLL) # creation du raster avec transposition de la matrice
sortie
}

