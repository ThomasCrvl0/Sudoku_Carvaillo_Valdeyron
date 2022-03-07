emplacementdisponible = function(i,j, k, A) {     # Vérifie si k peut être inséré à l'emplacement (i,j). True : oui, False : non.
  
  if ( !existesurligne(i, k, A) && !existesurcolonne(j, k, A) && !existesurbloc(i,j, k, A) ) {
    return( TRUE )
  }
  
  return( FALSE )
}