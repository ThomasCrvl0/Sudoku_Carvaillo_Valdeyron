#' 
#' Vérifie si k peut être inséré à l'emplacement (i,j).
#' @return un booléen

emplacementdisponible = function(i,j, k, A) {
  
  if ( !existesurligne(i, k, A) && !existesurcolonne(j, k, A) && !existesurbloc(i,j, k, A) ) {
    return( TRUE )
  }
  
  return( FALSE )
}