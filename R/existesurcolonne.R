#' Vérifie si k se trouve sur la colonne j.
#' 
#' @return un booléen

existesurcolonne = function(j, k, A) {
  
  for (n in c(1:9) ) {
    if ( A[n,j] == k ) {
      return(TRUE)
    }
  }
  return(FALSE)
  
}