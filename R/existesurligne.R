#' Vérifie si k se trouve sur la ligne i.
#' 
#' @return un booléen

existesurligne = function(i, k, A) {   
  
  for (n in c(1:9) ) {
    if ( A[i,n] == k ) {
      return(TRUE)
    }
  }
  return(FALSE)
  
}