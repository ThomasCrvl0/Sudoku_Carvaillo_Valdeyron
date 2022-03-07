existesurligne = function(i, k, A) {   # Vérifie si k se trouve sur la ligne i.
  
  for (n in c(1:9) ) {
    if ( A[i,n] == k ) {
      return(TRUE)
    }
  }
  return(FALSE)
  
}