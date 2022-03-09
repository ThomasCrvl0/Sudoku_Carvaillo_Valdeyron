#' 
#' Vérifie si k peut être insérer dans le bloc correspondant à la case (i,j)
#' @return un booléen


existesurbloc = function(i,j, k, A) {
  
  # On identifie le bloc auquel appartient (i,j), ce sera le bloc (a,b).
  
  a = (i-1)%/%3   # quotient de la div euclidienne  # a : ligne
  b = (j-1)%/%3   # quotient de la div euclidienne  # b : colonne
  
  a = a*3
  b = b*3
  
  for ( n in c(a+1, a+2, a+3) ) {
    for ( m in c(b+1, b+2, b+3) ) {
      
      if ( A[n,m] == k ) {
        return(TRUE)
      }
      
    }
  }
  return(FALSE)
  
}