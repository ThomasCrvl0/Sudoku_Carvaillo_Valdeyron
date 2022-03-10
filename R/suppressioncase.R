#'  Supprime une case aléatoire du sudoku A.
#'
#' @return Le sudoku A avec une case en moins.

suppressioncase <- function(A) {
  
  t = 0
  while (t == 0) {
    
    i = sample( c(1:9), size = 1 )
    j = sample( c(1:9), size = 1 )
    
    if ( A[i,j] != 0 ) {
      
      A[i,j] = 0
      t = 1
    }
    
  }
  return(A)
}