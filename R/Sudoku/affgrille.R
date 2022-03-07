#' Affichage de la grille de sudoku
#'
#' @return un sudoku complet sous forme de grille

library(gtable)
library(grid)
library(gridExtra)

affgrille <- function(l){
  Z <- grilleincomplete(l)
  A = Z[[1]]
  B = Z[[2]]
  cmp = Z[[3]]
  
  plot(0:10, 0:10, type="n", xlab=NA, ylab=NA, axes = FALSE)
  
  for (i in 1:9) {
    for (j in 1:9) {
    if(A[i,j] == 0){
      text(j, 10-i,"", offset=0)
    }else{
      text(j, 10-i, A[i,j], offset=0)
    }
    }
  }
  for (i in seq(0.5,9.5,3)) {
    lines(c(0.5, 9.5), c(i, i), lwd=2)
    lines(c(i, i), c(0.5, 9.5), lwd=2)
  }
  
  for(i in seq(0.5, 10)){
    lines(c(i,i),c(0.5,9.5))
    lines(c(0.5,9.5),c(i,i))
  }
  return(B)
}

