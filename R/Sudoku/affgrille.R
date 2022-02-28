#' Affichage de la grille de sudoku
#'
#' @return un sudoku complet sous forme de grille

library(gtable)
library(grid)

affgrille <- function(){
  
  A <- grille()
  grl <- tableGrob(A, widths=unit(rep(1.5,9),"cm"),heights=unit(rep(1.5,9),"cm"))
  grid.newpage()
  grid.draw(grl)
}
affgrille()

