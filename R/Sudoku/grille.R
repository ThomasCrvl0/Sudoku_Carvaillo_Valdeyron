#' Affichage de la grille de sudoku
#'
#' @return une grille complète sous forme de matrice

grille <- function() {
  
  L1 = sample(9)   # On génère une permutation de 9 nombres, qui devient la première ligne.

  L2 = c( L1[7:9], L1[1:3], L1[4:6] )
  L3 = c( L1[4:6], L1[7:9], L1[1:3] )

  nomco <- c('C1','C2','C3','C4','C5','C6','C7','C8','C9')
  nomli <- c('L1','L2','L3','L4','L5','L6','L7','L8','L9')
  A = matrix( NA, ncol=9, nrow=9, byrow=FALSE, dimnames = list(nomli,nomco) )   # On génère la matrice vide.

  # On la remplit ensuite :

  A[1,] = L1
  A[2,] = L2
  A[3,] = L3


  for (j in c(1:9) ) {
    
    a = (j-1)%/%3  # quotient de la div euclidienne
    b = (j+2)%%3   # reste de la div euclidienne
  
    if (b==0) {b = 3}
  
    A[4:6,j] = A[1:3, a*3 + b ]
  }

  for (j in c(1:9) ) {
  
    a = (j-1)%/%3  # quotient de la div euclidienne
    b = (j+1)%%3   # reste de la div euclidienne
  
    if (b==0) {b = 3}
  
    A[7:9,j] = A[1:3, a*3 + b ]
  }

  return(A)
}

############### TEST ###############


A = grille ()

for (t in c(1:60) ) {
 A = suppressioncase (A) 
}

L = rang(A)
n = length(L)

Lx = L[ 1:(n/2) ]
Ly = L[ (n/2 +1):n ]


backtraking(A, 20000)


grilleincomplete(1)


####################################

