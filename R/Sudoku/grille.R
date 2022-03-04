#' Affichage de la grille de sudoku
#'
#' @return une grille complète sous forme de matrice
#'


# I) GENERATION DE LA GRILLE COMPLETE

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


############################################################################


# II) FONCTION DE RESOLUTION BACKTRAKING


####### 1) Fonctions préliminaires d'éxistences :


# FALSE : k n'est pas présent. TRUE : k est présent.

existesurligne = function(i, k, A) {   # Vérifie si k se trouve sur la ligne i.
  
  for (n in c(1:9) ) {
    if ( A[i,n] == k ) {
      return(TRUE)
    }
  }
  return(FALSE)
  
}

existesurcolonne = function(j, k, A) {  # Vérifie si k se trouve sur la colonne j.
  
  for (n in c(1:9) ) {
    if ( A[n,j] == k ) {
      return(TRUE)
    }
  }
  return(FALSE)
  
}
  

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

emplacementdisponible = function(i,j, k, A) {     # Vérifie si k peut être inséré à l'emplacement (i,j). True : oui, False : non.
  
  if ( !existesurligne(i, k, A) && !existesurcolonne(j, k, A) && !existesurbloc(i,j, k, A) ) {
    return( TRUE )
  }
  
  return( FALSE )
}


####### 2) Fonction liste des rangs :


rang = function(A) {
  
  B = matrix(1000, ncol=9, nrow=9)  # Matrice des "rangs"   # 1000 = NA (pour la comparaison ci-dessous).
  
  for ( i in c(1:9) ) {     # On parcourt toute la matrice
    for ( j in c(1:9) ) {
      
      if ( A[i,j] == 0 ) {   # Si la case (i,j) de A possède un chiffre manquant/caché.
        
        n = 0
        for ( k in c(1:9) ) {  # On teste tous les chiffres candidats k possibles, et on n'en compte le total = n.
          
          if ( emplacementdisponible(i, j, k, A) ) {
            n = n+1
          }
        }
        
        if (n == 0) {              # On ne peut mettre aucun chiffre dans la case (i,j)...
          return( "ERROOOOOOOR" )  # cela n'est pas censé arriver.
          }
        
        B[i,j] = n   # Cela correspond au "rang" de la case (i,j).
      }
      
    }
  }
  
  
  # Liste des positions par rang croissant. Plus le rang est élevé, et moins bien sera classé la case.
  
  Lx = list() # Liste des coordonnées sur x.
  Ly = list() # Liste des coordonnées sur y.
  
  t = 0
  while (t == 0) {
    
    i = 1
    j = 1
    
    for ( n in c(1:9) ) {
      for ( m in c(1:9) ) {
      
        if ( B[n,m] < B[i,j] ) {   # On vérifie si la case (n,m) n'aurait pas un plus petit rang que (i,j).
          i = n
          j = m
        }
      
      }
    }
  
    if ( B[i,j] == 1000 ) {  # On a classé toute les cases entre-elles, donc on a finit.
      t = 1
    }
    
    if ( B[i,j] != 1000 ) {
      
      Lx = append( Lx, i )   # On insére les coordonées de la case (i,j) dans la liste des rangs.
      Ly = append( Ly, j )
      B[i,j] = 1000          # Puis on remplace sa valeur dans B pour qu'elle ne soit plus sélectionnée à l'avenir.
    
    }
    
  }
    
  L = c(Lx, Ly)
  return(L)
  
}


####### 3) Fonction Backtraking :


backtraking = function(A) {
  
  # On récupère les coordonnées des cases classées par rang.
  L = rang(A)
  n = length(L)    # n/2 est le nombre de cases vides dans A.
  
  Lx = L[ 1:(n/2) ]
  Ly = L[ (n/2 +1):n ]
  
  cmp = 0   # Nombre de passage dans l'algorithme.
  
  # Début de l'algorithme principale :
  
  Acopy = A   # Acopy est la copie de A sur laquelle on va travailler.
  t = 1
  while ( t <= n/2 ) {
    
    cmp = cmp + 1
    i = Lx[[t]]
    j = Ly[[t]]
    
    k = Acopy[i,j] + 1
    
    z = 0
    while ( (k <= 9) && (z == 0) ) {     # On va essayer de placer un chiffre dans la case vide (i,j).
      
      if ( emplacementdisponible(i,j, k, Acopy) ) {   # On peut placer k dans la case (i,j), et on avance d'un cran dans l'algorithme.
        Acopy[i,j] = k
        t = t + 1
        z = 1
        
      } else {
        k = k + 1
      }
      
    }
    
    if (z == 0) {   # Aucun chiffre n'est disponible pour la case (i,j), donc on remonte d'un cran dans l'algorithme.
      t = t - 1
      Acopy[i,j] = 0
    }
  
  }
  
  return( list(Acopy, cmp) )   # Acopy est une solution possible au Sudoku.
  
}


############################################################################


# III) GENERATION DE LA GRILLE INCOMPLETE


####### 1) Fonction de suppression aléatoire d'une case :


suppressioncase = function(A) {
  
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


####### 2) Génération grille incomplète selon le niveau de difficulté.


grilleincomplete = function(l) {   # l = niveau de difficulté = 1,2,3.
  
  # l = !!!
  A = grille()
  
  t = 0
  while ( t <= l ) {   # Tant que l'on a pas atteint le niveau de difficulté.
    
    n = sample( c(2:5), size=1 )  # Nombre de cases aléatoires que l'on va enlever à chaque itération.
    
    for ( i in c(1:n) ) {
      A = suppressioncase (A) 
    }
    
    Z = backtraking(A)
    t = Z[[2]]
  }
  
  return( list(A, Z[[1]]) )   # A = Sudoku à remplir, Z[[1]] = une solution.
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


backtraking(A)


grilleincomplete(1000)


####################################

