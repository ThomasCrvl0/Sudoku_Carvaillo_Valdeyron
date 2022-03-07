
backtraking = function(A, x) {  # x = nombre de tours maximal
  
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
    if (cmp > x) {     # Si cmp est trop grand (>x), alors on arrête le programme.
      break
    }
    
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

