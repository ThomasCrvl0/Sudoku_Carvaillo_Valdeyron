
grilleincomplete = function(m) {   # m = niveau de difficulté = 1,2,3.
  
  a1 = 500     # Seuils de difficultés
  a2 = 1000
  b1 = 3000
  b2 = 10000
  c1 = 15000
  c2 = 50000
  
  if (m == 1) {
    l = a1
    x = a2
    n = 42
  }
  if (m == 2) {
    l = b1
    x = b2
    n = 47
  }
  if (m == 3) {
    l = c1
    x = c2
    n = 52
  }
  
  
  z = 0
  while (z == 0) {
    
    A = grille()
    
    for ( i in c(1:n) ) {      # On enlève certaines cases par défault au début pour être plus rapide.
      A = suppressioncase (A) 
    }
    
    t = 0
    while ( t < l ) {   # Tant que l'on a pas atteint le niveau de difficulté.
      
      n = sample( c(1:2), size=1 )  # Nombre de cases aléatoires que l'on va enlever à chaque itération.
      
      for ( i in c(1:n) ) {
        A = suppressioncase (A) 
      }
      
      M = backtraking(A, x)
      t = M[[2]]
      
    }
    
    if ( t <= x ) {
      z = 1
    }
    
  }
  
  return( list(A, M[[1]], t) )   # A = Sudoku à remplir, M[[1]] = une solution, t = indice de difficulté.
}

