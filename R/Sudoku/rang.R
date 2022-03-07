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
