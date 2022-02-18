library(shiny)

shinyServer(function(input, output) {
  
  # On va ici créer la grille à l'aide de l'algorithme
  # donné dans le sujet
  
  nomco <- c('C1','C2','C3','C4','C5','C6','C7','C8','C9')
  nomli <- c('L1','L2','L3','L4','L5','L6','L7','L8','L9')
  matrix  <- matrix(11:19, ncol=9, nrow=9, byrow=FALSE, dimnames = list(nomli,nomco))
  
  # On remplit la première ligne
  ens <- c(1:9)
  for (c in 1:9){
    # Disjonction de cas car il persiste un problème pour le dernier coefficient
    if(c<9){
      # tirage aléatoire
      indi <- sample(ens,1)
      # on retire de l'ensemble le nombre tiré aléatoirement
      ens <- setdiff(ens, indi)
      matrix[1,c] <- indi
    }else{
      matrix[1,c] <- ens
      # ici ens a été réduit à un entier
    }
  }
  
  # On remplit la seconde ligne
  mat_temp <- matrix
  # On tire le nombre de décalage du bloc de trois aléatoirement
  ens_2 <- c(1,2)
  perm <- sample(ens_2,1)
  for (i in 1:9 ){
    matrix[2, (i+3*perm)%%9] <- mat_temp[1,i]
    matrix[2,9] <- mat_temp[1,9-3*perm]
  }
  
  # On remplit la troisième ligne
  mat_temp <- matrix
  for (i in 1:9 ){
    matrix[3, (i+3*perm)%%9] <- mat_temp[2,i]
    matrix[3,9] <- mat_temp[2,9-3*perm]
  }
  
  
  # On remplit la première colonne
  mat_temp <- matrix
  ens_3 <- c(2:3)
  perm <- sample(ens_3,1)
  for (i in 4:6){
    matrix[i,1] <- mat_temp[i-3,perm]
  }
  for (i in 7:9){
    matrix[i,1] <- mat_temp[i-6,setdiff(ens_3,perm)]
  }
  
  output$matrix <- renderTable({
    matrix
  },rownames = TRUE)
})
