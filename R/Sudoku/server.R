library(shiny)

shinyServer(function(input, output, session) {
  
  
      output$aff1 = renderPlot({ 
      
        Z = grilleincomplete(input$diff)
        A = Z[[1]]
        B = Z[[2]]
        cmp = Z[[3]]
        
        # Affichage Sudoku Incomplet
        plot(0:10, 0:10, type="n", xlab=NA, ylab=NA, axes = FALSE)
        
        for (i in 1:9) {
          for (j in 1:9) {
            if(A[i,j] != 0){
              text(j, 10-i,A[i,j], offset=0)
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
        
        # Affichage indice Backtracking cmp
        output$Text  = renderText({
          paste("Indice de Backtracking/Difficulté =", as.character(cmp))
        })
      
        # Affichage Solution
        output$aff2 = renderPlot({
          
          plot(0:10, 0:10, type="n", xlab=NA, ylab=NA, axes = FALSE)
          
          for (i in 1:9) {
            for (j in 1:9) {
              
              text(j, 10-i, B[i,j], offset=0)
              
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
          
        })
        
        # Légende
        output$legende = renderText({
          paste("  - L'indicateur de difficulté correspond au nombre de tours effectués", "\n", "dans l'algorithme de Backtracking.", "\n", "Voici la liste des niveaux proposés en fonction de cet indice :", "\n",
                "\n", "Niveau 1 (Facile) = 500 <= indice <= 1000",
                "\n", "Niveau 2 (Moyen) = 3000 <= indice <= 10000",
                "\n", "Niveau 3 (Difficile) = 15000 <= indice <= 50000", "\n", "\n",
                "\n", "- Attention : plus le niveau de difficulté est élevé, et plus le", "\n", "temps de génération peut être important.")
        })
      
    })
})
