shinyServer(function(input, output, session) {
  aux <- reactiveValues()


  # Cálculo Diferencial e Integral ------------------------------------------
  observeEvent(input$cehelp, {
    sendSweetAlert(
      session = session,
      title = "Como escrever a função?",
      text = paste0("A função deve ser escrita em linguagem R podendo utilizar ",
                    "todos os recursos disponiveis como log, min, max etc...",
                    "No entanto somente o 'x' será interpretado como variável e os",
                    " demais argumentos como parâmetros. \n\nAbaixo alguns exemplos de",
                    " como declarar as funções:", "\n\n1/x\n2*(x*log(x/theta)-x+theta)+mu",
                    "\n2 * (1 - cos(x - theta))\n(x-1)^3\n..."
      ),
      type = "info"
    )
  })
  
  output$cesliders <- renderUI({
    input$cegraph
    
    sliders <- ce_cria_sliders(input$cefx)
    aux$params <- sliders$params
    sliders$sliders
  })
  
  
  
    
})
