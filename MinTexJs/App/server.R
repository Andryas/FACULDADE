library(shiny)
library(shinyjs)
options(warn = -1)

shinyServer(function(input,output,session){
  # Variável auxiliar
  aux <- reactiveValues()
  aux$panel <- 0

  # Eventos observáveis
  #-----------------------------------------------------------------------------
  # Tópicos e troca de ui
  observeEvent(input$modelagem_topicos,{
    aux$panel <- 1
    hide("preprocessamento"); hide("modelagem_topicos"); hide("data"); hide("jornais"); hide("rmv_esparsidade")
    hide("n_categorias"); hide("dtm") ; hide("horizon1"); hide("horizon2"); hide("by")
    hide("dist_cron_top"); hide("email")
    show("voltar1")
    show("main")
  })

  observeEvent(input$preprocessamento,{
    show("modelagem_topicos"); show("dist_cron_top")
  })

  observeEvent(input$dist_cron_top,{
    aux$panel <- 4
    hide("preprocessamento"); hide("modelagem_topicos"); hide("data"); hide("jornais")
    hide("rmv_esparsidade"); hide("n_categorias"); hide("dtm") ; hide("horizon1"); hide("horizon2")
    hide("by")
    show("voltar1")
    show("main")
  })

  observeEvent(input$email,{
    hide("preprocessamento"); hide("modelagem_topicos"); hide("data"); hide("jornais"); hide("rmv_esparsidade")
    hide("n_categorias"); hide("dtm"); hide("dist_cron_top"); hide("email")
    show("voltar1")
    show("P_email_to"); show("P_email_from"); show("P_passwd_from"); show("P_execute")
    show("P_save")
  })

  # Titulos
  observeEvent(input$mouse,{
    aux$panel <- 2
    hide("voltar1")
    show("voltar2")
  })

  # Materia
  observeEvent(input$mouse2,{
    aux$panel <- 3
    hide("voltar2")
    show("voltar3")
  })

  observeEvent(input$voltar1,{
    aux$panel <- 0
    show("preprocessamento"); show("data"); show("jornais"); show("rmv_esparsidade")
    show("n_categorias"); show("dtm"); show("horizon1"); show("horizon2"); show("by")
    show("email")
    hide("voltar1")
    hide("main"); hide("dist_cron_top"); hide("modelagem_topicos")
    hide("P_email_to"); hide("P_email_from"); hide("P_passwd_from"); hide("P_execute")
    hide("P_save")
    session$sendCustomMessage(type = "resetValue",message="preprocessamento")
    session$sendCustomMessage(type = "resetValue",message="dist_cron_top")
  })

  observeEvent(input$voltar2,{
    hide("voltar2")
    show("voltar1")
    session$sendCustomMessage(type = "resetValue",message="mouse")
    aux$panel <- 1
  })

  observeEvent(input$voltar3,{
    hide("voltar3")
    show("voltar2")
    session$sendCustomMessage(type = "resetValue",message="mouse2")
    aux$panel <- 2
  })

  observeEvent(input$P_save,{
    info$Campo[1] <- input$P_email_to
    info$Campo[2] <- input$P_email_from
    info$Campo[3] <- input$P_passwd_from
    info$Campo[4] <- input$P_execute
    write.table(info,file = "../Web-Scraping/fun/info.txt",row.names = F,
                  sep = ":")
  })

  # Eventos reativos
  #-----------------------------------------------------------------------------
  # preprocessamentoamento.
  # Cria Matrix de Documentos e termos
  dtm <- eventReactive(input$preprocessamento,{
    Preprocessamento(df,input$data[1],input$data[2],input$jornais,input$rmv_esparsidade)
  })

  # Modelagem de tópicos (LDA)
  r <- eventReactive(input$modelagem_topicos,{
    r <- ModelTopic(dtm()[[1]],dtm()[[2]],input$n_categorias)
    i <- 1
    n <- length(levels(r[[1]]$topic))
    for(i in i:n){
      x <- subset(r[[1]],r[[1]]$topic == levels(r[[1]]$topic)[i])
      png(filename = paste0("www/topic",i,".png"),width = 300,height = 300)
      wordcloud(x$term,exp(x$lprob),random.order = F,rot.per = 0,
                colors = brewer.pal(8,"Paired"), scale = c(3.5,.5))
      dev.off()
    }
    i <- 1
    j <- 1
    topwords <- character(n)
    for(i in i:n){
      if(j == 3){
        j <- 1
      }
      topwords[i] <- paste0("<div class='column",j,"'>",
                            "<img src='topic",i,".png' id=",i," class= 'needed' ",
                            "width=300 height=300></div>")
      j <- j + 1
    }
    r[[5]] <- topwords
    return(r)
  })

  # Proporção dos tópicos
  topics_propotion <- eventReactive(input$dist_cron_top,{
    p <- ModelTopic(dtm()[[1]],dtm()[[2]],input$n_categorias)
    names(p[[4]]) <- c("Dt","tópicos","value")
    return(p[[4]])
  })

  # Topicos das materias pega ID
  click <- reactive({as.integer(input$mouse)})

  # Titulos das materias pega ID
  click2 <- reactive({as.integer(input$mouse2)})



  # Outputs
  #-----------------------------------------------------------------------------
  # Quantidades de documentos sendo trabalhados (DTM)
  output$dtm <- renderPrint({dtm()[[1]]})
  outputOptions(output, "dtm", suspendWhenHidden = FALSE)

  # topics_propotion
  output$topics_prop <- renderPlot({
    ggplot(topics_propotion(),aes(x=Dt,y=value,col= tópicos)) +
      geom_smooth(method = "loess") + labs(x="",y="")
  })

  # Panel
  output$topicos <- renderUI({
   if(aux$panel == 0){
   }else if(aux$panel == 1){
        # HTML(r()[[1]])
        HTML(r()[[5]])
   }else if(aux$panel == 2){
      HTML(paste0("<ul>",
        paste(r()[[2]][[click()]],collapse = ""),
        "</ul>"))
   }else if(aux$panel == 3){
     x <- r()[[3]][[click()]][click2()]
     t <- as.character(x)
     t <- strsplit(t,"\n")
     i <- 1
     for(i in i:length(t[[1]])){
       t[[1]][i] <- paste0("<p>",t[[1]][i],"</p>")
     }
     HTML(paste0("<div class=pro-p>",
       paste(t[[1]],collapse = ""),
       "</div>")
       )
   }else if(aux$panel==4){
        plotOutput("topics_prop")
    }
  })

  }) # Shiny Server
