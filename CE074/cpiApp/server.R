# library(rdrop2)

# Caso hospede o shiny em shinyappsio e use o Dropbox para armazenar os dados.
# A seguir os comandos necessários.


# Dropbox ----------------------------------------------------------------------
# Comandos com duas hastags significa que você precisa executar somente a primeira
# vez, para liberar seu acesso ao seu dropbox usando o pacote rdrop2.
## token <- drop_auth()
## saveRDS(token, "droptoken.rds")

# token <- readRDS("droptoken.rds")
# drop_acc(dtoken = token)

# ql <- drop_read_csv("ql.csv", colClasses = c("character","character","character",
                                             # "numeric","integer","character"))

# obs: Na linha 107 tem mais um comando!!

# -------------------------------------------------------------------------------

ql <- read.csv("ql.csv", colClasses = c("character","character","character",
                                       "numeric","integer","character"))

tits <- c("O que é o Big Data?","O que é STF?","Entenda o que é a Lava Jato.",
          "Computação em nuvem.")

# Id's permitidos
ids <- ql$id[!duplicated(ql$id)]

shinyServer(function(input, output, session) {
  aux <- reactiveValues()
  aux$txt <- 1
  
  observe({
    showModal(modalDialog(
      title= "",
      tags$div(
               HTML(
                 "<h1 style = 'text-align: center;'>Controle de Processos Industriais</h1>",
                 "<p  style = 'text-align: center;'>Trabalho 1 - Experimento Fatorial 2<sup>4</sup></p>",
                 "<br>",
                 "<p style='text-align: left;'>Alunos: Andryas Waurzenczak, Aline Benatti e Elias Santiago Diniz</p>"
                 # "<p style='text-align: left; margin-left: 50px'>      Aline Benatti</p>",
                 # "<p style='text-align: left; margin-left: 50px'>      Elias Santiago Diniz</p>",
                 # "<hr>",
                 # "<b>Explicação</b>"
                 )),
      
      p("Resumo: O objetivo desse experimento é medir a velocidade de leitura diferenciando alguns elementos ",
        " para isso precisamos que os participantes, leiam de forma normal."), 
      p("Após cada 'stop', o tempo será parado, recomeçando no próximo 'start'."), 
      p("É importante descansar 2 ou 3 minutos entre cada 'stop'."),
      p("Obrigado por participar!"),
      easyClose = FALSE,
      footer = modalButton(icon("close"))
    ))
  })

  # Start && stop 1
  observeEvent(input$start1,{
    if(input$id %in% ids & all(is.na(ql[ql$id == input$id,"tempo"]))){
      hide("start1"); hide("id"); show("stop1"); show("texto")
      # show("texto1"); show("texto2")
      aux$start <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    }else{
      showModal(modalDialog(
        title = "Este id já realizou o experimento ou está incorreto.",
        "Caso não tenha realizado o experimento ou o id que possuí não esteja correto, favor entrar em contato com a pessoal que lhe enviou o id.",
        footer = "",
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$stop1,{
    hide("stop1"); hide("texto"); show("start2")
    aux$end <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    aux$txt <- aux$txt + 1
    showModal(modalDialog(
      title = "",
      "Antes de recomeçar o próximo texto, descanse 2 ou 3 minutos.",
      footer = "",
      easyClose = TRUE
    ))
  })
  
  # Start 2
  observeEvent(input$start2,{
    hide("start2"); show("stop2"); show("texto")
    # show("texto1"); show("texto2")
    aux$start <- c(aux$start,format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  })
  
  observeEvent(input$stop2,{
    if(as.integer(aux$txt) == 4){
      hide("stop2"); hide("texto"); show("thanks")
      aux$end <- c(aux$end,format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      
      time <- difftime(as.POSIXct(aux$end, format = "%Y-%m-%d %H:%M:%S"),
                       as.POSIXct(aux$start, format = "%Y-%m-%d %H:%M:%S"))
      
      ql[ql$id == input$id,"tempo"] <- time
      write.csv(ql,"ql.csv",row.names = FALSE)
      # Dropbox para salvar o arquivo.
      # drop_upload("ql.csv",mode = "overwrite")
    }else{
      hide("stop2"); hide("texto"); show("start2")
      aux$end <- c(aux$end,format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      aux$txt <- aux$txt + 1
      showModal(modalDialog(
        title = "",
        "Antes de começar o próximo texto, descanse 2 ou 3 minutos.",
        footer = "",
        easyClose = TRUE
      ))
    }
  })
  
  estilo <- reactive({unlist(strsplit(ql[ql$id == input$id & ql$texto == paste0("text",aux$txt),
                                         "trat"],";"))})
  

  
  
  
  output$texto <- renderUI(
      div(style = paste0("font-family: ",estilo()[1],"; ",
                         "font-size: ",estilo()[2],"; ",
                         "line-height: ",estilo()[3],";"),
          h1(tits[aux$txt]),
          br(),
          br(),
          HTML(htmliza(length_line(as.integer(estilo()[4]),aux$txt))))
  )
  
})
