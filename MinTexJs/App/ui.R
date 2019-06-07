## ===============================================================================
## UI
## ===============================================================================
library(shiny)
library(shinyjs)

# Retira o ID (se houver ID) com base no Click
jscode <- "$(document).on('click', '.needed', function () {
                        Shiny.onInputChange('mouse',this.id);
                             });"

jscode2 <- "$(document).on('click', '.needed2', function () {
                        Shiny.onInputChange('mouse2',this.id);
                             });"
# Reseta o ID 
jscode3 <- "Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                Shiny.onInputChange(variableName, null);
            });"

# Interface do Usuario
shinyUI(
  fluidPage(
          list(
              tags$head(
                HTML('<link rel="icon" , href="icone.png" , type ="image/png" />'),
                div(
                  style="padding: 1px 0px; width: '100%'",
                  titlePanel(
                    title="",
                    windowTitle = "MinTexJs"
                  ))
              )
              ),
              theme="bootstrap.css",
              tags$head(tags$script(jscode)),
              tags$head(tags$script(jscode2)),
              tags$head(tags$script(jscode3)),
              useShinyjs(),
            hidden(
                actionButton("voltar1","",icon = icon("arrow-left"),width = "100%"),
                actionButton("voltar2","",icon = icon("arrow-left"),width = "100%"),
                actionButton("voltar3","",icon = icon("arrow-left"),width = "100%"),
                div(id="main",
                  class="mainplot",
                    htmlOutput("topicos")
                    )),
            div(class="centrado",
                hr(id="horizon1"),
            fluidRow(
              column(1,
                     actionButton("preprocessamento",icon("gears"),width = "100%"),
                     actionButton("email",icon("user-o"),width = "100%"),
                     hidden(
                     actionButton("modelagem_topicos",icon("list-alt"),width = "100%"),
                     actionButton("dist_cron_top",icon("line-chart"),width = "100%"),
                     actionButton("P_save","",icon("save"),width = "100%")
                     )
              ),
              column(3,
                     dateRangeInput("data","Amplitude das notícias",
                                    start = max(df$Publicado),
                                    end = max(df$Publicado),
                                    min = min(df$Publicado),
                                    max = max(df$Publicado),
                                    separator = "até"),
                     checkboxGroupInput("jornais",label = "Jornais",
                                     choices = c("Estadão" = "ESTADAO",
                                             "Folha de São Paulo" = "FolhaSP",
                                             "G1" = "OGLOBO"),
                                     selected = c("Estadão" = "ESTADAO",
                                             "Folha de São Paulo" = "FolhaSP",
                                             "G1" = "OGLOBO")),
                     hidden(
                       textInput("P_email_to","Email para receber:",width = "100%"),
                       textInput("P_email_from","Email para enviar:",width = "100%")
                     )),
              column(3,
                     numericInput("rmv_esparsidade","Remove esparsidade",value = 0.9,
                                  min = 0, max = 0.99,step = 0.01),
                     numericInput("n_categorias","Número de Tópicos",value = 2,
                                  min = 2,max = 9,step = 1),
                     hidden(
                       passwordInput("P_passwd_from","Senha do email que envia:",width = "100%"),
                       radioButtons("P_execute","Executar:",choices = c("Sim" = "sim",
                                                                        "Não" = "não"),
                                    selected = "não"
                       ))
                     ),
              column(5,
                     conditionalPanel("output.dtm",
                                      verbatimTextOutput("dtm"))
                     )
            ), # Fluid Row
            hr(id="horizon2"),
            p(id="by","By Andryas Waurzenczak")
            ) # Div ^| FluidRow
    ) # Fluid Page
  ) # Shiny UI