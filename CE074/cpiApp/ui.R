library(shiny)
library(shinyjs)

# Instalar libv8-dev

shinyUI(
  fluidPage(
    useShinyjs(),
    tags$head(
      # tags$script(jscode),
      tags$style(HTML('#start1{background-color:#33cc33}')),
      tags$style(HTML('#stop1{background-color:#ff0000')),
      tags$style(HTML('#start2{background-color:#33cc33}')),
      tags$style(HTML('#stop2{background-color:#ff0000'))
    ),
    column(1),
    column(8,
      tags$div(
        hidden(
          tags$h1(id="thanks",
                  style = "margin: 20% 0% 0% 30%; width: 150px; heigth: 150px;",
                  "Obrigado!"),
          # textOutput("texto1"),
          # textOutput("texto2"),
          # textOutput("teste")
          uiOutput("texto")
          )),
      
      tags$div(style = "margin: 0% 0% 0% 50%; width: 150px; heigth: 150px;",
               fluidRow(
                 numericInput("id","",""),
                 actionButton("start1",label = "",icon = icon("play"),
                            width = "100%")))),
    column(3,
           hidden(
            actionButton("stop1",label = "",icon = icon("stop"), width = "25%",
                        style = "margin: 10% 0% 0% 10%; width: 150px; heigth: 150px;"),
            actionButton("stop2",label = "",icon = icon("stop"), width = "25%",
                        style = "margin: 10% 0% 0% 10%; width: 150px; heigth: 150px;"),
            actionButton("start2","",icon = icon("play"), width = "25%",
                        style = "margin: 10% 0% 0% 10%; width: 150px; heigth: 150px;"))
    )
  )
)
