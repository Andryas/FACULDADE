if (length(setdiff("pacman", rownames(installed.packages()))) > 0) {
  install.packages(setdiff("pacman", rownames(installed.packages())))  
}

pacman::p_load(shiny, stringr, ggplot2, shinydashboardPlus, shinyWidgets, shinyjs)

dashboardPagePlus(
  dashboardHeaderPlus(
    enable_rightsidebar = FALSE,
    rightSidebarIcon = "keyboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "home", icon = icon("home")),
      menuItem("Cálculo Diferencial e Integral", tabName = "cdi", icon = icon("square-root-alt"), selected = TRUE),
      menuItem("Métodos Númericos", tabName = "mn", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    useShinyalert(),
    useSweetAlert(),
    tabItems(
      tabItem(tabName = "home", h2("boas vindas")
      ),
      tabItem(tabName = "cdi", 
              fluidRow(
                box(title = "", solidHeader = TRUE, width = 12, status = "primary",
                    fluidRow(align = "center", textInput("cefx", NULL, "1/x", width = "60%")),
                    
                    actionBttn(
                      inputId = "cehelp",
                      label = NULL,
                      style = "material-circle", 
                      color = "primary",
                      size = "sm",
                      icon = icon("exclamation-circle")
                    ),
                    br(),
                    br(),
                    actionBttn(
                      inputId = "cegraph",
                      label = NULL,
                      style = "material-circle", 
                      color = "success",
                      size = "sm",
                      icon = icon("chart-line")
                    ),
                    br(), 
                    br(),
                    dropdownButton(
                      
                      circle = TRUE, status = "danger",
                      size = "sm",
                      icon = icon("gear"), width = "300px",
                      tooltip = tooltipOptions(title = "Click to see inputs !")
                    )
                )
              )
      ),
      tabItem(tabName = "mn", h2("Widgets tab content 2")
      )
    )
  )
)