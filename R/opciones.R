
opciones_UI<- function(id, label = "Opciones") {
  ns <- NS(id)
  
  tabPanel(
    "Opciones",
    value="main_tabset_opciones",
    sidebarLayout(
      sidebarPanel(
        width=3,
      ),
      mainPanel(
        fluidRow(
        ),
        fluidRow(
        ),
        fluidRow(
          column(width=6,
                textInput(
                  inputId=ns("prueba_textinput"),
                  label="Prueb"
                ) 
          ),
          column(width=6,
                textInput(
                  inputId=ns("prueba_textinput2"),
                  label="Prueb"
                ) 
          )
          )
        )
      )
    )
  
}

opciones_Server <- function(id, tabset_id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      
    }
  )
}