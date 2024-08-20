
seccion2_UI<- function(id, label = "Seccion 2") {
  ns <- NS(id)
  
  tabPanel(
    "Seccion 2",
    value="main_tabset_seccion2",
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

seccion2_Server <- function(id, tabset_id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      
    }
  )
}