

dashboardPage(
  dashboardHeader(title = "Budgetizer", titleWidth = 130),
  dashboardSidebar(width = 130,
                   
                   
                 
                   div(
                     
                     
                     br(),
                     
             
                     tags$img(src='logo_rohos.png', height=50, width=125, 
                              style = "padding-left:5px")
                     
                     
                   )
                   
  ),
  
  dashboardBody(
    
    shinyDashboardThemes(
      theme = "blue_gradient"),
    
    fluidPage(
      br(),
      box(title = "TakeOff File",
          width = 3,
          solidHeader = F, 
          status = "primary",
          helpText(""),
          fileInput(
            inputId = "cargue_archivos",
            label = "browser",
            buttonLabel = "Search",
            placeholder = "pick a file..."
          )
          # hr(),
          # helpText("Cargar a BigQuery"),
          # actionButton(inputId = "subir_descapli", label = "Cargar", 
          #              icon = icon("upload"))
      ),
      #),
      #fluidRow(
      box(title = "Estimated Budget",
          width = 9,
          solidHeader = F, 
          status = "primary",
          DT::dataTableOutput("tbl_descapli") %>%  withSpinner(type = 7L,
                                                                  color = "#18BC9C")
      )
    )
  )
)




