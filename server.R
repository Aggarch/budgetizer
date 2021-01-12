

shinyServer(function(input, output){
  
  
  
  options(shiny.maxRequestSize=100*1024^2)
  
  
  
  tbl_descapli <- reactive({
    inFile <- input$cargue_archivos
    
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath) %>% 
      wrangler()
    
    
    return(df)
    
  })
  
  
  
  # tbl_descapli_dt <- reactive({
  #   inFile <- input$cargue_archivos
  #   
  #   if (is.null(inFile))
  #     return(NULL)
  #   
  #   dataf <- read.csv(inFile$datapath) %>% 
  #     wrangler() %>% 
  #   
  #   
  #   return(dataf)
  #   
  # })
  
  
  output$tbl_descapli<- DT::renderDataTable({
    
    
    DT::datatable(tbl_descapli(), style = 'bootstrap',rownames = F,
                  options = list(lengthMenu = c(5,15, 30, 50),
                                 dom = 'ft',
                                 hover=TRUE, compact=TRUE)) %>% 
      formatStyle(3, target= 'row',color = 'black', backgroundColor = tableColour, 
                  lineHeight='70%', padding = '3px 3px', fontSize = '80%')
    
    
  })
  
  
  # observeEvent(eventExpr = input$subir_descapli,{
  #   
  #   showNotification("Iniciando carga a BigQuery.", type = "message")
  #   
  #   data <- tbl_descapli()
  #   
  #   data <- data %>%  anti_join(db_fopep_desc_apli %>% collect(), by = c("periodo","documento","consecutivo"))
  #   
  #   transaccion_subida_big_query(datos = data,
  #                                proyecto = "finso-analytics",
  #                                conjunto_datos = "finso_warehouse",
  #                                tabla = "fopep_desc_apli")
  #   
  #   
  #   showNotification("Cargado, revisar BigQuery para confirmar", type = "warning")
  #   
  # })
  # 
  
})






