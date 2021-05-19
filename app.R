##Kiri Daust
## May 2021

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(ggiraph)
library(data.table)

#units <- fread("MerritUnits.csv")

##data for edatopic grid
grd1x <- seq(1.5,4.5,1)
grd1y <- seq(1.5,7.5,1)
temp <- as.data.table(expand.grid(y = c(1:8),x = c(1:5)))
ids <- 1:40

idDat <- expand.grid(SMR = 0:7, SNR = c("A","B","C","D","E"))
idDat <- as.data.table(idDat)
setorder(idDat,SNR,-SMR)
idDat[,ID := 1:40]
idDat[,edatopic := paste0(SNR,SMR)]

ui <- fluidPage(
  
  fluidRow(
    column(3,
           h1("Select Units"),
           fileInput("file1", "Upload csv file with unit names in the first column",
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
           ),
           uiOutput("bgcSelect"),
           uiOutput("ss_select"),
           textInput("newUnit",label = "Or add new unit")
    ),
    column(4,
           h1("Click on grid to enter data"),
           girafeOutput("edaplot"),
           actionBttn("clear","Clear Selection"),
           actionBttn("submit","Submit")
    ),
    column(5,
           h1("Entered Data"),
           panel(style = "overflow-y:scroll; max-height: 800px; position:relative; align: centre",
                 tableOutput("table"),
                 actionBttn("clearTab","Clear Table"),
                 downloadButton("downloadStn")
                  )
           
           )
  )
)

server <- function(input, output, session) {
  reUnits <- reactiveVal(NA)
  reBgcs <- reactiveVal(NA)
  
  observe({
    inFile <- input$file1
    if(!is.null(inFile)){
      dat <- fread(inFile$datapath)
      reUnits(unique(dat[[1]]))
      reBgcs(unique(gsub("/.*","",reUnits())))
      #browser()
    }
  })
  
  output$bgcSelect <- renderUI({
      bgcs <- reBgcs()
      selectInput("bgc",choices = bgcs,label = "Select BGC", multiple = F)
  })
  
  res <- reactiveValues(data = data.table(SS_NoSpace = character(),edatopic = character()))
  output$text <- renderText(input$edaplot_selected)
  
  output$ss_select <- renderUI({
    if(length(input$bgc) > 0){
      bgcs <- reBgcs()
      units <- reUnits()
      opts <- units[grep(input$bgc,units)]
      selectInput("unit",choices = opts,label = "Select unit(s)",selected = opts[1], multiple = T)
    }
  })
  
  observeEvent(c(input$clear, input$submit), {
    session$sendCustomMessage(type = 'edaplot_set', message = character(0))
  })
  
  observeEvent(input$submit, {
    if(input$newUnit == ""){
      tempUnit <- input$unit
    }else{
      tempUnit <- input$newUnit
    }
    if(length(tempUnit > 1)){
      temp <- as.data.table(expand.grid(SS_NoSpace = tempUnit,ID = as.integer(input$edaplot_selected)))
    }else{
      temp <- data.table(SS_NoSpace = tempUnit,ID = as.integer(input$edaplot_selected))
    }
    temp[idDat,edatopic := i.edatopic, on = "ID"]
    temp[,ID := NULL]
    res$data <- rbind(res$data,temp)
    updateTextInput(session, "newUnit", value = "")
    
    bgcs <- reBgcs()
    units <- reUnits()
    opts <- units[grep(input$bgc,units)]
    curr <- which(opts == input$unit)
    if(curr == length(opts)){
      nxt <- curr
    }else{
      nxt <- curr+1
    }
    updateSelectInput(session, "unit",selected = opts[nxt])
  })
  
  output$table <- renderTable({
    res$data
  })
  
  observeEvent(input$clearTab,{
    res$data <- data.table(SS_NoSpace = character(),edatopic = character())
  })
  
  output$downloadStn <- downloadHandler(
    filename = "EdatopicEntry.csv",
    content = function(file){
      fwrite(res$data, file)
    }
  )
  
  output$edaplot <- renderGirafe({
    gg2 <- ggplot(data = temp, aes(x = x, y = y))+
      geom_blank()+
      scale_y_discrete(limits = c("7","6","5","4","3","2","1","0"))+
      scale_x_discrete(limits = c("A","B","C","D","E"))+
      geom_tile_interactive(tooltip = idDat$edatopic, data_id = ids)+
      # geom_hline(aes(yintercept = grd1y), linetype = "dashed")+
      # geom_vline(aes(xintercept = grd1x), linetype = "dashed")+
      theme_bw(base_size = 10)+
      theme(panel.grid.major = element_blank())+
      labs(x = "SNR", y = "SMR")+
      coord_fixed()
    
    girafe(ggobj = gg2,
           options = list(opts_selection(type = "multiple")),
           width_svg = 4, height_svg = 7)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
