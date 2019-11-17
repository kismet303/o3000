 
library(shiny) 
library(shinydashboard) 
library(DT)  
library(shinyAce)
library(DBI)
library(readr)
library(haven)
library(shinyBS) ##tag button
library("tools")  ##for file extension 
library(glue)
#source("get_edit.R")

main_path<<-"/opt/bee_tools/shiny/3.5.3/users/remusatp/lopo3000/"
functionPath<<-paste0(main_path,"functions/")
###Input  ###
#source("connect_outp.R")  
study <<- "BP40657"
server_="BEE" 
s_path<<-paste0(main_path,"Studies/",study,"/",study)
 
 
source(paste0(functionPath,"execute_R_prog.R"))


server <- function(input, output,session) {
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query[['study']])) {
      study<<- query[['study']]
    }
    
    if (!is.null(query[['server']])) {
      server_<- query[['server']]
    }
    
    
    if (!is.null(query[['outputid']])) { 
      row_ID<-query[['outputid']]
    }
    
 
   # row_ID<-4
    
    myValue<-getOutput(study,s_path,row_ID)
    output$myTitle <- renderText({  myValue$Title })
    
    study_path <<- paste0(main_path,"Studies/",study,"/") 
    adsl_path<-paste0(study_path,"/data/ADAM/adsl.sas7bdat")
    
    ####Path to data and output and progam
    if (server_ == "Entimice") {
      output_file<<-paste0(myValue[9],".pdf") 
      program_file<<-paste0(myValue[5],".sas") 
      log_file<<-paste0(myValue[9],".log") 
      source("functions/sas_prog.R")
    }
    
    if (server_ == "BEE") {
      output_file<<-paste0(myValue[9],".pdf") 
      program_file<<-paste0(myValue[5],".R") 
      log_file<<-paste0(myValue[9],".txt") 
      FilterL<-myValue$Filters
    }
    
    
    source("functions/R_prog.R")
    
    #####Output (PDF)<-
    file.copy(paste0(study_path,"output/",output_file), paste0("www/",output_file), overwrite = TRUE)
    date_outp<-file.info( output_path)   
    output$myDateOutp <- renderText({ paste("Output viewed the:", as.character(date_outp$atime)) }) 
    
    #####Program (R)
    #output$myProgram <-  renderText({  read_file(program_path) })   
    #txt <- paste("<pre><code class='language-r'>",  read_file(program_path), "</code></pre>")
    #output$code_program <- renderUI({ prismCodeBlock(txt)})
    
    output$myProgram <-  renderText({  read_file(program_path) })   
    txt <- paste("",  read_file(program_path), "")
    output$code_program <- renderText({ txt })
    
    txt <- read_file(program_path) 
    output$myProgram <-  renderText({ txt })
    
    #####Log (R)
    txtLog <- get_log(paste0(study_path,"log/", log_file))
   # output$log <- renderUI({ prismCodeBlock( txtLog )})
    output$log <- renderText({   txtLog  })
    
    
    print(program_path)
    ##display a pdf file
    
    output$myFile <- renderUI({
      if (file.exists(paste0("www/",output_file))) {
        tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                    src=  output_file   )}
      
      else {tags$p(paste0(output_file," has not been created"))}
      
    })
    
    output$myProgram_name <- renderText({  program_file })
    output$myProgram_log <- renderText({  log_file })    
    output$outfilename <-  renderText({ output_file }) 
    
    
    
    ##############################################Data (SAS) ##################################################################
   
    myDatatmp   <- get_myData(myValue[3],study_path)  
    output$mydata_name <- renderText({  toupper(myDatatmp[[1]])   }) 
    
    output$mydata = DT::renderDataTable (myDatatmp[[2]]  ,  escape = FALSE,
                                         extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                                         #filter = 'top',
                                          
                                         colnames = glue(
                                           "<span title={colnames(myDatatmp[[2]])} data-toggle='tooltip'>{unique(as.data.frame(var_labels(myDatatmp[[2]]))[,1])}</span>"
                                         ),
                                     
                                         callback = JS("$('#mytable').tooltip({selector:'[data-toggle=\"tooltip\"]'})"),
                                        
                                         options = list(deferRender = F, 
                                                        dom = 't',
                                                        columnDefs = list(list(className = 'dt-center',targets = 5)),
                                                        scrollY = 600, 
                                                        scroller = TRUE,
                                                        scrollX = T,
                                                        autoWidth = TRUE,
                                                        pageLength = 10)                           
    )
    
    
    adsl<-get_adsl(adsl_path)
 
    output$myadsl = DT::renderDataTable (adsl[[2]],    escape = FALSE,
                                         extensions = 'Scroller', 
                                        # filter = 'top',
                                         colnames = glue(
                                           "<span title={colnames(adsl[[2]])} data-toggle='tooltip'>{ var_labels(adsl[[2]])}</span>"
                                         ),
                                         callback = JS("$('#mytable').tooltip({selector:'[data-toggle=\"tooltip\"]'})"),
                                         options = list(deferRender = F, 
                                                        dom = 't',
                                                        columnDefs = list(list(className = 'dt-center',targets = 5)),
                                                        scrollY = 600, 
                                                        scroller = TRUE,
                                                        scrollX = T,
                                                        autoWidth = TRUE,
                                                        pageLength = 10)                           
    )
    
    ##############################################################################################################################
    output$valcom <- renderText({ input$caption })  
    
    
    
    ####r EDITOR AND RESULT####
    updateAceEditor(session, "ace", txt ,
                    mode = "r", theme = "ambiance")
    
    
    output$outpuuut <- renderPrint({
      input$eval
      return(isolate(eval(parse(text=input$ace))))
    })
    
    
    #####save code#####
    observeEvent(input$Save_R, {
      writeLines( input$ace, program_path)
      
    })
    ##########################
    
 
    observeEvent(input$Run_R, {
       execute_R_prog(study,s_path,row_ID)
    })
    
    
    observeEvent(input$Run_SAS, {
      execute_SAS_prog()
    })
    
    observeEvent(input$clear, {
      updateAceEditor(session, "ace", value = "\r")
    })
    
  })
  
}


header <-dashboardHeader(title = textOutput("outfilename"))
 

sidebar <- 
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Output", icon = icon("th"), tabName = "output",badgeLabel = "new", badgeColor = "green"),
      menuItem("Data", icon = icon("th"), tabName = "data"),
      menuItem("Program", icon = icon("th"), tabName = "program"),
      menuItem("Comments", icon = icon("th"), tabName = "comments") 
      
    )
  )



body <- dashboardBody(
  
  fluidRow(
    
   tags$head(tags$style(HTML(' 
           ".tooltip-inner {max-width: 500px; /* the minimum width */}"  '))),
 
    tabItems(
      tabItem(tabName = "output",
              column(width = 12,
                     box(
                       title =  textOutput("myTitle") , width = NULL, solidHeader = TRUE, status = "primary",
                       htmlOutput("myFile")
                     ) 
              )
      ),
      
      
      tabItem(tabName = "data",
              #h2("Table of Adverese Events"),#
              
              tabsetPanel(
                tabPanel(textOutput("mydata_name"),  DT::dataTableOutput("mydata"    , width = 1400)),
                tabPanel("ADSL",  DT::dataTableOutput("myadsl"    , width = 1400)) 
              )
              
      ),
      
      
      
      tabItem(tabName = "comments",
              h2("Comments")  ,
              column(width = 4,
                     box(
                       title = , width = NULL, solidHeader = TRUE, status = "primary",
                       textOutput("myDateOutp" )  
                     ) ,
                     box(
                       title = , width = NULL, solidHeader = TRUE, status = "primary",
                       textInput("caption", "Caption", "Data Summary"),
                       verbatimTextOutput("valcom") 
                     ) )
      ),   
      
      
      tabItem(tabName = "program",
              #h2("Table of Adverese Events"),#
              mainPanel(
                tabsetPanel(
                  tabPanel("Program", 
                           div(style="display:inline-block;",        
                               h2(textOutput("myProgram_name")),
                               actionButton("Run_R", "Run"), 
                               actionButton("Save_R", "Save"),
                               actionButton("eval","Evaluate code"),
                               bsTooltip("eval", "To evalute the code below pleae add this: source(\"setup.R\") at the top and remove it before to save",
                                         "top", options = list(container = "body"))
                           ) ,
                           p(""),
                           fluidRow(
                             column(6,
                                    aceEditor(
                                      "ace",  value = " Enter your text here "  ,
                                      mode = "r",
                                      height = "900px", 
                                      fontSize = 10,
                                      autoScrollEditorIntoView = TRUE,
                                      minLines = 50,
                                      maxLines = 50 
                                    )) ,
                              column(6,
                                    verbatimTextOutput("outpuuut", placeholder = TRUE),
                                    tags$head(tags$style("#outpuuut{color:black; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 600px; background: ghostwhite;}"))  )
                             ) ),
 
                  tabPanel("Log",    
                           fluidRow(
                             column(width = 12, h3(textOutput("myProgram_log") ), 
                                    htmlOutput("log") 
                                    
                             ))  )  )   ### end of tabset panel (Log + prgram)
              )      ) )))      





# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  header,
  sidebar,
  body,
  title = paste0(study," Output")
)

# Preview the UI in the console
shinyApp(ui = ui, server = server)