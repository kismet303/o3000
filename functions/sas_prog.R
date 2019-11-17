####Path to data and output and progam (Entimice)
study_path <- "root/clinical_studies/RO5541267/CDT30212/BP40657/data_analysis/BASE/prod/" 
output_path <- paste0(study_path,"output/", output_file)
program_path <- paste0(study_path,"program/", program_file)
program_ext <- file_ext(program_path)
data_path <- paste0(study_path,"outdata_vad")  
log_path <- paste0(study_path,"log/", log_file)

 
   
withProgress(    
  message = 'Import data from Entimice',
  detail = 'This may take a while...',  
  value = 0, {
    for (i in 1:2) {
      incProgress(1/2)
      Sys.sleep(0.8)
    }
    
    #####Entimice conenction#######
    SAICE::initialize_connection(entimice_env = "PROD")
    
    adsl  <- SAICE::read_entimice(file.path(data_path,'adsl.sas7bdat'))
    output_list = SAICE::get_entimice(output_path)
    program_list = SAICE::get_entimice(program_path)
    log_list = SAICE::get_entimice(log_path)
    
    SAICE::close_connection()
    #####End of Entimice conenction#######
    
    
  })

#####Output (PDF)
file.copy(output_list, paste0("/opt/bee_tools/shiny/3.5.3/users/remusatp/ProjectTest/www/",output_file))
date_outp<-file.info( output_list)   
output$myDateOutp <- renderText({ paste("Output viewed the:", as.character(date_outp$atime)) }) 

#####Program (SAS)
output$myProgram <-  renderText({  read_file(program_list) })   
txt <- paste("<pre><code class='language-sas'>",  read_file(program_list), "</code></pre>")
output$code_program <- renderUI({ prismCodeBlock(txt)})

######Log (SAS)
txt_log <-  gsub( "program_sas_log", read_file(log_list) ,  read_file("logtest/logtest.html") )    
output$log <- renderUI({ txt_log   })

   
 