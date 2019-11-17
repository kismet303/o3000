####Path to data and output and progam

output_path <- paste0(study_path,"output/", output_file)
program_path <- paste0(study_path,"program/", program_file)
program_ext <- file_ext(program_path)
data_path <- paste0(study_path,"data")   



#########Check if Domain data exists########### 
get_myData <- function(domain , study_path) { 
  
  if (domain == "AE") { dataR <- "adae" }
  else if (domain == "DM") { dataR <- "adsl" }
  else if (domain %in%  c( "EX",  "Exposure")) { dataR <- "adex" }
  else if (domain %in%  c( "DS",  "Disposition")) { dataR <- "adsl" }
  else if (domain %in%  c("CM","Con Med" )) { dataR <- "adcm" }
  else if (domain == "EG") { dataR <- "adeg" }
  else if (domain == "VS") { dataR <- "advs" }
  else if (domain == "LB") { dataR <- "adlb" }
  else { dataR <- "unknown" }
  
  
  mydata_path<-paste0(study_path,"data/ADAM/",dataR,".sas7bdat")
  
  if (!file.exists(mydata_path)) {
    myData<- paste0(dataR," does not exisit" )
  }
  else { 
    myData<-read_sas(mydata_path)
  }
  
  return(list( dataR, myData))
  
}
 



#########Check if ADSL exists###########

get_adsl <- function(adsl_path) {
  dataR<-"adsl"
  if (!file.exists(adsl_path)) {
     adslMessage<-"ADSL does not exisit" 
  }
  else { 
     adsl<-read_sas(adsl_path)
  }
  
  return(list( dataR, adsl))
}



#######Get Comment #######
getComment <- function(item) {
  con <- dbConnect(RSQLite::SQLite(), paste0("../Studies/",study,"/comment/CommentDB")) 
  request<-paste0("SELECT description FROM CommentDB where ITEM = '",item,"'" )
  res <- unname(dbSendQuery(con,request))
  descr <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
  return(descr)
}
##################################







#########Check if Log exists###########
get_log <- function(log_path) {
  
  if (!file.exists(log_path)) {
    txtLog  <-   paste("<pre><code class='language-r'>",   "Log file does not exist"  , "</code></pre>")          
  }
  
  else {
    txtLog  <- paste("<pre><code class='language-r'>",  read_file(log_path), "</code></pre>")
    
  }
  
  return(txtLog)
  
}



