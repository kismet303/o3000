####Path to data and output and progam
study_path <- "root/clinical_studies/RO5541267/CDT30212/BP40657/data_analysis/BASE/prod/" 
output_path <- paste0(study_path,"output/", output_file)
program_path <- paste0(study_path,"program/", program_file)
program_ext <- file_ext(program_path)
data_path <- paste0(study_path,"outdata_vad")  
log_path <- paste0(study_path,"log/", log_file)
