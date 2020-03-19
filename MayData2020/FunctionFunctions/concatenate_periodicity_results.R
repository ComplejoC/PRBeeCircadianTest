#The purpose of this function is to concatenate the results of the periodicity_analysis function when
#there are multiple individuals
concatenate_periodicity_results <- function(){
  library(tidyverse)
  library(rstudioapi)
  #Select Directory of the "analysis" folder:
  directory <- selectDirectory()
  setwd(directory)
  files <- list.files()
  #Choose only the folders where the data is located
  folders_of_interest <- files[!str_detect(files, "\\.")]
  
  #Create paths to the files of interest:
  files_of_interest <- paste0(directory,"/",folders_of_interest,"/cosine_results.csv")
  #files_of_interest <- paste0(directory,"/",folders_of_interest,"cosine_results.csv")
  df <- map(files_of_interest, read_csv)
  names(df) <- folders_of_interest
  df_window <- map(df, ~ mutate(., window = 1:nrow(.)))
  df_bound <- bind_rows(df_window, .id = "Identifyer")
  
  #Spread sheet for the lombout period
  iterator <- names(df_bound)[!str_detect(names(df_bound), "Identifyer|window")]
  
  for (i in iterator) {
    df_bound %>%
      dplyr::select(Identifyer, i,window) %>% 
      pivot_wider(names_from = Identifyer,values_from = i) %>%
      write_csv(paste0(i,".csv"))
  }

    
  
  write_csv(df_bound, "cosine_results_all.csv")
}

