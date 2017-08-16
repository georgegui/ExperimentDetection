rm(list = ls())
platform <- Sys.info()['nodename']
if(grepl('ip-', platform)){
  my_directory <- '~/ExperimentDetection'
  data_path <- '~/data'
} else {
  my_directory <- '~/Workspace-George/ExperimentDetection_Cleaned/'
  data_path <- 'data/'
}
library(knitr)

setwd(my_directory)
# convert the up-to-date R markdown to R script
purl('Dominicks_Experiment_Detection.Rmd', 
     output = "Dominicks_Experiment_Detection.R", documentation = 2)
load('data/processed_info.RData')
category_dt <- rbindlist(category_list)[!is.na(exp_2_start)]
category_names <- category_dt$Category


for(cur_category in category_names[1:2]){
  source('Dominicks_Experiment_Detection.R')  
  setwd(my_directory)
  cur_folder <- paste0('out/time_window/', cur_category, '/')
  MakeDir(cur_folder)
  for(s in store_list){
    PlotStoreFormattedPrice(out$formatted_dt, out$store_week_prediction, s, 
                            plot_path = cur_folder)
  }
  # model_list <- grep('model_', ls(), value = TRUE)
  # model_list <- model_list[sapply(model_list, function(x) class(get(x))) == 'felm']
  # coef_list <- lapply(model_list, function(x) coef(get(x)))
}

# do it in parallel
library(parallel)
CL <- makeCluster(length(category_names))
clusterExport(CL, 'my_directory')
for(i in 1:length(category_names)){
  cur_category = category_names[[i]]
  clusterExport(CL, 'cur_category')
}
clusterEvalQ(CL, {
  setwd(my_directory) 
  source('Dominicks_Experiment_Detection.R')
  setwd(my_directory) 
  for(s in store_list){
    PlotStoreFormattedPrice(out$formatted_dt, out$store_week_prediction, s, 
                            plot_path = paste0('out/time_window/', cur_category, '/')
    )
  }
})

