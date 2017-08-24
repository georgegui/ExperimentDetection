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
library(data.table)
setwd(my_directory)

file_list <- c('R/Cleaning.R',
               'R/Elasticity_Function.R',
               'R/Main_Functions.R')
invisible(lapply(file_list, source))
rm(file_list)
# convert the up-to-date R markdown to R script
file.remove("Dominicks_Experiment_Detection.R")
purl('Dominicks_Experiment_Detection.Rmd', 
     output = "Dominicks_Experiment_Detection.R", documentation = 2)
load('data/processed_info.RData')
category_dt <- rbindlist(category_list)[!is.na(exp_2_start)]
category_dt <- category_dt[Category != 'Cigarettes']
category_names <- category_dt$Category

# for(cur_category in category_names[1:2]){
#   source('Dominicks_Experiment_Detection.R')  
#   setwd(my_directory)
#   cur_folder <- paste0('out/time_window/', cur_category, '/')
#   MakeDir(cur_folder)
#   for(s in store_list){
#     PlotStoreFormattedPrice(out$formatted_dt, out$store_week_prediction, s, 
#                             plot_path = cur_folder)
#   }
#   # model_list <- grep('model_', ls(), value = TRUE)
#   # model_list <- model_list[sapply(model_list, function(x) class(get(x))) == 'felm']
#   # coef_list <- lapply(model_list, function(x) coef(get(x)))
# }

# do it in parallel
library(parallel)
CL <- makeCluster(length(category_names))
clusterExport(CL, 'my_directory')
for(i in 1:length(category_names)){
  cur_category = category_names[[i]]
  clusterExport(CL[i], 'cur_category')
}
# run the basic estimation for all categories
tmp <- clusterEvalQ(CL, {
  setwd(my_directory) 
  source('Dominicks_Experiment_Detection.R')
  setwd(my_directory) 
  NULL
})

# making plots
tmp <- clusterEvalQ(CL, {
  cur_folder <- paste0('out/time_window/', cur_category, '/')
  MakeDir(cur_folder)
  for(s in store_list){
    PlotStoreFormattedPrice(out$formatted_dt, out$store_week_prediction, s, 
                            plot_path = paste0('out/time_window/', cur_category, '/')
    )
  }
})

for(INCLUDE_ZERO_QUANTITY in c(TRUE, FALSE)){
  clusterExport(CL, 'INCLUDE_ZERO_QUANTITY')
  tmp <- clusterEvalQ(CL, source('summarize_result.R'))
  category_summary <- clusterEvalQ(CL, {
    data.table(
      n_store_weeks = nrow(out$store_week_prediction), 
      n_store_weeks_wo_undocumented = out$store_week_prediction[store_time_label != 'Undocumented', .N], 
      n_store_weeks_correct = out$store_week_prediction[, sum(correct, na.rm = TRUE)],
      n_store_weeks_expr = out$store_week_prediction[is_expr_time == TRUE, .N], 
      n_store_weeks_expr_correct = out$store_week_prediction[is_expr_time == TRUE, sum(correct, na.rm = TRUE)],
      elas_documented = documented_elas, 
      elas_all = coef(model_all_basic)[[2]], 
      elas_all_fe = coef(model_all_fe), 
      elas_all_fe_inter = coef(model_all_fe_inter), 
      elas_expr = coef(model_expr_basic)[[2]], 
      elas_expr_fe = coef(model_expr_fe), 
      elas_expr_fe_inter = coef(model_expr_fe_inter), 
      elas_guess_avg = mean(random_guess_coefs), 
      elas_guess_sd = sd(random_guess_mse), 
      elas_guess_mse_q05 = quantile(random_guess_mse, 0.05), 
      elas_guess_mse_q50 = quantile(random_guess_mse, 0.50), 
      elas_guess_mse_avg = mean(random_guess_mse), 
      elas_guess_inter_sd = sd(random_guess_mse_fe_inter), 
      elas_guess_inter_mse_q05 = quantile(random_guess_mse_fe_inter, 0.05), 
      elas_guess_inter_mse_q50 = quantile(random_guess_mse_fe_inter, 0.50), 
      elas_guess_inter_mse_avg = mean(random_guess_mse_fe_inter)
    )
  })
  
  category_summary <- rbindlist(category_summary)
  category_summary[, ':='(
    accuracy_all_time = n_store_weeks_correct/n_store_weeks, 
    accuracy_expr_time = n_store_weeks_expr_correct/n_store_weeks_expr
  )]
  output_file <- ifelse(INCLUDE_ZERO_QUANTITY, 'out/result_comparison.RDS', 
                        'out/result_comparison_wo_0.RDS')
  saveRDS(category_summary, output_file)
}


for(INCLUDE_ZERO_QUANTITY in c(TRUE, FALSE)){
  output_file <- ifelse(INCLUDE_ZERO_QUANTITY, 'out/result_comparison.RDS', 
                        'out/result_comparison_wo_0.RDS')
  category_summary <- readRDS(output_file)
  category_summary[, ':='(
    predicted_mse = (elas_documented - elas_expr)^2, 
    predicted_mse_inter = (elas_documented - elas_expr_fe_inter)^2
  )]
  for(cur_col in names(category_summary)){
    if(is.numeric(category_summary[, get(cur_col)])){
      category_summary[, (cur_col) := round(get(cur_col), 2)]
    }
  }
  
  category_summary <- cbind(category_dt, category_summary)
  category_summary[, Category := Hoch_Name]
  category_summary <- category_summary[order(Category)]
  
  table_profiles <- list(
    accuracy = list(
      file_name = 'prediction_accuracy.tex', 
      col_var_names =  c(
        'n_store_weeks',  'n_store_weeks_correct', 'accuracy_all_time', 
        'n_store_weeks_expr', 'n_store_weeks_expr_correct', 'accuracy_expr_time'),
      col_print_names = c('\\# Stores-Weeks', '\\# Correct', 'Accuracy',
                          '\\# Store-Weeks', '\\# Correct', 'Accuracy'),
      row_names = category_summary[, Category], 
      rgroup = NULL, 
      cgroup = c('All Data', 'Within Experiment Period'), 
      n_cgroups = c(3, 3), 
      col_name_just = (rep('c', 6))
    ),
    elasticity = list(
      file_name = 'elasticity_combined_mix.tex', 
      col_var_names = c('Category', 'accuracy_all_time', 'accuracy_expr_time','elas_documented', 'elas_expr',
                        'predicted_mse',  'elas_guess_mse_q05',
                        'elas_guess_mse_q50'), 
      col_print_names = c(' ', 'Data', 'Period', rep(' ', 3), '5th-quantile', 'Median'), 
      col_name_just =  c('l|', 'c', 'c|', 'c', 'c|', rep('c', 3)), 
      rows_to_inserts = list(
        function(x) AddMultiColumn(x,
                                   cgroup = c(' ', 'Accuracy', 'Elasticity', 'Square Error'),
                                   n.cgroup = c(1, 2, 2, 3),
                                   cgroup.just = c('l|', 'c|', 'c|', 'c'), keyword = '5th'),
        function(x) AddMultiColumn(x,
                                   cgroup = c('Category', 'All', 'Experiment', 'Documented ', 'Predicted',
                                              'Predicted', 'Random Guess'),
                                   n.cgroup = c(rep(1, 6), 2),
                                   cgroup.just = c('l|', 'c', 'c|', 'c', 'c|', 'c', 'c'),
                                   keyword = '5th',
                                   hline = FALSE) 
      )
    ), 
    elasticity_combined = list(
      file_name = 'all_elasticities_mix.tex', 
      col_var_names = c('Category', 'accuracy_all_time', 'accuracy_expr_time', 'elas_documented', 'elas_expr',
                        'elas_all', 'elas_all_fe'), 
      col_print_names = c('Category', 'All Data', 'Expriment Period',
                          'Documented', 'Predicted', 'OLS', 'OLS Week FE'), 
      col_name_just =  c('l|', 'c', 'c|', rep('c', 7 - 3)), 
      col_label_just = c('l|', 'c', 'c|', rep('c', 7 - 3)), 
      row_name = NULL, 
      rows_to_inserts = list(
        function(x) AddMultiColumn(x,
                                   cgroup = c('', 'Accuracy', 'Elasticity'),
                                   n.cgroup = c(1, 2, 7 - 3),
                                   cgroup.just = c('c|', 'c|', 'c'))
      )
    )
  )
  
  table_profiles[['elasticity_inter']] <- table_profiles[['elasticity']]
  table_profiles[['elasticity_inter']]$file_name = 'elasticity_combined_mix_inter.tex'
  table_profiles[['elasticity_inter']]$col_var_names = c(
    'Category', 'accuracy_all_time', 'accuracy_expr_time','elas_documented', 'elas_expr_fe_inter',
    'predicted_mse_inter',  'elas_guess_inter_mse_q05',
    'elas_guess_inter_mse_q50')

  table_profiles[['elasticity_combined_inter']] <- table_profiles[['elasticity_combined']]
  table_profiles[['elasticity_combined_inter']]$file_name = 'all_elasticities_mix_inter.tex'
  table_profiles[['elasticity_combined_inter']]$col_var_names = c(
    'Category', 'accuracy_all_time', 'accuracy_expr_time', 'elas_documented','elas_expr_fe_inter',
    'elas_all', 'elas_all_fe_inter')
  
  for(tp in table_profiles){
    cur_file_name <- paste0('out/', tp$file_name)
    if(!INCLUDE_ZERO_QUANTITY) cur_file_name <- sub('.tex', '_wo_0.tex', cur_file_name)
    print(cur_file_name)
    tp_tex <- latex(category_summary[, tp$col_var_names, with = FALSE],
                    cgroup = tp$cgroup, 
                    n.cgroup = tp$n_cgroups,
                    rowname = tp$row_names,
                    rowlabel = ' ' ,
                    rgroup = tp$rgroup,
                    colheads = tp$col_print_names, 
                    file = cur_file_name,
                    table.env = FALSE,
                    extracolsize = 'small',
                    col.just = tp$col_name_just,
                    collabel.just = tp$col_name_just,
                    size = 'small')
    tp_tex <- readLines(cur_file_name)
    for(cur_row_func in tp$rows_to_inserts){
      tp_tex <- cur_row_func(tp_tex)
    }
    write(tp_tex, file = cur_file_name)
  }
}
