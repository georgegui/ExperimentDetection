#' ---
#' title: "R Notebook"
#' output: html_notebook
#' ---
#' 
#' # Path and Library Setting 
## ---- warning=FALSE, message=FALSE---------------------------------------
require(CommonFunctions)
require(Hmisc)
require(lfe)
require(ggplot2)
require(gridExtra)
require(data.table)
require(mixtools)
require(scales)
require(stargazer)
platform <- Sys.info()['nodename']
if(grepl('ip-', platform)){
  setwd('~/ExperimentDetection')
  data_path <- '~/data'
} else {
  setwd('~/Workspace-George/ExperimentDetection_Cleaned/')
  data_path <- 'data/'
}

file_list <- c('R/Cleaning.R',
               'R/Elasticity_Function.R',
               'R/Main_Functions.R')
invisible(lapply(file_list, source))
rm(file_list)

#' 
#' ## Category Experiment File
## ------------------------------------------------------------------------
load('data/processed_info.RData')
head(price_expr)

#' 
#' ## Relevant Experiment with Documented Time
#' 
## ------------------------------------------------------------------------
relevant_cols <- c('store', 'zone', grep('test$', names(price_expr), value = TRUE))
head(price_expr[, relevant_cols, with = FALSE])

#' 
#' # Global Variables
#' The following variables will be global variables that used through out the code and inside various functions implicitly
## ------------------------------------------------------------------------
PRICE_EXPR <- price_expr[zone == 2] # select the largest zones of 27 stores
WEEK_TABLE <- week_table #

color_list <- hue_pal()(3)
color_list <- c(color_list, 'darkred', 'darkred', color_list[[2]], 'darkgreen', color_list[[1]], 'gray')
names(color_list) <- c('EDLP', 'Hi-Lo', 'Control', 'EDLP/S', 'EDLP2', 'Hi-Lo1', 'Hi-Lo2', 'EDLP1', 'Undocumented')
COLOR_LIST <- color_list
GG_COLOR <- scale_color_manual(values = COLOR_LIST)
rm(list = c('price_expr', 'week_table', 'color_list'))

#' 
#' 
#' # Category Information
## ------------------------------------------------------------------------
category_dt <- rbindlist(category_list)
category_dt[, c('Category', 'exp_2_start', 'exp_2_end'), with = FALSE]

#' 
#' # Nonparametric Mixture Estimation
#' 
#' ## Define function
## ------------------------------------------------------------------------
PlotOriginalPrice <- function(dt, price_col = 'price'){
  dt_avg <- dt[, .(
    price = mean(get(price_col))
    ), by = .(week_start, test, zone)]
  gg <- ggplot(dt_avg, aes(x = week_start, y = price, colour = test)) + geom_line() + GG_COLOR
  return(gg)
}

PlotStoreFormattedPrice <- function(dt, store_week_prediction, 
                                    store_id = 2, plot_path = NULL){
  dt <- merge(dt[store == store_id], 
              store_week_prediction[, .(store, pred_label, week_start)], 
              by = c('store', 'week_start'))
  dt_to_plot <- rbind(
    dt[store == store_id, .(price = mean(formatted_price)),
       by = .(week_start, store_time_label = pred_label)][, type := 'Predicted'],
    dt[store == store_id, .(price = mean(formatted_price)),
       by = .(week_start, store_time_label)][, type := 'Documented']
  )
  dt_to_plot[, type := factor(type, levels = c('Predicted', 'Documented'))]
  gg <- ggplot(dt_to_plot, aes(x = week_start, y = price, colour = store_time_label)) +
    geom_point() + facet_grid(type ~ .) +
    theme(legend.position = 'bottom') +
    scale_color_manual("Documented/Predicted Label", values = COLOR_LIST) + 
    labs(x = 'Week Start', y = 'Demeaned Log Price')
  if(!is.null(plot_path)){
    png(file = paste0(plot_path, store_id, '.png'),
        width = 11*50, height = 8.27*50)
    print(gg)
    dev.off()
  }
  return(gg) 
}

GetNpMix <- function(
  category_info, 
  dt = NULL, 
  n_product_limit = 20, 
  price_format_function = function(x) log(x) - mean(log(x)), 
  time_range_start = "1992-01-01", # 
  time_range_end = "1993-12-31"
  ){ 
  if(is.null(dt)){
    dt <- BasicCleaning(category_info)
  } else {
    dt <- copy(dt)
  }
  dt[, week := paste0('t', week)] # rename numeric week columns to charcter for dcasting
  unique_products <- dt[, unique(upc)]
  # cap the product limit to save time
  if(length(unique_products) > n_product_limit) unique_products <- unique_products[1:n_product_limit] 
  dt <- dt[, upc_name := paste0('p', upc)] #rename upc to character for dcasting
  # label the experiment store-week according to the documentation
  dt[, is_expr_time := 
       week_start >= category_info$exp_2_start &
       week_start <= category_info$exp_2_end] 
  # use store label(test) and time label to create store_time_label
  dt[, store_time_label := test]
  dt[is_expr_time == FALSE, store_time_label := 'Control']
  dt[week_start > category_info$exp_2_end, store_time_label := 'Undocumented']
  # select a subset of weeks
  dt <- dt[week_start >= time_range_start & week_start <= time_range_end]
  # demean 
  dt[, formatted_price := price_format_function(price), by = .(zone, week, upc)]
  #-----
  dt_dcast <- dcast(dt, store + test + is_expr_time +
                      store_time_label + week + week_start ~ upc_name,
                    value.var = 'formatted_price')
  product_cols <- paste0('p', unique_products)
  summary(dt_dcast)
  # initial guess
  mu0 <- matrix(rep(c(-0.05, 0, 0.05), length(product_cols)), nrow = 3)
  # run the nonparametric estimation
  out <- mvnpEM(as.matrix(dt_dcast[, product_cols, with = FALSE]),
                mu0 = mu0)
  # summarize the posterior
  dt_prob <- out$posteriors
  dt_prob_label <- c('EDLP', 'Control', 'Hi-Lo')[apply(dt_prob, 1, which.max)]
  dt_prob_max <- apply(dt_prob, 1, max)
  dt_dcast[, pred_label := dt_prob_label]
  dt_dcast[, prob := dt_prob_max]
  dt_dcast[is_expr_time == TRUE, mean(pred_label == store_time_label)]
  dt_dcast[store_time_label != 'Undocumented', correct := pred_label == store_time_label]
  expr_time_accuracy <- dt_dcast[is_expr_time == TRUE, .(
    true_experiment = wtd.mean(correct, na.rm = TRUE))]
  all_time_accuracy <- dt_dcast[, .(true_experiment = wtd.mean(correct, na.rm = TRUE))]
  store_week_prediction <- dt_dcast[, .(
    store, is_expr_time, store_time_label,
    test, week_start, prob, pred_label, correct)]
  list(
    formatted_dt          = dt,                      # formatted price movement data
    all_time_accuracy     = all_time_accuracy,       # the accuracy for all store-week
    expr_time_accuracy    = expr_time_accuracy,      # the accuracy for within the documented experiment period
    store_week_prediction = store_week_prediction   # the store-week prediction data.table
  )
}


#' 
#' 
#' ## List of Well-Documented Category
## ------------------------------------------------------------------------
category_dt <- category_dt[!is.na(exp_2_start)]
category_dt[, .(Category)]
    

#' 
#' ## Select a categry and load the movement data
## ------------------------------------------------------------------------
if(!exists('cur_category')) cur_category = 'Analgesics'
category_info <- category_dt[Category == cur_category]
dt <- BasicCleaning(category_info)

#' 
#' ##  Run the Estimation
#' 
## ------------------------------------------------------------------------
out <- GetNpMix(
  category_info, 
  dt                    = dt, 
  n_product_limit       = 20, 
  price_format_function = function(x) log(x) - mean(log(x)), 
  time_range_start      = "1992-01-01", # 
  time_range_end        = "1993-12-31"
  )

#' ## Original category level price by documented store label
## ------------------------------------------------------------------------
PlotOriginalPrice(out$formatted_dt)

#' 
#' ## Availabel stores
#' 
## ------------------------------------------------------------------------
store_list <- dt[, unique(store)]
store_list

#' 
#' ## Pick a store and plot its prediction price
#' 
## ------------------------------------------------------------------------
PlotStoreFormattedPrice(out$formatted_dt, out$store_week_prediction, 5)

#' 
#' ## Estimate Elasticity
#' 
#' ### Use all the data
## ------------------------------------------------------------------------
estimation_data <- merge(
  out$formatted_dt, 
  out$store_week_prediction[, .(store, week_start, pred_label)], 
  by = c('store', 'week_start'))
estimation_data[, demeaned_move := log(move + 1) - mean(log(move + 1)), 
                by = .(week_start, upc)]
estimation_data[, ':='(
  week_start = as.factor(week_start), 
  upc = as.factor(upc)
  )]
model_all <- felm(log(move + 1) ~ log(price), estimation_data)
model_all_fe <- felm(log(move + 1) ~ log(price)|week_start + upc, estimation_data)
# the interaction using felm and manual demeaning
model_all_fe_inter <- felm(log(move + 1) ~ log(price)|week_start * upc, estimation_data)
model_all_fe_inter2 <- felm(demeaned_move ~ formatted_price, estimation_data)

#' 
#' ### Use experiment data
#' 
## ------------------------------------------------------------------------
model_expr <- felm(log(move + 1) ~ log(price), estimation_data[pred_label != 'Control'])
# use the experiment data to demean 
model_expr_fe <- felm(log(move + 1) ~ log(price)|week_start + upc, 
                      estimation_data[pred_label != 'Control'])
model_expr_fe_inter <- felm(
  log(move + 1) ~ log(price)|as.factor(week_start) * as.factor(upc), 
  estimation_data[pred_label != 'Control'])
# use all the data to demean instead of only the experiment data, currently I used this one.
model_expr_fe_inter2 <- felm(
  demeaned_move ~ formatted_price, 
  estimation_data[pred_label != 'Control'])


#' 
#' # Which elasticties to report?
#' 
## ------------------------------------------------------------------------
comparison_table <- stargazer(
  model_all, model_all_fe, model_all_fe_inter, model_expr_fe_inter, model_expr_fe_inter2, 
  add.lines = list(
    c('All Data', rep('Yes', 3), rep('No', 2)), 
    c('Fixed Effect', 'No', rep('Yes', 4)), 
    c('Interaction', rep('No', 2), rep('Yes', 3)), 
    c('FE before Filteirng', rep('', 3), 'No', 'Yes')),
  type = 'text', omit.stat=c("f", "ser"))


#' 