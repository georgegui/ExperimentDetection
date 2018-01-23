#' ---
#' title: "R Notebook"
#' output: html_notebook
#' ---
#' # Path and Library Setting
## ---- warning=FALSE, message=FALSE---------------------------------------
library(CommonFunctions) #https://github.com/georgegui/CommonFunctions_Public

package_list <- c('mixtools', 'gridExtra', 'lfe', 'matrixStats',
                  'RcppArmadillo', 'Hmisc', 'lfe', 'gridExtra',
                  'data.table', 'mixtools', 'scales', 'stargazer', 'zoo')

#matrixStats installed from source
for(cur_package in package_list){
  if(!require(cur_package, character.only = TRUE)){
    install.packages(cur_package, repos='http://cran.rstudio.com/')
    require(cur_package, character.only = TRUE)
  }
}

platform <- Sys.info()['nodename']

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
#' ### Load Preselected Products
#' Load a list of preselected products that are likely to be in experiment as documented
## ------------------------------------------------------------------------
product_list <- readRDS('data/preselected_products.RDS')

#'
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
rm(list = c('price_expr', 'color_list'))

#'
#'
#' # Category Information
## ------------------------------------------------------------------------
category_dt <- rbindlist(category_list)
category_dt[, Documented_EDLP_Effect_neg := -Documented_EDLP_Effect] # adjust the documented EDLP to consistent sign.
category_dt[, c('Category', 'exp_2_start', 'exp_2_end'), with = FALSE]

#'
#' # Nonparametric Mixture Estimation
#'
#' ## Define function
## ------------------------------------------------------------------------
SelectDispersedProducts <- function(dt,
                                    expr_time = NULL,
                                    expr_weeks = 8,
                                    expr_percentage = .8,
                                    price_change_threshold = .05){
  # calculate maximum percentage price difference
  dt[, max_price_range := (max(price) - min(price))/mean(smoothed_price),
     by = .(upc, zone, week_start)]
  price_diff <- dt[, .(max_price_range = (max(price) - min(price))/mean(smoothed_price)),
                   by = .(upc, zone, week_start)]
  if(!is.null(expr_time)){
    # if the experiment time frame is provided, use it to calcualte price edispersion
    price_diff <- price_diff[week_start %between% expr_time, .(
      avg_diff = mean(max_price_range > price_change_threshold)),
      by = .(upc)]
  } else {
    # if the experiment time frame is not provided, find all time-frame of length expr_weeks
    #   and evaluate the avergae price experiment condition
    setkey(price_diff, upc, week_start)
    price_diff <- price_diff[, rollmean(max_price_range > price_change_threshold,
                                        expr_weeks), keyby = .(upc)]
    price_diff <- price_diff[, .(avg_diff = max(V1)), by = upc]
  }
  # if the expr_percentage is kmean, use kmean to generate a expr_percentage threshold
  if(expr_percentage == 'kmean'){
    higher_percentage <- GetTwoClass(price_diff$avg_diff)
    selected_upc <- price_diff$upc[higher_percentage == 1]

  } else {
    price_diff <- price_diff[avg_diff > expr_percentage]
    if(nrow(price_diff) == 0) return(NULL)
    selected_upc <- unique(price_diff$upc)
  }
  # else filter using numeric expr_percentage
  dispersion_diff <- mean(price_diff[upc %in% selected_upc, avg_diff]) -
    mean(price_diff[!(upc %in% selected_upc), avg_diff])
  attr(selected_upc, 'dispersion_difference') = dispersion_diff
  return(selected_upc)
}


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
  selected_products = NULL,
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
  # rank product by their non-uniform price weeks; some products have almost no price variation and is clearly not in
  #
  product_rank = dt[, mean(formatted_price != 0), by = upc
                    ][order(-V1), .(disp_rank = 1:.N, upc)]
  if(is.null(selected_products)){
    selected_products <- SelectDispersedProducts(dt)
    dt <- dt[upc %in% selected_products]
    selected_products <- dt[, sum(formatted_price != 0), by = upc][order(-V1), upc]
  }
  if(length(selected_products) < 3) stop('not enough products')
  # cap the product limit to save time
  if(length(selected_products) > n_product_limit){
    selected_products <- selected_products[1:n_product_limit]
    dt <- dt[upc %in% selected_products]
  }
  # select a subset of products with enough price variation
  # dt[, log_price := log(price)]
  # dt[, price_d := log_price - mean(log_price), by = .(zone, week, upc)]

  #-----
  dt_dcast <- dcast(dt, store + test + is_expr_time +
                      store_time_label + week + week_start ~ upc_name,
                    value.var = 'formatted_price')
  product_cols <- paste0('p', selected_products)
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
if(!exists('cur_category')) cur_category = 'Cheese'
category_info <- category_dt[Category == cur_category]
dt <- BasicCleaning(category_info)

#'
#' ##  Run the Estimation
#'
## ------------------------------------------------------------------------
set.seed(5)
out <- GetNpMix(
  category_info,
  dt                    = dt,
  selected_products     = product_list[[cur_category]],
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
## ------------------------------------------------------------------------
if(!exists('INCLUDE_ZERO_QUANTITY')) INCLUDE_ZERO_QUANTITY = TRUE
if(INCLUDE_ZERO_QUANTITY){
  quantity_func <- function(x) log(x + 1)
} else {
  quantity_func <- function(x) log(x)
}

#'
#'
#'
## ------------------------------------------------------------------------
source('summarize_result.R')


#'
#' # Which elasticties to report?
#'
## ------------------------------------------------------------------------
# comparison_table <- stargazer(
#   model_all_basic, model_all_fe, model_all_fe_inter, model_expr_fe_inter, model_expr_fe_inter2,
#   add.lines = list(
#     c('All Data', rep('Yes', 3), rep('No', 2)),
#     c('Fixed Effect', 'No', rep('Yes', 4)),
#     c('Interaction', rep('No', 2), rep('Yes', 3)),
#     c('FE before Filteirng', rep('', 3), 'No', 'Yes')),
#   type = 'text', omit.stat=c("f", "ser"))


