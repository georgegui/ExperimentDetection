#' This function returns the average ols elasticity of a category

GetOLS <- function(result, week_table_control, exclude_holiday = TRUE){
  if(!is.null(week_table_control$time_control)){
    cur_formula <- formula('move ~ price|store + time_control')
  } else {
    cur_formula <- formula('move ~ price|store')
  }
  dt <- BasicCleaning(result$category_info)
  if(CountUnique(dt$upc) < 5) return(NULL)
  if(exclude_holiday) dt <- dt[holiday == '']
  dt[, ':='(price = log(price),
            move = log(move + 1))]
  dt <- merge(dt, week_table_control, 'week_start')
  expr_products <- result$products
  if(is.null(expr_products)){
    expr_products <- unique(dt$upc)
  }
  dt <- dt[upc %in% expr_products]
  out <- dt[, get_coef(cur_formula, .SD), by = upc]
  out <- out[, .(elas = mean(coef),
                 se = sqrt(mean(se^2, na.rm = TRUE)))]
  out[, Category := result$category_info[, Category]]
  out
}


#' Convert Dominick's format data to Nielsen
ConvertToNielsen <- function(dt, expr_time, prediction,
                             time_col = 'week_start',
                             price_col = 'price',
                             quantity_col = 'move',
                             store_label_col = 'class',
                             store_col = 'store'
){
  dt <- merge(dt, prediction[, .SD, .SDcols = c(store_col, store_label_col)],
              by = store_col)
  setnames(dt, c(time_col, price_col, quantity_col, store_col, store_label_col),
           c('week_end', 'price', 'units', 'store_code_uc', 'price_class'))
  dt[, ':='(expr_start = expr_time[[1]],
            expr_end = expr_time[[2]])]
  dt[, expr_obs := (week_end <= expr_time[[2]] &
                    week_end >= expr_time[[1]])]
  return(dt)
}


#' This function returns the average IV elasticity of Dominicks data
GetDominicksIV <- function(result, price_weight = NULL,
                           use_all_data = TRUE, exclude_holiday = TRUE){
  dt <- BasicCleaning(result$category_info)
  if(exclude_holiday) dt <- dt[holiday == '']
  if(use_all_data){
    expr_time <- as.Date(unlist(result$info[, .(predicted_expr_start, predicted_expr_end)]))
  } else {
    expr_time_start <- as.Date(unlist(result$info[, .(predicted_expr_start)]))
    expr_time <- c(expr_time_start, expr_time_start + 7*16)
    if(result$category_info$Category == 'Canned Soup'){
      history_time <- c(expr_time[[1]] - 7*52, expr_time[[1]] - 7*(52-16))
    } else {
      history_time <- c(expr_time[[1]] - 7*26, expr_time[[1]] - 7)
    }
    dt <- dt[week_start %between% expr_time | week_start %between% history_time]
  }

  dt <- ConvertToNielsen(dt, expr_time, result$prediction)

  out <- GetIv(dt, region_cols = c('zone'),
               control_class = 'middle_class',
               price_weight = price_weight)

  if(is.null(out)) return(NULL)
  if(nrow(out) == 0) return(NULL)
  out <- out[, .(elas = mean(elas_iv),
                 se = sqrt(mean(se^2, na.rm = TRUE)))]
  out[, Category := result$category_info[, Category]]
}


#' This function gets elasticity based on the elas_profile list

GetAnyElas <- function(elas_profile, documented_category_id = NULL){
  load('data/processed_info.RData')

  category_list <- rbindlist(category_list)
  cols_to_include <- c('Category', 'Hoch_Name', 'Price_Change',
                       'Documented_HiLo_Effect', 'Documented_EDLP_Effect',
                       'Documented_Significance', 'n_cluster')
  documented_result <- category_list[, .SD, .SDcols = cols_to_include]

  week_table_control <- week_table[, .(week_start)]
  if(elas_profile$use_only_largest_zone){
    price_expr <- price_expr[zone == 2]
    load('out/prediction_with_many_knowledge_one_zone.RData')
  } else {
    load('out/prediction_with_many_knowledge.RData')
  }
  if(is.null(documented_category_id)){
    documented_category_id <- which(sapply(results, function(x) !is.null(x$prediction)))
  }

  if(!elas_profile$use_predicted_products){
    for(i in documented_category_id){
      results[[i]]$products <- NULL
    }
  }

  if(!elas_profile$use_predicted_time_zone){
    documented_category_id <- intersect(
      category_list[, which(!is.na(exp_2_start) & !is.na(exp_2_end))],
      documented_category_id
    )
    for(i in documented_category_id){
      results[[i]]$info[, ':='(
        predicted_expr_start = category_list[i, exp_2_start],
        predicted_expr_end = category_list[i, exp_2_end]
      )]
    }
  }

  if(!elas_profile$use_predicted_labels){
    for(i in documented_category_id){
      results[[i]]$prediction[, class := NULL]
      results[[i]]$prediction <- merge(results[[i]]$prediction,
                                       label_map, by = 'test')
    }
  }

  if(!exists(key_cols)) key_cols = c('class', 'store')

  add_test_random <- elas_profile$use_predicted_products & elas_profile$use_predicted_labels
  if(elas_profile$use_Hoch_Method){
    predicted_result <- ParallelApply(
      results[documented_category_id], function(x) GetElasticityDiff(
        x, key_cols = key_cols,
        constant_product_weight = elas_profile$use_calculated_weighted_average_price_change,
        within_product_weight = elas_profile$use_calculated_weighted_average_price_change,
        test_random = add_test_random,
        exclude_holiday = elas_profile$exclude_holiday),
      n_cores = length(documented_category_id))
    predicted_result <- rbindlist(predicted_result, fill = TRUE)
    predicted_result <- setnames(predicted_result, 'category', 'Category')
    predicted_result <- merge(predicted_result, documented_result, by = 'Category')
    if(elas_profile$use_documented_price_change){
      predicted_result[, ':='(HiLo_Price_Change = Price_Change/100,
                              EDLP_Price_Change = -Price_Change/100)]
    }
    predicted_result[, ':='(
      Documented_HiLo_Effect = HiLo_Quantity_Change/HiLo_Price_Change,
      Documented_EDLP_Effect = EDLP_Quantity_Change/EDLP_Price_Change
    )]
    predicted_result[, ':='(elas = rowMeans(.SD, na.rm = TRUE),
                            se = NA),
                     .SDcols = c('Documented_HiLo_Effect', 'Documented_EDLP_Effect')]
  }

  if(elas_profile$use_OLS){
    if(elas_profile$control_quarter){
      week_table_control[, time_control := as.factor(year(week_start) + 0.25 * floor((month(week_start)-1)/3))]
    }
    if(elas_profile$control_month){
      week_table_control[, time_control := format(as.Date(week_start), '%Y-%m')]
    }
    if(elas_profile$control_week){
      week_table_control[, time_control := week_start]
    }
    predicted_result <- ParallelApply(results, function(x) GetOLS(
        x, week_table_control, exclude_holiday = elas_profile$exclude_holiday
        ), n_cores = length(results)
    )
    predicted_result <- rbindlist(predicted_result)
    predicted_result <- merge(predicted_result, documented_result, by = 'Category')
  }

  if(elas_profile$use_IV){
    if(elas_profile$use_calculated_weighted_average_price_change){
      price_weight <- 'units'
    } else {
      price_weight <- NULL
    }
    predicted_result <- ParallelApply(
      results[documented_category_id], function(x) GetDominicksIV(
        x, price_weight, exclude_holiday = elas_profile$exclude_holiday),
      n_cores = length(results)
    )
    predicted_result <- rbindlist(predicted_result)
    predicted_result <- merge(predicted_result, documented_result, by = 'Category')
  }
  cols_to_include <- intersect(c('Hoch_Name', 'elas', 'se', 'guess_type'), names(predicted_result))
  predicted_result[, .SD, .SDcols = cols_to_include]
}



#' @param constant_product_weight if true, the category-price change will be an
#'  average of product effective price change weighted by the revenue.
#' @param within_product_weight if true, the product effective price will be weighted
#'  by quantity sold, else it will be simple average.
#'  1) To have pure simple average across all the weeks and products, set
#'  constant_product_weight = FALSE and within_product_weight = FALSE
#'  2) To have pure quantity weighted average, set
#'  constant_product_weight = FALSE and within_product_weight = TRUE
#'  3) To have across-product weight to be constant but within-product weight to be quantity, set
#'  constant_product_weight = TRUE and within_product_weight = TRUE

GetElasticityDiff <- function(result,
                              key_cols = c('class', 'store', 'upc'),
                              within_product_weight = TRUE,
                              constant_product_weight = TRUE,
                              test_random = TRUE,
                              exclude_holiday = TRUE){
  set.seed(6)
  if(is.null(result$prediction)) return(NULL)
  expr_time <- as.Date(unlist(result$info[, .(predicted_expr_start, predicted_expr_end)]))
  dt <- BasicCleaning(result$category_info)
  if(exclude_holiday) dt <- dt[holiday == '']
  if(constant_product_weight){
    across_product_weight <- dt[, .(weight = sum(move * price)), by = upc]
    across_product_weight[, weight := weight/sum(weight)]
  } else {
    across_product_weight <- NULL
  }

  good_guess <- GetHochElasticity(dt,
                                  result$category_info,
                                  expr_time[[1]],
                                  result$prediction[, .(store, class)],
                                  result$products,
                                  key_cols,
                                  across_product_weight,
                                  within_product_weight)
  good_guess[, guess_type := 'Predicted']
  unique_upcs <- unique(dt$upc)
  product_sample_size <- length(result$products)
  time_sample_range <- dt[, unique(week_start)]
  time_sample_range <- head(tail(time_sample_range, -26), -52)
  if(!test_random){
    return(good_guess)
  }
  random_guess_list <- list()
  for(i in 1:100){
    prediction_sample <- data.table(store = result$prediction$store)
    sample_label <- sample(
      c('edlp_class', 'hilo_class', 'middle_class'), nrow(prediction_sample),
      rep = TRUE)
    prediction_sample[, class := sample_label]
    random_guess_list[[i]] <- GetHochElasticity(
      dt, result$category_info,
      sample(time_sample_range, 1),
      prediction_sample,
      sample(unique_upcs, product_sample_size),
      key_cols,
      across_product_weight,
      within_product_weight
    )
  }
  random_guess <- rbindlist(random_guess_list)
  random_guess[, guess_type := 'random']
  guess_compare <- rbind(random_guess, good_guess)
  return(guess_compare)
}


#' @param category_info category information
#' @param expr_time a list of two Date that marks the start and end of an experiment
#' @param prediction_labels a data.table that includes "store" and "class"
#'    class includes "hilo_class", "edlp_class", and "middle_class"
#' @param expr_products a list of upcs
#'
GetHochElasticity <- function(dt,
                              category_info,
                              expr_time_start,
                              predicted_class,
                              expr_products,
                              key_cols = c('class', 'store', 'upc'),
                              across_product_weight = NULL,
                              within_product_weight = TRUE){
  expr_time <- c(expr_time_start, expr_time_start + 7*16)
  if(category_info$Category == 'Canned Soup'){
    history_time <- c(expr_time[[1]] - 7*52, expr_time[[1]] - 7*(52-16))
  } else {
    history_time <- c(expr_time[[1]] - 7*26, expr_time[[1]] - 7)
  }
  if(is.null(expr_products)){
    expr_products <- unique(dt$upc)
  }
  dt <- dt[upc %in% expr_products]
  # if predicted class is not given, then we will use the actual label
  dt <- merge(predicted_class, dt, by = c('store'))


  get_product_price <- function(x, y){
    if(within_product_weight){
      wtd.mean(x, y + 1e-12, na.rm = TRUE)
    } else {
      mean(x, na.rm = TRUE)
    }
  }

  sales_before_expr <- dt[week_start %between% history_time,
                          .(price_before = get_product_price(price, move),
                            move_before = mean(move, na.rm = TRUE)),
                          by = c('class', 'store', 'upc')]
  sales_in_expr <- dt[week_start %between% expr_time,
                      .(price_in = get_product_price(price, move),
                        move_in = mean(move, na.rm = TRUE)),
                      by = c('class', 'store', 'upc')]
  sales_comparison <- merge(sales_before_expr, sales_in_expr,
                            by = c('class', 'store', 'upc'))
  # if different weights are assigned to different products, merge it
  if(!is.null(across_product_weight)){
    sales_comparison <- merge(sales_comparison, across_product_weight, by = 'upc')
    sales_comparison[, ':='(weight_before = weight,
                            weight_in = weight)]
  } else {
    if(within_product_weight){
      sales_comparison[, ':='(weight_before = move_before,
                              weight_in = move_in)]
    } else {
      sales_comparison[, ':='(weight_before = 1,
                              weight_in = 1)]
    }
  }
  sales_comparison <- sales_comparison[, .(
    price_before = wtd.mean(price_before, weight_before),
    price_in = wtd.mean(price_in, weight_in),
    move_before = mean(move_before),
    move_in = mean(move_in)
  ), by = key_cols]


  sales_comparison[, ':='(
    quantity_change = (move_in - move_before)/(move_before),
    price_change = (price_in - price_before)/price_before
  )]
  sales_comparison <- sales_comparison[, delete :=
    (is.na(price_before)|is.na(price_in)|
       is.infinite(quantity_change)|is.infinite(price_change)|
       is.na(quantity_change)|is.na(price_change))]
  sales_comparison <- sales_comparison[delete == FALSE]
  if(nrow(sales_comparison) == 0) return(NULL)
  more_obs <- sales_comparison[, .N > 1, by = class][, all(V1) & (sum(V1) > 1)]
  # remove such operation because it is no longer used
  more_obs <- FALSE
  if(more_obs){
    aov_test <- sales_comparison[, aov(quantity_change ~ class)]
    aov_test_p <- summary(aov_test)[[1]][1, 5]
  } else {
    aov_test_p <- NA
  }

  rhs_cols <- setdiff(key_cols, c('class', 'store'))
  if(length(rhs_cols) == 0){
    rhs_arg <- '.'
  } else {
    rhs_arg <- paste(rhs_cols, collapse = '+')
  }
  dcast_formula <- formula(paste(rhs_arg, '~class'))
  sales_comparison_dcast <- dcast(
    sales_comparison, dcast_formula, value.var = c('quantity_change', 'price_change'),
    fun.agg = mean)

  setnames(sales_comparison_dcast, names(sales_comparison_dcast),
           sub('_mean', '', names(sales_comparison_dcast)))

  var_cols <- sprintf('_change_%s_class',
                      c('hilo', 'edlp', 'middle'))
  var_cols <- levels(interaction(c('price', 'quantity'), var_cols, sep = ''))
  for(my_col in var_cols){
    if(!(my_col %in% names(sales_comparison_dcast))){
      sales_comparison_dcast[, (my_col) := 0]
    }
  }
  sales_comparison_dcast[, ':='(
    hilo_change = quantity_change_hilo_class - quantity_change_middle_class,
    edlp_change = quantity_change_edlp_class - quantity_change_middle_class,
    hilo_price_change = price_change_hilo_class - price_change_middle_class,
    edlp_price_change = price_change_edlp_class - price_change_middle_class
  )]
#
#   sales_comparison_avg <- sales_comparison[, .(
#     quantity_change = mean(quantity_change, na.rm = TRUE),
#     price_change = mean(price_change)
#   ), by = class]
  sales_change_summary <- sales_comparison_dcast[, .(
    category = category_info$Category,
    HiLo_Quantity_Change = mean(hilo_change, na.rm = TRUE),
    EDLP_Quantity_Change = mean(edlp_change, na.rm = TRUE),
    EDLP_Price_Change = mean(edlp_price_change, na.rm = TRUE),
    HiLo_Price_Change = mean(hilo_price_change, na.rm = TRUE),
    p_value = aov_test_p)]

#   The following tries to incorporate standard errors into the calculation
#   lm_formulas <- sprintf('%s_change ~ -1 + %s_price_change',
#                                   c('hilo', 'eldp'), c('hilo', 'eldp'))
#   elas_out <- list()
#   for(i in 1:2){
#     elas_out[[i]] <- sales_comparison_dcast[, get_coef(
#       lm_formulas[[i]], .SD, reg_func = lm)]
#   }
#   elas_out <- rbindlist(elas_out)
#   elas_out[, .(elas = mean(coef, na.rm = TRUE),
#                se = sqrt(mean(se^2, na.rm = TRUE))))]
#   cbind(sales_change_summary, elas_out)
  #

}



AnalyzeCategory <- function(category){
  print('----------------------------------------------------')
  dt <- BasicCleaning(category)
  # imputed_by_week <- dt[, .(missing_price = mean(1 - ok)), by = week_start]
  myformula <- formula('log(move + 1) ~ log(price) | store')

  if(is.null(dt)){
    print('Invalid Data Source')
    return(NULL)
  }
  print(CountUnique(dt$upc))
  # # compare experiment within the same time range
  result <- CompareInTimeRange(dt, category, p_value_threshold = 0.05,
                               exclude_holiday = FALSE)
  # result <- ControlWeekFix(dt, category, p_value_threshold = 0.05)
  # print(result)
  return(result)
}
#' Get coefficient and p-value of a felm regression
#'
# get_coef <- function(f, df, intercept = 0){
#   out <- felm(f, data = df)
#   # out = data.table(t(coefficients(summary(out))))
#   out <- data.table(t(summary(out)$coefficient[1 + intercept, c(1, 4, 2)]))
#   setnames(out, names(out), c('coef', 'p_value', 'se'))
#   out[, N_obs := nrow(df)]
#   out
# }


get_coef <- function(f, df, intercept = 0,
                     col_name = c('coef', 'p_value', 'se'),
                     reg_func = felm){
  out <- reg_func(f, data = df)
  # out = data.table(t(coefficients(summary(out))))
  out <- data.table(t(summary(out)$coefficient[1 + intercept, c(1 , 4, 2)]))
  out <- out[, .SD, .SDcols = 1:length(col_name)]
  setnames(out, names(out), col_name)
  out[, N_obs := nrow(df)]
  out
}


# Compare regression results using all data and experiment data with p-value threshold
CompareInTimeRange <- function(dt, category,
                               p_value_threshold = 0.05,
                               exclude_holiday = FALSE){
  myformula <- formula('log(move + 1) ~ log(price) | store')
  if(exclude_holiday) dt <- dt[holiday == '']
  key_cols <- c('zone', 'upc', 'test')
  dt[, grp := .GRP, by = key_cols]
  # filter cases without any price variation
  dt[, d_price := price - mean(price), by = .(store, upc)]
  dt <- dt[, count := CountUnique(d_price), by = key_cols
           ][count > 1][, count := NULL]
  # run naive regression
  naive_result = dt[, get_coef(myformula, .SD), by = .(upc, zone)]
  naive_result = naive_result[p_value < 0.05, .(positive_percentage = mean(coef > 0),
                                                test = 'All Data',
                                                Number_of_upc_zone = .N)]
  # filter non-experiment time
  dt <- dt[(week_start < category$exp_2_end + 7 * 0) &
             (category$exp_2_start - 7*0 < week_start)]
  # filter cases without any price variation during experiment
  dt[, d_price := price - mean(price), by = .(store, upc)]
  dt <- dt[, count := CountUnique(d_price), by = key_cols
           ][count > 1][, count := NULL]
  result_experiment <- dt[, get_coef(myformula, .SD), by = key_cols]
  out = result_experiment[p_value < p_value_threshold,
                          .(positive_percentage = wtd.mean(coef > 0, N_obs),
                            Number_of_upc_zone  = .N),
                          by = 'test']
  out <- rbind(out, naive_result)
  out[, category_name := category$Category]
  return(out)
}


#' Compare regression result between the experiment time range and the overall range
#'
#' @param dt cleaned price movement data.table
#' @param category categroy info
#' @param myformula regression formula
#' @return a data.table that compares the percentage of positive elatsicities
CompareTimeRange <- function(dt, category, myformula){
  result_naive <- dt[, .(naive_elas = get_coef(myformula, .SD)),
                     by = c('zone', 'upc')]

  result_experiment <- dt[(week_start < category$exp_2_end) & (category$exp_2_start + 1 < week_start),
                          .(expr_elas = get_coef(myformula, .SD)),
                          by = c('zone', 'upc')]
  result_comparison <- merge(result_naive, result_experiment,
                             by = c('zone', 'upc'))
  result_comparison[, .(expr_positive_percentage = mean(expr_elas > 0),
                        all_positive_percentage = mean(naive_elas > 0))]
}


#' First difference regression near the start of the experiment
#'
#' Only include week change before and after the start of the experiment
#'
#' @param dt a price movement data.table that only includes two weeks
FD <- function(dt){
  dt[, exogenous_price := week_start == max(week_start)]
  dt_dcast <- dcast(dt, upc + store + zone + test ~ exogenous_price,
                    value.var = c('move', 'price'))
  dt_dcast[, ':='(move_change = (move_TRUE - move_FALSE)/(move_FALSE),
                  price_change = (price_TRUE - price_FALSE)/price_FALSE)]
  result = dt_dcast[, coef(lm(move_change ~ price_change - 1)), by = .(upc, test)]

  result = dt_dcast[, mean(elas), by = .(upc, zone)]
  dt[, exogenous_price ]
}

#' Sale change relative to price change near the start of the experiment
#'
#' Only include month before and after the start of the experiment. Calculate the
#'   ratio of sale change relative to the price change
#'
#' @param category category info
#' @return a ggplot that summarizes the sale change.
RegressPriceChange <- function(category){
  dt <- BasicCleaning(category)
  predicted_range <- IdentifyTimeFrame(dt)
  exp_start <- ConfirmExperimentStart(dt, c(category$exp_2_start,
                                            category$exp_2_end))
  dt <- dt[(week_start >= exp_start[[1]] - 3 * 7) &
             week_start <= exp_start[[2]] + 3 * 7]
  dt[, after_exp := week_start >= exp_start[[2]]]
  dt_local <- dt[, .(move = mean(move), price = mean(price)),
                 by = .(upc, store, after_exp, test)]
  dt_local <- dcast(dt_local, upc + store + test ~ after_exp,
                    value.var = c('move', 'price'))
  dt_local[, ':='(
    price_change = (price_TRUE - price_FALSE)/price_FALSE,
    quantity_change = (move_TRUE - move_FALSE)/move_FALSE)]
  dt_local[abs(price_change) > 0.03, elasticity := quantity_change/price_change]
  range = quantile(dt_local$elasticity, c(0.02, 0.98), na.rm = TRUE)
  ggplot(dt_local, aes(x = elasticity)) +
    geom_histogram(fill = 'steelblue') +
    scale_x_continuous(limits = c(max(-100, range[[1]]),
                                  min(100, range[[2]]))) +
    facet_grid(~test) +
    labs(title = category$Category)

  # myformula <- formula('log(move + 1) ~ log(price) | store')
  # felm(myformula, dt[test != 'Control'])
  # result <- dt[, get_coef(myformula, .SD), by = .(upc, test)]
  # dt[, ':='(d_price = log(price) - mean(log(price)),
  #           d_move = log(move + 1) - mean(log(move + 1))),
  #    by = .(store, upc)]
  # ggplot(dt, aes(x = d_price, y = d_move)) + geom_point()
  # result[p_value , mean(coef > 0), by = test]
}


# TODO: test the function
#' Regression by taking difference between experiment stores and control stores
ControlWeekFix <- function(dt, category,
                           p_value_threshold = 0.05,
                           exclude_holiday = FALSE){
  dt <- copy(dt)
  if(exclude_holiday) dt <- dt[holiday == '']
  test_vals <- dt[, unique(test)]
  n_test <- length(test_vals)
  dt <- dt[, count := length(unique(test)), by = zone
           ][count == n_test][, count := NULL]
  key_cols <- c('zone', 'upc', 'test')
  dt[, grp := .GRP, by = key_cols]
  dt <- dt[(week_start <= (category$exp_2_end + 0 * 7)) &
             (week_start >= (category$exp_2_start - 0 * 7))]
  dt[, d_price := price - mean(price), by = .(store, upc)]
  dt <- dt[, count := CountUnique(d_price), by = key_cols
           ][count > 1][, count := NULL]

  for(cur_col in test_vals){
    col_name = gsub('[^a-zA-Z]', '', cur_col)
    dt[, (col_name) := 0]
    dt[cur_col == test, (col_name) := log(price)]
  }
  price_cols <- gsub('[^a-zA-Z]', '', test_vals)
  myformula <- formula(paste0(
    'log(move + 1) ~ ', paste(price_cols, collapse = '+'), '|store + week'
  ))
  #   dt[, ':='()]
  #   dt <- dcast(dt, dcast_formula, value.var = 'price', )
  get_coef <- function(f, df){
    out <- felm(f, data = df)
    out = summary(out)$coefficient[, c(1, 4)]
    out = data.table(rownames(out), out)
    setnames(out, names(out), c('test', 'coef', 'p_value'))
    out[, N_obs := nrow(df)]
    return(out)
    # out = data.table(t(coefficients(summary(out))))
  }
  result_experiment <- dt[, get_coef(myformula, .SD), by = .(upc, zone)]
  out = result_experiment[p_value < p_value_threshold,
                          .(positive_percentage = mean(coef > 0, na.rm = TRUE)),
                          by = test]
  # print(out)
  out[, category_name := category$Category]
  return(out)
}


