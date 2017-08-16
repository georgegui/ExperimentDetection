
#' Visualize the predicted time frame in a price movement graph colored
#'   by type of experiment.

VisualizeResult <- function(result, include_time_frame = TRUE,
                            label_column = 'test', additional_dt = NULL,
                            time_range = NULL){
  if(is.null(result$prediction)) return(NULL)
  dt <- BasicCleaning(result$category_info)
  n_test = CountUnique(dt$test)
  dt <- dt[, count := CountUnique(test), by = .(upc, zone)
           ][count == n_test][, count := NULL]
  if(!is.null(dt)){
    dt <- dt[week_start >= time_range[[1]] & week_start <= time_range[[2]]]
  }
  if(!is.null(additional_dt)){
    dt <- merge(dt, additional_dt, by = intersect(names(dt), names(additional_dt)))
  }
  # if(!setequal(dt$test, all_labels)) return(NULL)
  avg_price <- dt[, .(price = mean(price)), by = c('week_start', label_column, 'zone')]
  avg_price <- avg_price[, .(price = mean(price)), by = c('week_start', label_column)]

  g <- ggplot(avg_price, aes(x = week_start, y = price)) +
    geom_line(aes_string(color = label_column))  +
    guides(fill=guide_legend(ncol=2)) +
    theme_minimal() +
    theme(legend.position = 'bottom') +
    labs(color = "", linetype = "", x = 'Week Start', y = 'Price')
  if(include_time_frame){
    if(!is.na(result$category_info$exp_2_start)){
      vline_dt <- data.table(date = as.numeric(c(
        result$info$predicted_expr_start,
        result$info$predicted_expr_end,
        result$category_info$exp_2_start,
        result$category_info$exp_2_end)),
        prediction = c(
          rep('Predicted Time Frame', 2), rep('Documented Time Frame', 2))
      )
    } else {
      vline_dt <- data.table(date = as.numeric(c(
        result$info$predicted_expr_start,
        result$info$predicted_expr_end)),
        prediction = c(
          rep('predicted time frame', 2))
      )
    }
    g <- g +  geom_vline(data = vline_dt,
                aes(xintercept = date, linetype = prediction),
                alpha = 0.5, size = 1.5)
  }

  return(g)
}


VisualizePricePrediction <- function(category, price_level = TRUE,
                                     log_price = FALSE,
                                     fit_func = UnbiasedLasso){
  dt <- BasicCleaning(category)
  dt[, test := factor(test, levels = unique(test))]
  predicted_range <- previous_result[category_name == category$Category,
                                     .(predicted_expr_start, predicted_expr_end)]
  predicted_range <- as.Date(unlist(predicted_range))
  predicted_range[[1]] <- as.Date("1992-04-01")
  cur_product <- SelectDispersedProducts(
    dt, predicted_range, expr_percentage = 'kmean'
  )
  setkey(dt, upc, store, week_start)
  # dt[, price := price - shift(price), by = .(upc, store)]
  dt <- dt[!is.na(price)]

  relevant_weeks <- intersect(week_table[week_start %between% predicted_range, week_start],
                              unique(dt$week_start))
  # dt <- MergeCategoryPrice(dt, category_price, relevant_weeks)
  # dt[, price := log(price)]

  # dt_dcast_rhs <- dcast(dt[upc < 10],
  #                       week_start ~ store + upc, value.var = 'price')
  dt_dcast_rhs <- dcast(dt[!(upc %in% cur_product)],
                        week_start ~ store + upc, value.var = 'price')
  all_weeks <- dt_dcast_rhs$week_start
  expr_weeks <- all_weeks %in% relevant_weeks
  dt_dcast_rhs <- as.matrix(dt_dcast_rhs[, .SD, .SDcols = -1])
  avg_price <- dt[upc %in% cur_product, .(price = mean(price)), by = .(week_start, test)]
  y_list <- split(avg_price, avg_price$test)
  predicted_price <- data.table(sapply(
    y_list, function(x) predict_price_func = fit_func(dt_dcast_rhs, x$price, expr_weeks)))
  predicted_price[, week_start := all_weeks]
  predicted_price <- melt(predicted_price, id.vars = 'week_start',
                          measure.vars = c('Hi-Lo', 'Control', 'EDLP'),
                          value.name = 'price', variable.name = 'test')
  predicted_price[, type := 'predicted']
  avg_price <- avg_price[, test := as.character(test)
                         ][, type := 'original']
  avg_price <- rbind(predicted_price, avg_price)

  vline_dt <- data.table(date = as.numeric(as.Date(predicted_range)),
                         prediction = rep('predicted_time_frame', 2))
  g <- ggplot(avg_price, aes(x = week_start, y = price)) +
    geom_line(aes(color = test, linetype = type))  +
    geom_vline(data = vline_dt,
               aes(xintercept = date),
               alpha = 0.5, size = 1.5) +
    guides(fill=guide_legend(ncol=2)) +
    theme(legend.position = 'bottom') +
    labs(title = category$Category)

  avg_price_diff <- dcast(avg_price, week_start + test ~ type, value.var = 'price')

  avg_price_diff[, predict_diff := predicted - original]
  avg_price_diff[, mean(predict_diff^2), by = test]

  error_sd <- avg_price_diff[expr_weeks == FALSE, sd(predict_diff)]
  avg_price_diff[, z_test := pnorm(-abs((predict_diff/error_sd)))]
  ggplot(avg_price_diff, aes(x = week_start, y = z_test)) +
    geom_line(aes(color = test))
}

