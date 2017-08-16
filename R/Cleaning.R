IsOutlier <- function(x, threshold_bottom = 0.35, threshold_top = 2.5){
  is_outlier = rep(FALSE, times = length(x))
  median_x = median(x, na.rm = TRUE)
  is_outlier[x/median_x < threshold_bottom | x/median_x > threshold_top] = TRUE
  return(is_outlier)
}

#' Clean the price movement function
#'
#' @param move a data.table that includes 'price', 'move'(unit sold),
#'  'qty', 'upc', 'week'(id)
#' @param week_table a data.table that includes 'week'(id), and 'week_start'(exact date)
#'   and 'holiday'
#' @param category a list of category info
#' @param price_expr a data.table that includes the experiment labels for of some categories
#'   it also includes the zone informotion.
CleanMove <- function(move, week_table, category, price_expr,
                      strict_filtering = TRUE){
  setnames(move, names(move), tolower(names(move)))
  move[, price := price/qty]
  # summarize the product and select the products that cover top 90% of the revenue
  upc_summary <- move[, list(revenue = sum(move * price, na.rm = TRUE),
                             first_week = min(week, na.rm = TRUE),
                             last_week = max(week, na.rm = TRUE)),
                      by = upc]
  upc_summary[, revenue := revenue/(last_week - first_week)]
  upc_summary <- upc_summary[order(-revenue)][, revenue_rank := 1:.N]
  upc_summary[, cum_coverage := cumsum(revenue)]
  upc_summary[, cum_coverage := cum_coverage/max(cum_coverage)]
  upc_summary <- upc_summary[cum_coverage < .9]
  move <- merge(move, upc_summary[, .(upc)], by = 'upc')
  move_summary <- move[, list(count = sum(move * qty, na.rm = TRUE)/CountUnique(store)),
                       by = upc]
  move_summary <- move_summary[order(-count)]
  move <- merge(move, move_summary[, .(upc)], by = 'upc')
  if(nrow(move) == 0) return(NULL)
  # generate a upc-store-week data data.table
  week_list <- intersect(unique(week_table$week), unique(move$week))
  store_list <- unique(move$store)
  upc_list <- unique(move$upc)
  upc_store_week <- data.table(expand.grid(upc_list, store_list, week_list))
  setnames(upc_store_week, names(upc_store_week), c('upc', 'store', 'week'))
  # all missing price or 0 price will be denoted as NA
  move <- move[price != 0]
  move <- merge(move, upc_store_week, by = c('upc', 'store', 'week'), all.y = TRUE)
  rm(upc_store_week)
  # exclude upc-week pair if all of its pirces are NA
  move <- move[, count := sum(is.na(price))/.N, by = .(upc, week)][count < 1]
  move <- merge(price_expr[, .(store, zone)], move, by = 'store')
  # count the number of unique prices per upc-zone-week
  move[, tmp_key := .GRP, by = .(upc, zone, week)]
  setkey(move, tmp_key)
  move[, zone_n_price := CountUniqueC(price, tmp_key)]
  move[, zone_avg_price := mean(price, na.rm = TRUE), by = tmp_key]
  move[is.na(price), ok := 0]
  # if there is only one unique price, impute the NA with the zone-average price
  move[zone_n_price == 1, price := zone_avg_price]
  # remove upc if there are still 30% of the price missing
  move <- move[, count := mean(is.na(price)), by = upc][count < 0.3]
  # remove a store if there are still 10% of the price missing
  move <- move[, count := mean(is.na(price)), by = .(store)][count < 0.15]
  # fill in the rest with zone average price
  move[is.na(price), price := zone_avg_price]
  # there are zone-upc-week that are all NA, fill those with upc-week average prices
  move[, zone_all_price := mean(price, na.rm = TRUE), by = .(week, upc)]
  move[is.na(price), price := zone_all_price]
  # remove irrelavant columns
  move[, c('qty', 'sale', 'count', 'tmp_key',
           'zone_avg_price', 'zone_all_price') := NULL]
  # select upcs that have maximum number of weeks
  # TODO: check its filtering impact

  move <- move[!is.na(price)]
  move[, is_outlier := IsOutlier(price), by = .(upc, store)]
  move <- move[is_outlier == FALSE][, is_outlier := NULL]
  previous_row <- nrow(move)
  if(strict_filtering){
    move <- move[, count := .N, by = upc][count == max(count)][, count := NULL]
  } else {
    move <- move[, count := CountUnique(week), by = upc
                 ][count > max(count) * 0.9]
    move <- move[, count := .N, by = week
                 ][count == max(count)][, count := NULL]
  }

  VerboseWarning(nrow(move)/previous_row)
  move[is.na(move), move := 0]
  # merge week and holiday info
  move <- merge(move, week_table[, .(week, week_start, holiday)], by = 'week')
  # exclude holiday
  # move <- move[holiday == '']
  test_col <- paste0(category$data_abbrev, 'test')
  # remove stores whose label is not clear
  move <- merge(move, price_expr[, .SD, .SDcols = c('store', test_col)],
                by = 'store')
  setnames(move, test_col , 'test')
  move <- move[test != '']
  # calculated the smoothed price that is convinient for normalization
  setkey(move, upc, store, week)
  smoothed_price <- move[, ksmooth(week, price, x.points = week,
                                   bandwidth = 5),
                         by = .(upc, store)]
  setnames(smoothed_price, c('x', 'y'), c('week', 'smoothed_price'))
  move <- merge(move, smoothed_price, by = c('upc', 'store', 'week'))
  move[, test := factor(test, levels = unique(test))]
  return(move)
}

#' this fucntion takes load a category data and performs basic cleaning
BasicCleaning <- function(category){
  print(category$Category)
  dt <- LoadData(paste0('data/move/', category$movement, '.RData'), 'cur_data')
  basket <- LoadData(paste0('data/upc/', category$upc, '.RData'), 'cur_data')
  if(is.null(dt)) return(data.table())
  dt <- CleanMove(dt, WEEK_TABLE, category, PRICE_EXPR, strict_filtering = FALSE)
  if(is.null(dt)) return(data.table())
  return(dt)
}

GetCategoryPrice <- function(category){
  print(category$Category)
  dt <- LoadData(paste0('data/move/', category$movement, '.RData'), 'cur_data')
  basket <- LoadData(paste0('data/upc/', category$upc, '.RData'), 'cur_data')
  if(is.null(dt)) return(data.table())
  dt <- CleanMove(dt, WEEK_TABLE, category, PRICE_EXPR, strict_filtering = FALSE)
  if(is.null(dt)) return(data.table())
  revenue <- dt[, .(weight = sum(move * price)), by = .(upc, store)]
  dt <- merge(dt, revenue, by = c('upc', 'store'))
  category_price <- dt[, .(price = wtd.mean(price, weight)),
                       by = .(store, week_start)]
  category_price[, category := category$Category]
  return(category_price)
}


PriceDispersionFtest <- function(dt, p = 'price', t = 'week'){
  dt <- copy(dt)
  dt[, d_p := get(p) - mean(get(p), na.rm = TRUE), by = get(t)]
  y = dt[, d_p]
  result = dt[, list(F_test = var.test(d_p, y)$p.value,
                     alternative = "less"), by = get(t)]
}




AddMultiColumn <- function(input, cgroup, n.cgroup, cgroup.just,
                           keyword = 'Category', hline = TRUE, line_below = -1){
  insert_line_number <- grep(keyword, input)
  insert_content <- sprintf('\\multicolumn{%d}{%s}{%s}', n.cgroup, cgroup.just, cgroup)
  insert_content <- paste0(paste(insert_content, collapse = '&'), '\\tabularnewline')
  if(hline){
    output <- c(input[1:insert_line_number + line_below],
              insert_content,
              '\\hline',
              input[-(1:insert_line_number + line_below)])
  } else {
    output <- c(input[1:insert_line_number + line_below],
              insert_content,
              input[-(1:insert_line_number + line_below)])
  }

}


