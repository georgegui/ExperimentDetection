estimation_data <- merge(
  out$formatted_dt, 
  out$store_week_prediction[, .(store, week_start, pred_label)], 
  by = c('store', 'week_start'))
estimation_data[, demeaned_move := quantity_func(move) - mean(quantity_func(move)), 
                by = .(week_start, upc)]
estimation_data[, ':='(
  week_start = as.factor(week_start), 
  upc = as.factor(upc)
)]
if(!INCLUDE_ZERO_QUANTITY) estimation_data <- estimation_data[move > 0]

formula_list <- list(
  basic = 'quantity_func(move) ~ log(price)', 
  fe = 'quantity_func(move) ~ log(price)|week_start + upc', 
  fe_inter = 'quantity_func(move) ~ log(price)|week_start * upc'
)
n_expr_data <- estimation_data[, sum(pred_label != 'Control')]
n_round <- 100
documented_elas <- category_dt[Category == cur_category, rowMeans(.SD, na.rm = TRUE)/Price_Change, 
                               .SDcols = c('Documented_HiLo_Effect', 'Documented_EDLP_Effect_neg')]
for(cur_f in names(formula_list)){
  cur_formula <- formula(formula_list[[cur_f]])
  model_all <- felm(cur_formula, estimation_data)
  model_expr <- felm(cur_formula, estimation_data[pred_label != 'Control'])
  set.seed(5)
  random_guess_coefs <- rep(0, n_round)
  for(i in 1:n_round){
    sample_row <- sample(nrow(estimation_data), n_expr_data)
    model_guess_fe <- felm(cur_formula, 
                           estimation_data[sample_row])
    random_guess_coefs[[i]] <- tail(coef(model_guess_fe), 1)
  }
  random_guess_mse <- (random_guess_coefs - documented_elas)^2
  assign(paste0('model_all_', cur_f), model_all)
  assign(paste0('model_expr_', cur_f), model_expr)
  assign(paste0('random_guess_mse_', cur_f), random_guess_mse)
}
NULL
