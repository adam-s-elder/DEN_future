fix_split_list <- function(split_l) {
  unique_orders <- split_l |> map(function(x) {
    x[-(1:3)] |> trimws()
  }) |> unique()
  location_info_df <- unique_orders |>
    map(.f = order_to_df) |> do.call(what = bind_rows)
  return_l <- split_l |> map(.f = function(x) {
    ifelse(length(x) > 3, x[4], NA)
  })
  new_order <- map2(.x = split_l, .y = return_l, .f = function(x, y) {
    return(c(x[1:3], y[1]))
  })
  problems <- which(is.na(return_l |> do.call(what = c)))
  return(list(new_split = new_order, problem_idx = problems,
              location_df = location_info_df))
}