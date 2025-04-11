# parse_params is responsible for splitting apart all the data
# encoded into the form {{param_value, start_date, end_date, location}} into
# four different data elements.

parse_params <- function(str_vector) {
  split_list <- str_split(str_vector, pattern = ",")
  non4split_length <-
    which((split_list |> map(length) |> do.call(what = c)) != 4)
  split_list_fix <- fix_split_list(split_list[non4split_length])
  split_list[non4split_length] <- split_list_fix$new_split
  failed_parse_idx <- non4split_length[split_list_fix$problem_idx]
  split_param_df <- list_transpose(split_list) |>
    map(trimws) |> as.data.frame()
  colnames(split_param_df) <- param_order
  if (length(failed_parse_idx) > 0) {
    warning("Some parameters were not coded properly and have an incorrect format: \n",
            paste0(do.call(split_list[failed_parse_idx] |>
                             map(.f = function(x) paste0(x, collapse = ", ")),
                           what = c), collapse = "\n"),
            "\nCheck these rows: ",
            paste0(failed_parse_idx, collapse = ", "))
    split_list <- split_list[-failed_parse_idx]
    na_mat <- matrix(NA, nrow = length(failed_parse_idx), ncol = 4)
    na_df <- as.data.frame(na_mat)
    colnames(na_df) <- colnames(split_param_df)
  } else {
    na_df <- NULL
  }
  split_param_df <- bind_rows(split_param_df, na_df)
  base_idx <- 1:length(str_vector)
  fix_idx <- c(setdiff(base_idx, failed_parse_idx), failed_parse_idx)
  return(list(
    location_data_frame = split_list_fix$location_df,
    param_df = split_param_df[match(base_idx, fix_idx), ]))
}
