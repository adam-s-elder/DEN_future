# parse_params is responsible for splitting apart all the data
# encoded into the form {{param_value, start_date, end_date, location}} into
# four different data elements.

parse_params <- function(str_vector, param_name, source_name, pname_order) {
  lengths_equal <- (length(str_vector) == length(param_name)) &
    (length(str_vector) == length(source_name))
  if (!lengths_equal) stop("The lengths of the three passed vectors do not match.")
  split_list <- str_split(str_vector, pattern = ",")
  non4split_length <-
    which((split_list |> map(length) |> do.call(what = c)) != 4)
  split_list_fix <- fix_split_list(split_list[non4split_length])
  split_list[non4split_length] <- split_list_fix$new_split
  failed_parse_idx <- non4split_length[split_list_fix$problem_idx]
  split_param_df <- list_transpose(split_list) |>
    map(trimws) |> as.data.frame()
  split_param_df$param_name <- param_name
  split_param_df$source <- source_name
  split_param_df$comment_string <- str_vector
  colnames(split_param_df) <- c(
    pname_order, "param_name", "source", "comment_string")
  if (length(failed_parse_idx) > 0) {
    warning("Some parameters were not coded properly and have an incorrect format: \n",
            paste0(do.call(split_list[failed_parse_idx] |>
                             map(.f = function(x) paste0(x, collapse = ", ")),
                           what = c), collapse = "\n"),
            "\nCheck these rows: ",
            paste0(failed_parse_idx, collapse = ", "))
  }
  return(list(
    location_data_frame = split_list_fix$location_df,
    param_df = split_param_df))
}
