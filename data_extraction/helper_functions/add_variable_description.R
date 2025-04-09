# Add descriptions of each variable to go along with tag names
#
#

add_variable_description <- function(param_df, description_df) {
  init_names <- description_df$Name
  type_vals <- c("mean", "med", "mode", "count", "max")
  split_names <-
    init_names |> str_replace("\\]", "") |> str_split(pattern = "\\[")
  expanded_names <- split_names |> map(.f = function(x) {
    if (length(x) == 1) {
      if (grepl("hd_", x)) {
        return(paste0(x, c("_mean", "_median", "_hdcount")))
      } else {
        return(x)
      }
    } else if (length(x) == 2) {
      if (x[2] == "type") {
        return(paste0(x[1], type_vals))
      } else if (x[2] == "count|perc") {
        return(paste0(x[1], c("count", "perc")))
      } else if (grepl("type_", x[2])) {
        suffix <- x[2] |> gsub(pattern = "type_", replacement = "_")
        return(paste0(x[1], type_vals, suffix))
      } else {
        browser()
      }
    }
  })
  new_name_df <- list(init_names, expanded_names) |>
    list_transpose(simplify = FALSE) |>
    map(.f = function(x) {
      return(data.frame("Name" = x[[1]], "param_name" = x[[2]]))
    }) |> do.call(what = bind_rows)
  hd_df <- description_df |> filter(grepl("hd_", Name))

  expanded_descr <- hd_df$Desc |> map(.f = function(x) {
    descr <- x |> str_split("(\\%|\\#) of")
    metric_type <- x |> str_extract("\\%|\\#")
    paste0(c("Mean ", "Median ", "Number of health departments reporting "),
             paste0(metric_type, " of", descr[[1]][2]))
  })
  expanded_name <- hd_df$Name |> map(.f = function(x) {
    paste0(x, c("_mean", "_median", "_hdcount"))
  })
  expanded_hd_df <-
    suppressMessages(list(expanded_name, expanded_descr) |> list_transpose() |>
    map(bind_cols)) |> bind_rows()
  colnames(expanded_hd_df) <- c("Name", "Desc")
  clean_desc_df <- left_join(new_name_df |> filter(!grepl("hd_", Name)),
                             description_df |> filter(!grepl("hd_", Name)),
                             by = "Name")
  clean_descr_with_hd <-
    bind_rows(clean_desc_df |> select(param_name, Desc),
              expanded_hd_df |> select(param_name = Name, Desc))
  desc_df <- clean_descr_with_hd |> mutate(
    param_name = str_replace(pattern = "case_",
                             replacement = "cases_", param_name),
    param_name = str_replace(param_name, "contact_", "contacts_"),
    param_name = str_replace(param_name, "ave|avg", "mean"),
    param_name = str_replace(param_name, "total", "count")
  )
  updated_param_df <-
    param_df |> left_join(desc_df |> select(param_name, Desc),
                          by = "param_name")
  return(updated_param_df)
}
