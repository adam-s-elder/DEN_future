# Add descriptions of each variable to go along with tag names
#
#

add_variable_description <- function(param_df, description_df) {
  init_names <- description_df$Name
  split_names <-
    init_names |> str_replace("\\]", "") |> str_split(pattern = "\\[")
  expanded_names <- split_names |> map(.f = function(x) {
    if (length(x) == 1) {
      return(x)
    } else {
      if (x[2] == "type") {
        return(paste0(x[1], c("mean", "med", "mode", "count")))
      } else if (x[2] == "count|perc") {
        return(paste0(x[1], c("count", "perc")))
      } else {
        browser()
      }
    }
  })
  new_name_df <- list(init_names, expanded_names) |>
    list_transpose(simplify = FALSE) |>
    map(.f = function(x) {
      return(data.frame("Name" = x[[1]], "name" = x[[2]]))
    }) |> do.call(what = bind_rows)
  clean_desc_df <- left_join(new_name_df, description_df, by = "Name")
  clean_desc_df <- clean_desc_df |> mutate(
    name = str_replace(pattern = "case_", replacement = "cases_", name),
    name = str_replace(name, "contact_", "contacts_"),
    name = str_replace(name, "ave|avg", "mean"),
    name = str_replace(name, "total", "count")
  )
  updated_param_df <-
    param_df |> left_join(clean_desc_df |> select(name, Desc), by = "name")
  return(updated_param_df)
}
