# Add Variable Sample Size

add_var_sample_size <- function(orig_data, count_data) {
  final_df <- orig_data |>
    mutate(pm_start_date = lubridate::mdy(pm_start_date)) |>
    left_join(count_data |>
                mutate(pm_start_date = lubridate::mdy(pm_start_date)),
              by = c("source", "pm_start_date"))
  labeled_data <- final_df |> mutate(
    date = pm_start_date,
    digital = ifelse(grepl("digital", source), "digital", "manual"),
    pm_location = gsub(pattern = "communities with higher than average COVID-19 incidence rates", "", pm_location),
    comb_loc_date = factor(
      x = paste0("(", digital, ") ", pm_location, ",", date),
      levels = paste0("(", digital, ") ", pm_location, ",", date),
      labels = paste0("(", digital, ") ", pm_location, ", ", date)))
  return(labeled_data)
}
