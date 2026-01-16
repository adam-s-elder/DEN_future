calc_state_level_cases <- function(state_l, start_date, end_date, case_data) {
  count <- case_data |>
    filter(
      state == state_l,
      date >= start_date,
      date <= end_date
    ) |>
    pull(day_cases) |>
    sum(na.rm = TRUE)
  return(count)
}
