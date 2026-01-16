calc_monthly_aves <- function(data_frame) {
  monthly_averages <- data_frame |>
    group_by(month_year, simp_pname) |>
    summarise(
      num_sources = n(),
      overall_count = sum(count),
      inclusive_ave = sum(count * param_value) / overall_count,
      exclusive_ave = sum(incl * count * param_value) / sum(incl * count)
    ) |>
    mutate(count = overall_count)
  return(monthly_averages)
}
