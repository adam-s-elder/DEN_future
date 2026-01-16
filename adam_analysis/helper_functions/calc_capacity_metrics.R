calc_capacity_metrics <- function(wide_capacity_data, pop_counts, case_counts) {
  clean_wide_data <- wide_capacity_data |>
    filter(
      grepl("ENPA and ENCV metrics reported by States", source) |
        grepl("64 HD accross US and collonies", pm_location)
    ) |>
    mutate(bef_feb = pm_start_date < lubridate::mdy("02/01/2023")) |>
    filter(bef_feb) |>
    group_by(pm_location) |>
    summarise(
      pm_start_date = min(pm_start_date),
      pm_end_date = max(pm_end_date),
      self_report_codes_issued = sum(self_report_codes_issued, na.rm = TRUE),
      user_cases_notifying = sum(user_cases_notifying, na.rm = TRUE),
      num_users_count = sum(num_users_count, na.rm = TRUE),
      cases_named_contacts_count = sum(
        cases_named_contacts_count,
        na.rm = TRUE
      ),
      contacts_reached_count = sum(contacts_reached_count, na.rm = TRUE)
    ) |>
    mutate(
      state = ifelse(grepl("US", pm_location), "US", pm_location)
    )
  comb_data <- clean_wide_data |>
    left_join(
      pop_counts |> select(state = abrv, pop_size = population),
      by = "state"
    )

  for (st_idx in 1:nrow(comb_data)) {
    comb_data[st_idx, "case_count"] <- calc_state_level_cases(
      comb_data[st_idx, "state", drop = TRUE],
      comb_data[st_idx, "pm_start_date", drop = TRUE],
      comb_data[st_idx, "pm_end_date", drop = TRUE],
      case_counts
    )
  }
  table_data <- comb_data |>
    mutate(
      addoption_rate_perc = 100 * num_users_count / pop_size,
      cont_notified_per_case = contacts_reached_count / case_count,
      adj_cont_notified_per_case = 100 *
        contacts_reached_count /
        case_count /
        addoption_rate_perc,
      ar_rounded = round(addoption_rate_perc),
      round_cnpc = round(cont_notified_per_case, 2),
      round_acnpc = round(adj_cont_notified_per_case, 2),
      cases_tho = round(case_count / 1000),
      users_tho = round(num_users_count / 1000),
      pop_tho = round(pop_size / 1000),
      contacts_reached_tho = round(contacts_reached_count / 1000),
    ) |>
    left_join(
      state_initials() |> select(state = abrv, location = state),
      by = "state"
    ) |>
    select(
      location,
      pm_start_date,
      pm_end_date,
      pop_tho,
      users_tho,
      ar_rounded,
      cases_tho,
      contacts_reached_tho,
      round_cnpc,
      round_acnpc
    )
  return(table_data)
}
