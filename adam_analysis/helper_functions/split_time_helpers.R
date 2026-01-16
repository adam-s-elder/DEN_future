make_param_ss_count_df <- function(long_param_df) {
  case_and_contact_counts <- long_param_df |>
    filter(
      param_name %in%
        c(
          "cases_reached_count",
          "cases_interviewed_count",
          "contacts_reached_count",
          "contacts_interviewed_count"
        )
    )
  cc_long <- case_and_contact_counts |>
    pivot_wider(
      id_cols = c("source", "pm_start_date"),
      names_from = param_name,
      values_from = param_value
    )
  case_ci_ratio <- cc_long |>
    filter(!is.na(cases_reached_count), !is.na(cases_interviewed_count)) |>
    summarise(
      total_reached = sum(cases_reached_count),
      total_interviewed = sum(cases_interviewed_count)
    ) |>
    mutate(ratio = total_interviewed / total_reached) |>
    pull(ratio)
  contact_ci_ratio <- cc_long |>
    filter(
      !is.na(contacts_reached_count),
      !is.na(contacts_interviewed_count)
    ) |>
    summarise(
      total_reached = sum(contacts_reached_count),
      total_interviewed = sum(contacts_interviewed_count)
    ) |>
    mutate(ratio = total_interviewed / total_reached) |>
    pull(ratio)
  count_df <- cc_long |>
    mutate(
      cases_interviewed = ifelse(
        is.na(cases_interviewed_count),
        cases_reached_count * case_ci_ratio,
        cases_interviewed_count
      ),
      contacts_interviewed = ifelse(
        is.na(contacts_interviewed_count),
        contacts_reached_count * contact_ci_ratio,
        contacts_interviewed_count
      ),
    ) |>
    select(source, cases_interviewed, contacts_interviewed, pm_start_date)
  return(count_df)
}
