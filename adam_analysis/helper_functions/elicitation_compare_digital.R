elicitation_compare_digital <- function(comb_data) {
  pop_data <- construct_us_population_counts(
    "../../data_extraction/datasets/baselines/NST-EST2024-POP.xlsx",
    state_initials()
  )

  us_case_counts <- construct_us_case_counts(
    "../../data_extraction/datasets/baselines/weekly_metrics_by_state.csv"
  )

  capcity_params <- c(
    "self_report_codes_issued",
    "user_cases_notifying",
    "notifications_recieved_total",
    "num_users_count",
    "notifications_recieved",
    "cases_named_contacts_count",
    "contacts_reached_count",
    "contacts_named_ratio_mean_cases_naming"
  )

  wide_capacity_data <- make_wide_data(comb_data, capcity_params)

  # Next we need to get the case count and population data cleaned.
  # It should match the format of the combined data
  # State, pm_start_date, pm_end_date, param_name, param_value

  ec_wide_data <- wide_capacity_data |>
    filter(pm_start_date < lubridate::mdy("02/01/2023"))

  ## For combined data we want to keep:
  ## Param value will include number of users and notifications as well as
  ## Number of keys uploaded since these are the
  ## primary variables of interest.

  ## Rate (contacts reached per case naming)
  ## ENPA / ENCV data:
  ##     contacts_named_ratio_mean_cases_naming =
  ##           contacts_reached_count / cases_named_contacts_count
  ec_wide_clean <- ec_wide_data |>
    mutate(state = ifelse(grepl("US", pm_location), "US", pm_location)) |>
    left_join(
      pop_data |> select(state = abrv, pop_size = population),
      by = "state"
    )

  for (st_idx in 1:nrow(ec_wide_clean)) {
    ec_wide_clean[st_idx, "case_count"] <- calc_state_level_cases(
      ec_wide_clean[st_idx, "state", drop = TRUE],
      ec_wide_clean[st_idx, "pm_start_date", drop = TRUE],
      ec_wide_clean[st_idx, "pm_end_date", drop = TRUE],
      us_case_counts
    )
  }

  ec_wide_plot <- ec_wide_clean |>
    mutate(
      contacts_named_ratio_mean_cases_naming = ifelse(
        is.na(contacts_named_ratio_mean_cases_naming),
        contacts_reached_count / cases_named_contacts_count,
        contacts_named_ratio_mean_cases_naming
      ),
      addoption_rate_perc = 100 * num_users_count / pop_size,
      cont_notified_per_case = contacts_reached_count / case_count,
      adj_cont_notified_per_case = 100 *
        contacts_reached_count /
        case_count /
        addoption_rate_perc,
    )

  ec_wide_plot |>
    ggplot(aes(
      xmin = pm_start_date,
      xmax = pm_end_date,
      y = contacts_named_ratio_mean_cases_naming,
      color = param_type
    )) +
    geom_linerange(aes(linewidth = log(contacts_reached_count))) +
    scale_x_date() +
    scale_linewidth_continuous(range = c(0.01, 2)) +
    ggthemes::scale_color_tableau() +
    guides(linewidth = "none", color = guide_legend(""))

  contacts_per_cases_naming_plot <- ec_wide_plot |>
    filter(
      contacts_named_ratio_mean_cases_naming < 20
    ) |>
    mutate(param_type = stringr::str_to_title(param_type)) |>
    ggplot(aes(
      y = contacts_named_ratio_mean_cases_naming,
      x = param_type,
      size = contacts_reached_count,
      color = param_type,
      group = param_type
    )) +
    ggbeeswarm::geom_beeswarm(cex = 2.5) +
    coord_flip() +
    scale_size_continuous(
      range = c(0.01, 2),
      transform = "sqrt",
      breaks = c(100, 10000, 1000000),
      labels = scales::label_comma()
    ) +
    scale_y_continuous(expand = c(0, 0.2)) +
    theme_altair() +
    theme(
      axis.title = element_blank(),
      legend.position.inside = c(1, 1),
      legend.justification.inside = c(1, 1),
      legend.title = element_text(hjust = 0.5)
    ) +
    guides(
      color = "none",
      size = guide_legend(
        "Contacts Reached (total)",
        position = "inside",
        nrow = 1
      )
    ) +
    ggthemes::scale_color_tableau() +
    ggtitle("Contacts Named per Case Notifying")

  contacts_notified_per_case <- ec_wide_plot |>
    ungroup() |>
    mutate(param_type = stringr::str_to_title(param_type)) |>
    filter(cont_notified_per_case > 0, cont_notified_per_case < 2) |>
    ggplot(aes(
      y = cont_notified_per_case,
      x = param_type,
      size = case_count,
      color = param_type,
      group = param_type
    )) +
    ggbeeswarm::geom_beeswarm(cex = 2.5) +
    coord_flip() +
    scale_size_continuous(
      range = c(0.01, 2),
      transform = "sqrt",
      breaks = c(100000, 1000000, 10000000),
      labels = scales::label_comma()
    ) +
    scale_y_continuous(expand = c(0, 0.2)) +
    theme_altair() +
    theme(
      axis.title = element_blank(),
      legend.position.inside = c(1, 1),
      legend.justification.inside = c(1, 1),
      legend.title = element_text(hjust = 0.5)
    ) +
    guides(
      color = "none",
      size = guide_legend(
        "Cases Reported to the CDC",
        position = "inside",
        nrow = 1
      )
    ) +
    ggthemes::scale_color_tableau() +
    ggtitle("Contacts Notified per Case Reported to the CDC")
  return_list <- list(
    contacts_per_cases_naming_plot = contacts_per_cases_naming_plot,
    contacts_notified_per_case = contacts_notified_per_case
  )
}
