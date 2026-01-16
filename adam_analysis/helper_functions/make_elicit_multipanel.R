# Creat 6 panel plot showing effects of strain (case assigned per ci per month
# on elicitation).

make_elicit_multipanel <- function(comb_data) {
  require(patchwork)
  compare_df <- comb_data |>
    pivot_wider(
      id_cols = c("pm_start_date", "source", "param_type", "pm_location"),
      names_from = "param_name",
      values_from = "param_value"
    ) |>
    mutate(
      cases_per_ci = cases_assigned_count / staff_hired_ci_count,
      contacts_per_ct = contacts_named_count / staff_hired_ct_count,
      prop_interviewed = cases_interviewed_count / cases_assigned_count,
      prop_cont_reached = contacts_reached_count / contacts_named_count,
    )

  perc_interviewed <- compare_df |>
    ggplot(aes(x = cases_per_ci, y = prop_interviewed)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    ggtitle(
      "Percent of Cases Interviewed (A)",
      subtitle = "Among Assigned Cases"
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 1),
      labels = scales::label_percent()
    ) +
    xlab("Cases Assigned Per Case Investigator") +
    theme_altair() +
    theme(
      axis.title.y = element_blank()
    )

  perc_named_contact <- compare_df |>
    ggplot(aes(x = cases_per_ci, y = hd_percent_naming_contacts_mean / 100)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    ggtitle(
      "Percent of Cases Naming Contacts (B)",
      subtitle = "Among Interviewed Cases"
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 1),
      labels = scales::label_percent()
    ) +
    xlab("Cases Assigned Per Case Investigator") +
    theme_altair() +
    theme(
      axis.title.y = element_blank()
    )

  num_named_contact <- compare_df |>
    ggplot(aes(
      x = cases_per_ci,
      y = hd_contacts_named_ratio_mean_cases_naming_mean
    )) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    ggtitle(
      "Mean Number of Contacts Named (C)",
      subtitle = "by Cases Naming Contacts"
    ) +
    scale_y_continuous(limits = c(0, NA)) +
    xlab("Cases Assigned Per Case Investigator") +
    theme_altair() +
    theme(
      axis.title.y = element_blank()
    )

  combined_metric <- compare_df |>
    filter(!is.na(cases_per_ci)) |>
    ggplot(aes(
      x = cases_per_ci,
      y = hd_percent_naming_contacts_mean /
        100 *
        prop_interviewed *
        hd_contacts_named_ratio_mean_cases_naming_mean
    )) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    ggtitle(
      "Mean Number of Contacts Named (A X B X C)",
      subtitle = "per Case Assigned"
    ) +
    scale_y_continuous(limits = c(0, NA)) +
    xlab("Cases Assigned Per Case Investigator") +
    theme_altair() +
    theme(
      axis.title.y = element_blank()
    )

  perc_cont_reach <- compare_df |>
    ggplot(aes(x = cases_per_ci, y = contacts_reached_perc / 100)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    ggtitle("Percent of Contacts Reached (D)", subtitle = "Among Those Named") +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 1),
      labels = scales::label_percent()
    ) +
    xlab("Cases Assigned Per Case Investigator") +
    theme_altair() +
    theme(
      axis.title.y = element_blank()
    )

  all_combined <- compare_df |>
    filter(!is.na(cases_per_ci)) |>
    ggplot(aes(x = cases_per_ci, y = contacts_reached_per_cases_assigned)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    ggtitle(
      "Mean Number of Contacts Reached (A X B X C X D)",
      subtitle = "per Case Assigned"
    ) +
    scale_y_continuous(limits = c(0, NA)) +
    xlab("Cases Assigned Per Case Investigator") +
    theme_altair() +
    theme(
      axis.title.y = element_blank()
    )

  fin_plot <- (perc_interviewed | perc_named_contact | num_named_contact) /
    (combined_metric | perc_cont_reach | all_combined)
  return(fin_plot)
}
