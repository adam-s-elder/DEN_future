ten_param_starg_compare <- function(month_level_data) {
  monthly_averages_stargel <- month_level_data |>
    clean_elic_data() |>
    mutate(count = cases_interviewed) |>
    group_by(pm_start_date, simp_pname, stargel_param) |>
    summarise(
      tot_sources = n(),
      non_missing_sources = sum(!is.na(count * param_value)),
      overall_count = sum(count, na.rm = TRUE),
      inclusive_ave = sum(count * param_value, na.rm = TRUE) / overall_count,
    ) |>
    mutate(count = overall_count)

  monthly_averages_stargel |>
    mutate(
      simp_pname = case_when(
        simp_pname ==
          "cases_interviewed_perc" ~ "Percent of Assigned \nCases Interviewed",
        simp_pname ==
          "cases_reached_perc" ~ "Percent of Assigned \nCases Reached",
        simp_pname ==
          "contacts_reached_perc" ~ "Percent of Named \nContacts Reached",
        simp_pname ==
          "contacts_interviewed_perc" ~ "Percent of Named \nContacts Interviewed",
        simp_pname ==
          "percent_naming_contacts" ~ "Percent of Cases \nNaming Contacts",
        simp_pname ==
          "contacts_named_perc_household" ~ "Percent of Named \nContacts in Household",
        simp_pname ==
          "contacts_named_ratio" ~ "Contacts Named Per Case Interviewed",
        simp_pname ==
          "contacts_named_ratio_mean_household" ~ "Household Contacts Named \nPer Case Naming Contacts",
        simp_pname ==
          "contacts_reached_per_cases_assigned" ~ "Contacts Reached Per Case Assigned",
        simp_pname == "cases_interviewed_count" ~ "Cases Interviewed",
        TRUE ~ simp_pname
      )
    ) |>
    ggplot(aes(
      x = pm_start_date,
      y = inclusive_ave,
      color = stargel_param,
      group = stargel_param
    )) +
    geom_point(aes(size = sqrt(count))) +
    geom_line() +
    scale_size_continuous(
      "Sample Size",
      range = c(0.2, 4),
      breaks = c(10, 100, sqrt(1000000)),
      labels = c(10, 100, sqrt(1000000))**2 |>
        scales::label_comma()()
    ) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_color_manual(
      "Source",
      values = ggthemes::tableau_color_pal()(2),
      breaks = c("stargel", "non-stargel"),
      labels = c("Stargel Only", "Non Stargel")
    ) +
    facet_wrap(~simp_pname, scales = "free_y") +
    theme_altair() +
    ggthemes::scale_color_tableau() +
    theme(
      legend.position = "bottom",
      axis.title.x = element_blank()
    )
}
