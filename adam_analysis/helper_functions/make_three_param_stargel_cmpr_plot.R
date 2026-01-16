make_three_param_stargel_cmpr_plot <- function(month_level_elicit_data) {
  ap_el_plot_data <- month_level_elicit_data |>
    mutate(
      count = ifelse(
        param_name %in% "contacts_reached_perc",
        contacts_interviewed,
        cases_interviewed
      ),
      f_param_name = case_when(
        param_name %in% "cases_interviewed_perc" ~
          "Percent of Cases Interviewed",
        param_name %in% "percent_naming_contacts" ~
          "Percent of Those Interviewed that Named Contacts",
        param_name %in% "contacts_reached_perc" ~
          "Percent of Named Contacts Eventually Reached"
      )
    )

  monthly_aves <- ap_el_plot_data |>
    mutate(
      count = ifelse(
        param_name %in% "contacts_reached_perc",
        contacts_interviewed,
        cases_interviewed
      )
    ) |>
    filter(
      param_name %in%
        c(
          "cases_interviewed_perc",
          "percent_naming_contacts",
          "contacts_reached_perc"
        )
    ) |>
    group_by(f_param_name, stargel_param, date) |>
    filter(!is.na(count)) |>
    summarise(
      param_value = sum(param_value * count, na.rm = TRUE) /
        sum(count),
    ) |>
    ungroup()

  three_param_details <- ap_el_plot_data |>
    filter(
      param_name %in%
        c(
          "cases_interviewed_perc",
          "percent_naming_contacts",
          "contacts_reached_perc"
        )
    ) |>
    ggplot(aes(y = param_value, x = date, color = stargel_param)) +
    geom_point(aes(size = sqrt(count)), alpha = 0.5) +
    scale_size_continuous(
      "Sample Size",
      breaks = c(10, 100, sqrt(1000000)),
      labels = c(10, 100, sqrt(1000000))**2 |>
        scales::label_comma()()
    ) +
    scale_alpha_continuous(range = c(0.6, 1), guide = NULL) +
    scale_x_date(date_breaks = "month", labels = scales::label_date("%b\n%Y")) +
    geom_line(
      data = monthly_aves,
      aes(linetype = stargel_param, group = stargel_param),
      size = 0.5
    ) +
    scale_color_manual(
      "Source",
      values = ggthemes::tableau_color_pal()(2),
      breaks = c("stargel", "non-stargel"),
      labels = c("Stargel Only", "Non Stargel")
    ) +
    scale_linetype_manual(
      "Source",
      values = c(1, 2),
      breaks = c("stargel", "non-stargel"),
      labels = c("Stargel Only", "Non Stargel")
    ) +
    scale_y_continuous(expand = c(0, 0.1), limits = c(0, 100)) +
    theme_altair() +
    theme(
      axis.title = element_blank(),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    facet_wrap(~f_param_name, ncol = 1)
  return(three_param_details)
}
