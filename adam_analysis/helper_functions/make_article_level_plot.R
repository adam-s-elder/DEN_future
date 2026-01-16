## Contacts Reached Per Case Assigned
make_article_level_plot <- function(dataf, variable, title) {
  monthly_cr_per_ca <- dataf |>
    mutate(count = cases_interviewed) |>
    filter(param_name %in% variable) |>
    group_by(param_name, stargel_param, date) |>
    filter(!is.na(count)) |>
    summarise(
      param_value = sum(param_value * count, na.rm = TRUE) /
        sum(count),
    ) |>
    ungroup()

  contacts_per_case_assigned <- dataf |>
    mutate(count = cases_interviewed) |>
    filter(param_name %in% variable) |>
    ggplot(aes(y = param_value, x = date, color = stargel_param)) +
    geom_point(aes(size = sqrt(count))) +
    scale_size_continuous(
      "Sample Size",
      breaks = c(10, 100, sqrt(1000000)),
      labels = c(10, 100, sqrt(1000000))**2 |>
        scales::label_comma()()
    ) +
    scale_alpha_continuous(range = c(0.6, 1), guide = NULL) +
    scale_x_date(date_breaks = "month", labels = scales::label_date("%b\n%Y")) +
    geom_line(
      data = monthly_cr_per_ca,
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
    theme_altair() +
    ggtitle(title) +
    theme(axis.title = element_blank(), legend.position = "bottom")
}
