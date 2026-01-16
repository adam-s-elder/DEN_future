make_sv_plot <- function(monthly_data, variable_name, title_name) {
  var_names <- variable_name
  sub_df <- monthly_data |> filter(simp_pname %in% var_names)
  sub_df |>
    ggplot(aes(y = inclusive_ave, x = pm_start_date)) +
    # scale_size_continuous(guide = NULL, range = c(2, 8)) +
    geom_point() +
    geom_line() +
    scale_x_date(
      date_breaks = "2 months",
      date_minor_breaks = "month",
      labels = scales::label_date("%b\n%Y"),
      expand = c(0.025, 0.1)
    ) +
    theme_altair() +
    theme(
      axis.title = element_blank(),
      legend.margin = margin(0, 0, 0, 0), # turned off for alignment
      legend.justification.top = "left",
      legend.justification.left = "top",
      legend.justification.bottom = "right",
      legend.justification.inside = c(0.95, 1),
      legend.location = "plot",
      legend.box = "horizontal",
      legend.direction = "horizontal",
      panel.grid.minor.x = element_line(color = "lightgray", linewidth = 0.25)
    ) +
    guides(color = guide_legend(nrow = 3, position = "inside")) +
    ggtitle(title_name)
}
