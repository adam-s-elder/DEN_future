make_elicit_plot <- function(monthly_data, title_name) {
  var_names <- c(
    "cases_interviewed_perc",
    "percent_naming_contacts",
    "contacts_reached_perc"
  )
  sub_df <- monthly_data |> filter(simp_pname %in% var_names)
  sub_df |>
    ggplot(aes(
      y = inclusive_ave / 100,
      x = pm_start_date,
      color = simp_pname,
      group = simp_pname
    )) +
    # scale_size_continuous(guide = NULL, range = c(2, 8)) +
    geom_point() +
    geom_line() +
    scale_x_date(
      date_breaks = "2 months",
      date_minor_breaks = "month",
      labels = scales::label_date("%b\n%Y"),
      expand = c(0.025, 0.1)
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 1),
      labels = scales::label_percent()
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
    scale_color_manual(
      "",
      values = ggthemes::tableau_color_pal("Tableau 10")(3),
      breaks = c(
        "cases_interviewed_perc",
        "percent_naming_contacts",
        "contacts_reached_perc"
      ),
      labels = c(
        "Percent of Cases Interviewed",
        "Percent of Those Interviewed that Named Contacts",
        "Percent of Named Contacts Eventually Reached"
      )
    ) +
    guides(color = guide_legend(nrow = 3, position = "inside")) +
    ggtitle(title_name)
}
