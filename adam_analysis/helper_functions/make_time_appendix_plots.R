make_time_appendix_plots <- function(single_period_df, monthly_aves) {
  ct_plot <- single_period_df |>
    filter(param_value < 24 * 4.2) |>
    ggplot(aes(
      x = param_value / 24,
      y = as.POSIXct(date),
      size = count,
      shape = metric,
      fill = metric_color
    )) +
    scale_y_continuous(trans = rev_date) +
    scale_size_continuous(guide = NULL) +
    geom_point(aes(alpha = 1 - impt_type_alpha)) +
    scale_alpha_continuous(range = c(0.6, 1), guide = NULL) +
    scale_fill_manual("", values = ggthemes::tableau_color_pal()(3)) +
    # geom_line(aes(group = interaction(source, metric)),
    #           size = 0.2, alpha = 0.5, orientation = "y") +
    scale_shape_manual(values = c(21, 22), labels = c("Mean", "Median")) +
    xlim(c(0, NA)) +
    facet_wrap(~periods, nrow = 1) +
    guides(
      fill = guide_legend(override.aes = list(shape = 21, size = 4, alpha = 1))
    )
  theme(axis.title = element_blank(), legend.position = "bottom") +
    ggtitle("Timiliness of Contact Tracing Process (in days)")
  timeliness_two <- sp_times |>
    ggplot(aes(y = param_value / 24, x = date, color = metric)) +
    geom_point(aes(
      alpha = 1 - impt_type_alpha,
      size = sqrt(count),
      shape = impt_count
    )) +
    scale_alpha_continuous(range = c(0.6, 1), guide = NULL) +
    scale_x_date(
      date_breaks = "month",
      labels = scales::label_date("%b-%Y"),
      expand = c(0, 0)
    ) +
    geom_line(
      data = monthly_aves |>
        mutate(date = month_year, param_value = inclusive_ave),
      alpha = 0.5,
      orientation = "x",
      linewidth = 0.5,
      linetype = 2
    ) +
    geom_line(
      data = monthly_aves |>
        mutate(date = month_year, param_value = exclusive_ave),
      alpha = 0.5,
      orientation = "x",
      linewidth = 0.5
    ) +
    scale_size_continuous(
      "Sample Size",
      breaks = c(10, 100, sqrt(1000000)),
      labels = c(10, 100, sqrt(1000000))**2 |>
        scales::label_comma()()
    ) +
    # geom_vline(data = monthly_aves |>
    #              mutate(date = as.POSIXct(month_year)),
    #            aes(xintercept = date), alpha = 0.4) +
    scale_shape_manual(
      "",
      values = c(20, 1),
      labels = c("Count Reported", "Count not reported")
    ) +
    scale_color_manual(
      "",
      values = ggthemes::tableau_color_pal("Tableau 10")(2),
      breaks = c("med", "mean"),
      labels = c("Median", "Mean")
    ) +
    scale_y_continuous(expand = c(0, 0.1), limits = c(0, 4)) +
    theme_altair() +
    theme(
      axis.title = element_blank(),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    facet_wrap(~periods, ncol = 1) +
    ggtitle("Timiliness of Contact Tracing Process (in days)")
  return(list(timeliness1 = ct_plot, timeliness2 = timeliness_two))
}
