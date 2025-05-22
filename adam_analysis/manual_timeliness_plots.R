# Single period time plots
source("helper_functions/plotting_functions.R")
all_period_dfs <- read_csv("intermediate_data/all_period_times.csv")
impt_df <- read_csv("intermediate_data/props_for_impts.csv")
sp_times <- read_csv("intermediate_data/single_period_times.csv")
monthly_aves <- read_csv("intermediate_data/monthly_averages.csv")


## Calculating Symtpom to Test times
sympt_times <-
  all_period_dfs |>
  filter(
    periods %in% c("A", "B", "ABC", "AB", "BC"),
    !(impt_type %in% c("outer_perc", "outer_abs"))
    )

sp_times <- sp_times |> mutate(
  periods = as.factor(periods),
  periods = forcats::fct_recode(periods, !!!periods_to_label),
  periods = factor(x = periods,
                       levels = names(periods_to_label),
                       labels = names(periods_to_label)))

monthly_aves <- monthly_aves |> mutate(
  periods = as.factor(periods),
  periods = forcats::fct_recode(periods, !!!periods_to_label),
  periods = factor(x = periods,
                       levels = names(periods_to_label),
                       labels = names(periods_to_label)))

sp_times |>
  ggplot(aes(x = param_value / 24, y = as.POSIXct(date), size = count,
             shape = metric, color = metric_color, fill = metric_color)) +
  scale_y_continuous(trans = rev_date) +
  scale_size_continuous(guide = NULL) +
  geom_point(aes(alpha = 1 - impt_type_alpha), color = "black") +
  scale_alpha_continuous(range = c(0.6, 1), guide = NULL) +
  # geom_line(aes(group = interaction(source, metric)),
  #           size = 0.2, alpha = 0.5, orientation = "y") +
  scale_shape_manual(values = c(21, 22), labels = c("Mean", "Median")) +
  xlim(c(0, NA)) +
  facet_wrap(~periods, nrow = 1) +
  theme(axis.title = element_blank(),
        legend.position = "bottom") +
  ggtitle("Timiliness of Contact Tracing Process (in days)")

# Main text single period-plots

single_period_plot <- function(sp_val) {
  sub_df <- monthly_aves |> filter(periods == sp_val) |>
    mutate(
      has_sample_size = !is.na(exclusive_ave),
      average = ifelse(has_sample_size,
                       inclusive_ave,
                       exclusive_ave),
      sample_size = ifelse(has_sample_size, "SS available", "SS unavailable"),
      count = ifelse(has_sample_size, overall_count, 1))
  overall_ave <- sub_df |> group_by(metric) |>
    summarise(exclusive_ave =
                sum(exclusive_ave * overall_count, na.rm = TRUE) /
                sum(overall_count * !is.na(exclusive_ave)))
  sub_df |>
    ggplot(aes(y = inclusive_ave / 24,
               x = month_year, shape = has_sample_size,
               color = metric)) +
    scale_size_continuous(guide = NULL, range = c(2, 8)) +
    scale_alpha_continuous(range = c(0.6, 1), guide = NULL) +
    geom_point(aes(size = count)) +
    scale_shape_manual("", values = c(1, 20),
                       labels = c("Sample Size Unavailable",
                                  "Sample Size Available")) +
    geom_hline(data = overall_ave,
               aes(yintercept = exclusive_ave / 24,
                   color = metric)) +
    scale_x_date(date_breaks = "2 months",
                 date_minor_breaks = "month",
                 labels = scales::label_date("%b-%Y"),
                 expand = c(0.025, 0.1)) +
    scale_y_continuous(expand = c(0, 0.1), limits = c(0, NA)) +
    theme_altair() +
    theme(axis.title = element_blank(),
          legend.position = c(1, 0.05),
          legend.box = "horizontal",
          legend.direction = "horizontal",
          legend.justification = "right",
          panel.grid.minor.x = element_line(color = "lightgray",
                                            linewidth = 0.25)) +
    scale_color_manual("", values =
                         ggthemes::tableau_color_pal("Tableau 10")(2),
                       breaks = c("med", "mean"),
                       labels = c("Median", "Mean")) +
    guides(color = guide_legend(nrow = 1),
           shape = guide_legend(nrow = 1)) +
    ggtitle(paste0("Time from ", sp_val))
}

single_period_plot("Symptoms to Test")
single_period_plot("Test to Result")
single_period_plot("Result to Case Notified") + ylim(0, 4)
single_period_plot("Contact Named to Reached") + theme(
  legend.position = c(1, 1)
)



# Appendix Plot
sp_times |>
  ggplot(aes(y = param_value / 24, x = date,
             size = count, color = metric)) +
  scale_size_continuous(guide = NULL) +
  geom_point(aes(alpha = 1 - impt_type_alpha, shape = impt_count)) +
  scale_alpha_continuous(range = c(0.6, 1), guide = NULL) +
  scale_x_date(date_breaks = "month",
               labels = scales::label_date("%b-%Y"),
               expand = c(0, 0)) +
  geom_line(data = monthly_aves |>
              mutate(date = month_year,
                     param_value = inclusive_ave),
            alpha = 0.5, orientation = "x", size = 0.5, linetype = 2) +
  geom_line(data = monthly_aves |>
              mutate(date = month_year,
                     param_value = exclusive_ave),
            alpha = 0.5, orientation = "x", size = 0.5) +
  # geom_vline(data = monthly_aves |>
  #              mutate(date = as.POSIXct(month_year)),
  #            aes(xintercept = date), alpha = 0.4) +
  scale_shape_manual("", values = c(1, 20),
                     labels = c("Count Reported", "Count not reported")) +
  scale_y_continuous(expand = c(0, 0.1), limits = c(0, 4)) +
  theme_altair() +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  facet_wrap(~ periods, ncol = 1) +
  ggtitle("Timiliness of Contact Tracing Process (in days)") +
  ggthemes::scale_color_tableau()

## Stacked Plot
monthly_wide <- monthly_aves |> select(
 month_year, metric, inclusive_ave, periods
) |> pivot_wider(
  id_cols = c("month_year", "metric"),
  values_from = inclusive_ave, names_from = periods)

date_metric_missing <- monthly_wide |> pivot_longer(
  cols = setdiff(colnames(monthly_wide), c("month_year", "metric")),
  names_to = "periods", values_to = "average")

monthly_wide_filled <- monthly_wide |> mutate(
  `Contact Named to Reached` = ifelse(
    is.na(`Contact Named to Reached`),
    impt_period_lens |> filter(periods == "D") |> pull(ave_time),
    `Contact Named to Reached`),
  `Result to Case Notified` = ifelse(
    is.na(`Result to Case Notified`),
    impt_period_lens |> filter(periods == "C") |> pull(ave_time),
    `Result to Case Notified`),
  `Test to Result` = ifelse(
    is.na(`Test to Result`),
    impt_period_lens |> filter(periods == "B") |> pull(ave_time),
    `Test to Result`),
  `Symptoms to Test` = ifelse(
    is.na(`Symptoms to Test`),
    impt_period_lens |> filter(periods == "A") |> pull(ave_time),
    `Symptoms to Test`),
)

long_monthly_with_impt <- monthly_wide_filled |>
pivot_longer(
  cols = setdiff(colnames(monthly_wide), c("month_year", "metric")),
  names_to = "periods", values_to = "average") |>
left_join(
date_metric_missing |> mutate(imputed = is.na(average)) |>
  select(-average), by = c("month_year", "metric", "periods")) |>
  mutate(periods = factor(periods,
                          levels = names(periods_to_label),
                          labels = names(periods_to_label)))

long_monthly_with_impt |>
  mutate(imputed_alpha = 1 - as.numeric(imputed),
         metric = ifelse(metric == "mean", "Mean", "Median")) |>
  ggplot(aes(y = average / 24, x = month_year,
             fill = forcats::fct_rev(periods))) +
  geom_col(position = position_stack(),
           aes(alpha = imputed_alpha)) +
  scale_alpha_continuous(
    breaks = c(0, 1),
    labels = c("Imputed", "Calculated"),
    range = c(0.6, 1)) +
  theme_altair() +
  scale_x_date(expand = c(0, 0),
               date_breaks = "2 month",
               labels = scales::label_date("%b\n%Y")) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill = guide_legend(title = "", reverse = TRUE),
         alpha = guide_legend(title = "", reverse = TRUE)) +
  theme(legend.position = "bottom",
        axis.title = element_blank()) +
  facet_wrap(~metric) + ggthemes::scale_fill_tableau() +
  ggtitle("Estimated time between case symptom onset and contact notification")

waver

labeled_data |>
  filter(df_type == "outer_abs", metric == "med",
         !grepl("Using Automation", source)) |>
  filter(impt_type_alpha != 0.7) |>
  ggplot(aes(x = comb_loc_date,
             y = param_value / 24,
             fill = periods,
             linewidth = impt_type_alpha)) +
  scale_linewidth_continuous(range = c(0, 0.4)) +
  geom_col(position = "stack", color = "black") + coord_flip() +
  guides(linewidth = "none") +
  facet_wrap(~periods, nrow = 1, scales = "free_x")

labeled_data |>
  filter(df_type == "outer_abs", metric == "mean",
         !grepl("Using Automation", source)) |>
  filter(impt_type_alpha != 0.7) |>
  ggplot(aes(x = comb_loc_date,
             y = param_value / 24,
             fill = periods,
             linewidth = impt_type_alpha)) +
  scale_linewidth_continuous(range = c(0, 0.4)) +
  geom_col(position = "stack", color = "black") + coord_flip() +
  facet_wrap(~periods, nrow = 1) + guides(linewidth = "none")
