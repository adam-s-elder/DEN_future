# Single period time plots
library(tidyverse)
plot_data_folder <- "split_by_month"
source("helper_functions/plotting_functions.R")
impt_period_lens <- read_csv("intermediate_data/props_for_impts.csv")
impt_df <- read_csv("intermediate_data/props_for_impts.csv")
sp_times <- read_csv(paste0("intermediate_data/",
                            plot_data_folder,
                            "/single_period_times.csv"))

sp_times <- sp_times |> mutate(
   periods = as.factor(periods),
  periods = forcats::fct_recode(periods, !!!periods_to_label),
  periods = factor(x = periods,
                       levels = names(periods_to_label),
                       labels = names(periods_to_label)),
  en_type = ifelse(
    grepl("\\%\\%\\%digital", source),
    "Digital", "Manual")
  )

monthly_aves <- calc_monthly_averages(sp_times |> filter(en_type == "Manual"))

## Exploratory Plot

sp_times |> filter(param_value < 24 * 4.2) |>
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

single_period_plot(monthly_aves, "Symptoms to Test")
single_period_plot(monthly_aves, "Test to Result") + ggtitle(
  "Time from Test Submission to Result Reported to Health Department"
)
single_period_plot(monthly_aves, "Result to Case Notified") + ylim(0, 3)
single_period_plot(monthly_aves, "Contact Named to Reached") + theme(
  legend.position.inside = c(0.9, 0.9)) +
  ggtitle("Time from Contact Named to Contact Reached (in days)")


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
            alpha = 0.5, orientation = "x", linewidth = 0.5, linetype = 2) +
  geom_line(data = monthly_aves |>
              mutate(date = month_year,
                     param_value = exclusive_ave),
            alpha = 0.5, orientation = "x", linewidth = 0.5) +
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

stargel_compare <-  sp_times |>
  mutate(stargel =
           as.numeric(grepl("64 HD accross US and collonies", source)),
         stargel = ifelse(stargel == 1, "Stargel", "Non-Stargel"))
monthly_starg_aves <- stargel_compare |> group_by(
  stargel, periods, metric, month_year) |> summarise(
    param_value = mean(param_value, na.rm = TRUE)) |> ungroup()

stargel_compare |>
  ggplot(aes(y = param_value / 24, x = date,
             color = metric)) +
  scale_size_continuous(guide = NULL) +
  geom_point(aes(shape = stargel), alpha = 0.5) +
  geom_line(data = monthly_starg_aves,
            aes(x = month_year, y = param_value / 24,
                color = metric, linetype = stargel)) +
  scale_alpha_continuous(range = c(0.6, 1), guide = NULL) +
  scale_x_date(date_breaks = "month",
               labels = scales::label_date("%b-%Y"),
               expand = c(0, 0)) +
  # geom_line(data = monthly_aves |>
  #             mutate(date = month_year,
  #                    param_value = inclusive_ave),
  #           alpha = 0.5, orientation = "x", size = 0.5, linetype = 2) +
  # geom_line(data = monthly_aves |>
  #             mutate(date = month_year,
  #                    param_value = exclusive_ave),
  #           alpha = 0.5, orientation = "x", size = 0.5) +
  # geom_vline(data = monthly_aves |>
  #              mutate(date = as.POSIXct(month_year)),
  #            aes(xintercept = date), alpha = 0.4) +
  scale_shape_manual("", values = c(1, 20),
                     breaks = c("Stargel", "Non-Stargel"),
                     labels = c("Stargel", "Non-Stargel")) +
  scale_y_continuous(expand = c(0, 0.1), limits = c(0, 4)) +
  theme_altair() +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  facet_wrap(~ periods, ncol = 1) +
  ggtitle("Timiliness of Contact Tracing Process (in days)") +
  ggthemes::scale_color_tableau()

# The following should mostly be done using the single period times
# and should not have the split month times since the benefits here are
# mostly from looking at article level estimates (per time period reported
# in the article).
faceted_period_article_split_plot(sp_times, "med", "Median")

## Stacked Plot
filled_monthly <- fill_missing_values(monthly_aves, imputation_df = impt_df)


filled_monthly |>
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

combined <- filled_monthly |> group_by(month_year, metric) |> summarise(
  symptom_to_notified = sum(average)
)

combined |>
  mutate(metric = ifelse(metric == "mean", "Mean", "Median")) |>
  ggplot(aes(y = symptom_to_notified / 24, x = month_year,
             color = metric)) +
  geom_point(size = 3) +
  geom_line(alpha = 0.5) +
  theme_altair() +
  scale_x_date(date_breaks = "2 month",
               labels = scales::label_date("%b\n%Y")) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom",
        axis.title = element_blank()) +
  ggthemes::scale_fill_tableau() +
  ggtitle("Estimated time between case symptom onset and contact notification")

