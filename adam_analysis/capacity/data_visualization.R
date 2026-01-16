library(tidyverse)
library(patchwork)
library(extrafont)
extrafont::loadfonts(device = "postscript")
source("../helper_functions/read_in_functions.R", chdir = TRUE)

# Considering just data from Stargel
combined_data <- read_csv("../../data_extraction/output/combined_data.csv")
all_cases <- read_csv(
  "../../data_extraction/datasets/baselines/weekly_metrics_by_state.csv"
)
# Pulling in the population and case count data as well

pop_data <- construct_us_population_counts(
  "../../data_extraction/datasets/baselines/NST-EST2024-POP.xlsx",
  state_initials()
)

us_case_counts <- construct_us_case_counts(
  "../../data_extraction/datasets/baselines/weekly_metrics_by_state.csv"
)

capacity_params <- c(
  "self_report_codes_issued",
  "user_cases_notifying",
  "notifications_recieved_total",
  "num_users_count",
  "notifications_recieved",
  "cases_named_contacts_count",
  "contacts_reached_count",
  "contacts_named_ratio_mean_cases_naming"
)

wide_capacity_data <- make_wide_data(combined_data, capacity_params)


# Capacity Table

calc_capacity_metrics(wide_capacity_data, pop_data, us_case_counts) |>
  write.csv("out/capacity.csv")


stargel_data <- combined_data |>
  filter(grepl("United States, November 2020 to December 2021.", source))

## Capacity Plots (for appendix)

big_five <-
  stargel_data |>
  filter(
    param_name %in%
      c(
        "cases_assigned_count",
        "cases_interviewed_count",
        "cases_named_contacts_count",
        "contacts_named_count",
        "contacts_reached_count"
      )
  ) |>
  mutate(
    pm_start_date = mdy(pm_start_date)
  )

big_six <- bind_rows(
  big_five,
  calc_cdc_cases(
    all_cases,
    min(big_five$pm_start_date),
    max(big_five$pm_start_date)
  )
)
# Big six confuses things because the cdc cases are sometimes less than
# cases assigned.  This isn't impossible, but it is confusing, so we are
# going to stick with the big five for now.

big_six_plot <- big_six |> demand_plot()
big_five_plot <- big_five |> demand_plot()

ggsave("out/big_five_appendix.png", big_five_plot, height = 6, width = 12)
ggsave("out/big_six_appendix.png", big_six_plot, height = 6, width = 12)

staff_df <- stargel_data |>
  filter(
    param_name %in%
      c(
        "staff_hired_ct_count",
        "staff_hired_ci_count",
        "cases_assigned_count",
        "contacts_named_count",
        "cases_interviewed_count",
        "contacts_reached_count"
      )
  )

ratio_df <- staff_df |>
  pivot_wider(
    id_cols = "pm_start_date",
    names_from = "param_name",
    values_from = "param_value"
  ) |>
  mutate(
    cases_per_ci = cases_assigned_count / staff_hired_ci_count,
    contacts_per_ct = contacts_named_count / staff_hired_ct_count,
    prop_interviewed = cases_interviewed_count / cases_assigned_count,
    prop_cont_reached = contacts_reached_count / contacts_named_count,
    pm_start_date = lubridate::mdy(pm_start_date)
  )

staff_df |>
  ggplot(aes(
    x = pm_start_date,
    y = param_value,
    color = param_name,
    group = param_name
  )) +
  geom_line() +
  geom_point() +
  scale_y_log10()

ymax_val <- ratio_df$cases_assigned_count |> max()

cases_plot <- ratio_df |>
  pivot_longer(cols = -pm_start_date) |>
  filter(
    name %in% c("cases_assigned_count", "cases_per_ci", "staff_hired_ci_count")
  ) |>
  mutate(
    name = case_when(
      name == "cases_assigned_count" ~ "Cases Assigned",
      name == "cases_per_ci" ~ "Cases Assigned Per Case Investigator Hired",
      name == "staff_hired_ci_count" ~ "Case Investigators Hired"
    )
  ) |>
  ggplot(aes(x = pm_start_date, y = value, color = name, group = name)) +
  geom_line() +
  geom_point() +
  scale_y_log10(limits = c(5, ymax_val)) +
  scale_x_date(
    date_breaks = "2 months",
    date_minor_breaks = "month",
    labels = scales::label_date("%b-%Y"),
    expand = c(0.025, 0.1)
  ) +
  ggthemes::scale_color_tableau() +
  theme_altair() +
  theme(legend.position = "bottom", axis.title = element_blank()) +
  guides(color = guide_legend(title = "", nrow = 2)) +
  ggtitle("Case Investigator Demand and Capacity")

contacts_plot <- ratio_df |>
  pivot_longer(cols = -pm_start_date) |>
  filter(
    name %in%
      c("contacts_named_count", "contacts_per_ct", "staff_hired_ct_count")
  ) |>
  mutate(
    name = case_when(
      name == "contacts_named_count" ~ "Contacts Named",
      name == "contacts_per_ct" ~ "Contacts Named Per Contact Tracer Hired",
      name == "staff_hired_ct_count" ~ "Contact Tracers Hired"
    )
  ) |>
  ggplot(aes(x = pm_start_date, y = value, color = name, group = name)) +
  geom_line() +
  geom_point() +
  scale_y_log10(limits = c(5, ymax_val)) +
  scale_x_date(
    date_breaks = "2 months",
    date_minor_breaks = "month",
    labels = scales::label_date("%b-%Y"),
    expand = c(0.025, 0.1)
  ) +
  ggthemes::scale_color_tableau() +
  theme_altair() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank()
  ) +
  guides(color = guide_legend(title = "", nrow = 2)) +
  ggtitle("Contact Tracing Demand and Capacity")


comb_plot <- cases_plot | contacts_plot
ggsave("out/capacity_vs_demand.svg", comb_plot, height = 6, width = 12)
ggsave(
  "out/capacity_vs_demand.pdf",
  comb_plot,
  height = 6,
  width = 12,
  device = cairo_pdf
)

ratio_df |>
  pivot_longer(cols = -pm_start_date) |>
  filter(name %in% c("prop_interviewed", "cases_per_ci")) |>
  mutate(
    value = ifelse(
      name == "prop_interviewed",
      value * 100,
      value
    )
  ) |>
  ggplot(aes(x = pm_start_date, y = value, color = name, group = name)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  theme(legend.position = "bottom")

cases_per_ci <-
  ratio_df |>
  ggplot(aes(x = cases_per_ci, y = prop_interviewed)) +
  geom_point() +
  scale_x_continuous(expand = c(0, 2), limits = c(0, NA)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1),
    labels = scales::label_percent()
  ) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  xlab("Cases Assigned per Case Investigator") +
  theme_altair() +
  theme(
    axis.title.y = element_blank()
  ) +
  ggtitle(
    "Percent of Cases Reached",
    subtitle = "Against Cases Assigned per Case Investigator"
  )

contacts_per_ct <-
  ratio_df |>
  ggplot(aes(x = contacts_per_ct, y = prop_cont_reached)) +
  geom_point() +
  scale_x_continuous(expand = c(0, 2), limits = c(0, NA)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1),
    labels = scales::label_percent()
  ) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  xlab("Contacts per Contact Tracer") +
  theme_altair() +
  theme(
    axis.title.y = element_blank()
  ) +
  ggtitle(
    "Percent of Contacts Reached",
    subtitle = "Against Contacts per Contact Tracer"
  )

cases_per_ci | contacts_per_ct

## Capacity Round 2

ratio_df |>
  pivot_longer(cols = -pm_start_date) |>
  filter(
    name %in%
      c(
        "prop_interviewed",
        "cases_assigned_count",
        "cases_per_ci",
        "staff_hired_ci_count"
      )
  ) |>
  ggplot(aes(x = pm_start_date, y = value, color = name, group = name)) +
  geom_line() +
  geom_point() +
  scale_y_log10(limits = c(0.01, ymax_val)) +
  scale_x_date(
    date_breaks = "2 months",
    date_minor_breaks = "month",
    labels = scales::label_date("%b-%Y"),
    expand = c(0.025, 0.1)
  ) +
  ggthemes::scale_color_tableau() +
  theme_altair() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank()
  ) +
  guides(color = guide_legend(title = "", nrow = 2)) +
  ggtitle("Contact Tracing Demand and Capacity")


make_two_yaxis_plot <- function(var_one, var_two, xvar, v_one_lab, v_two_lab) {
  cols <- ggthemes::tableau_color_pal("Tableau 10")(2)
  df <- data.frame(
    x = ratio_df[, xvar, drop = TRUE],
    y1 = ratio_df[, var_one, drop = TRUE],
    y2 = ratio_df[, var_two, drop = TRUE]
  )

  # Create a scaling factor for the second y-axis
  scale_factor <- max(df$y1) / max(df$y2)

  # Create the plot
  ggplot(df, aes(x = x)) +
    geom_line(aes(y = y1, color = "yval1"), size = 1) +
    geom_point(aes(y = y1, color = "yval1"), size = 3) +
    geom_line(aes(y = y2 * scale_factor, color = "yval2"), size = 1) +
    geom_point(aes(y = y2 * scale_factor, color = "yval2"), size = 3) +
    scale_y_continuous(
      name = v_one_lab,
      sec.axis = sec_axis(
        ~ . / scale_factor,
        name = v_two_lab,
        labels = scales::label_comma(scale = 1, accuracy = 1)
      ),
      labels = scales::label_comma(scale = 1, accuracy = 1),
      limits = c(0, NA)
    ) +
    scale_color_manual(
      values = c("yval1" = cols[1], "yval2" = cols[2]),
      labels = c(v_one_lab, v_two_lab)
    ) +
    labs(
      x = "X Variable",
      title = "Dual Y-Axis Plot",
      color = "Variable"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      axis.title.x = element_blank(),
      axis.title.y.right = element_text(color = cols[2]),
      axis.text.y.right = element_text(color = cols[2]),
      axis.title.y.left = element_text(color = cols[1]),
      axis.text.y.left = element_text(color = cols[1])
    )
}

cases_plot <- make_two_yaxis_plot(
  "cases_assigned_count",
  "staff_hired_ci_count",
  "pm_start_date",
  "Cases Assigned",
  "Case Investigators Hired"
) +
  ggtitle("Cases Assigned and Case Investigators Hired Over Time")

contacts_plot <-
  make_two_yaxis_plot(
    "contacts_named_count",
    "staff_hired_ct_count",
    "pm_start_date",
    "Contacts Named",
    "Contact Tracers Hired"
  ) +
  ggtitle("Contacts Named and Contact Tracers Hired Over Time")

combined_plot <- cases_plot / contacts_plot
ggsave("out/cases_contacts_hired.svg", combined_plot, width = 12, height = 12)
ggsave("out/cases_contacts_hired.png", combined_plot, width = 12, height = 12)
