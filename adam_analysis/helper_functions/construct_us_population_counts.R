construct_us_population_counts <- function(file_path, state_initials) {
  pop_data <- readxl::read_xlsx(file_path, sheet = 2) |>
    mutate(state = gsub("\\.", "", State)) |>
    select(population = `2022_pop`, state)
  pop_data <- bind_rows(
    pop_data,
    bind_cols(state = "United States", population = sum(pop_data$population))
  )
  pop_data <- pop_data |> left_join(state_initials, by = "state")
  return(pop_data)
}
