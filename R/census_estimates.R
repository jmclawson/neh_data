library(tidyverse)

# Define the range of census data to download
decades <- 196:199*10

# Function to download all the data once
get_data <- function(){
  # download data files
  paste0(
    "https://www2.census.gov/programs-surveys/popest/tables/1980-1990/state/asrh/",
    c("st6070ts.txt",
      "st7080ts.txt",
      "st8090ts.txt")) |> 
    walk(get_if_needed, destdir = "data-raw")
  
  get_if_needed("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/state/totals/st-99-03.txt", destdir = "data-raw")
  
  get_if_needed("https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/state/st-est00int-agesex.csv", destdir = "data-raw")
  
  get_if_needed("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx", destdir = "data-raw")
  
  get_if_needed("https://www2.census.gov/programs-surveys/popest/tables/2020-2022/state/totals/NST-EST2022-POP.xlsx", destdir = "data-raw")
  
  # Pull it all together. Some of these files aren't very clean, so it takes some work to standardize everything.
  pop_estimates <- 
    read_table("data/st6070ts.txt", 
             skip = 24, n_max = 51,
             col_names = c("state", 1960:1964)) |> 
    mutate(across(where(is.numeric), {\(x) x * 1000})) |> 
    left_join(
      read_table("data/st6070ts.txt", 
                 skip = 86, n_max = 51,
                 col_names = c("state", 1965:1970)) |> 
        select(-`1970`) |> 
        mutate(across(where(is.numeric), {\(x) x * 1000}))) |> 
    left_join(
      read_table("data/st7080ts.txt", 
                 skip = 14, n_max = 51,
                 col_names = c("fip", "state", 1970:1975))) |> 
    left_join(
      read_table("data/st7080ts.txt", 
                 skip = 67, n_max = 51,
                 col_names = c("fip", "state", 1976:1980))) |> 
    select(-fip) |> 
    left_join(
      read_table("data/st8090ts.txt", 
                 skip = 11, n_max = 51,
                 col_names = c("state", 1980:1984))) |> 
    left_join(
      read_table("data/st8090ts.txt", 
                 skip = 70, n_max = 51,
                 col_names = c("state", 1985:1990))) |> 
    mutate(
      state_name = c(state.name[1:8], 
                     "District of Columbia", 
                     state.name[9:50])) |> 
    relocate(state_name)

  pop_estimates <- pop_estimates |> 
    select(-`1990`) |> 
    left_join(
      read_fwf("data/st-99-03.txt", skip = 101, n_max = 51) |> 
        setNames(c("num", "state_name", 1993:1990, "1990_2")) |> 
        select(state_name, `1990`:`1993`)) |> 
    left_join(
      read_fwf("data/st-99-03.txt", skip = 27, n_max = 51) |> 
        setNames(c("num", "state_name", 1999:1994)) |> 
        select(state_name, `1994`:`1999`)) |> 
    left_join(
      read_csv("data/st-est00int-agesex.csv") |> 
        janitor::clean_names() |> 
        filter(sex == 0,
               age == 999,
               name != "United States") |> 
        select(state_name = name, starts_with("popestimate")) |> 
        pivot_longer(-state_name) |> 
        mutate(name = str_remove_all(name, "popestimate")) |> 
        pivot_wider())
  
  pop_estimates |> 
    select(-`2010`) |>
    left_join(
      readxl::read_excel("data/nst-est2019-01.xlsx", skip = 3) |>
        rename(state_name = `...1`) |>
        filter(str_detect(state_name, "^[.]")) |>
        mutate(state_name = str_remove_all(state_name, "[.]")) |>
        select(state_name, `2010`:`2019`)) |> 
    left_join(
      readxl::read_excel("data/NST-EST2022-POP.xlsx", skip = 3) |>
        rename(state_name = `...1`,
               april = `...2`) |>
        select(-april) |> 
        filter(str_detect(state_name, "^[.]"),
               !str_detect(state_name, "Puerto")) |>
        mutate(state_name = str_remove_all(state_name, "[.]")) |>
        select(state_name, `2020`:`2022`))
}

# Save all the above to a table.
population_table <- get_data()

# Create a function to look up the estimated population of a state in a given year
get_pop_est <- function(year, abbr){
  population_table |> 
    select(-state_name) |> 
    pivot_longer(-c(state)) |> 
    filter(state == abbr,
           name == year) |> 
    pull(value)
}

# sample use:
# get_pop_est(2006, "OH")
