library(xml2)
library(tidyverse)

# make a list of URLs to download
urls <- paste0("https://apps.neh.gov/open/data/NEH_Grants",
               (196:202 * 10),
               "s.zip")

# load a "get_if_needed" function I've written previously
source("https://gist.githubusercontent.com/jmclawson/65899e2de6bfee692b08141a98422240/raw/7c5590377332e427691f2331b69abd58be2141ec/get_if_needed.R")

# apply the function to every url
purrr::walk(urls, get_if_needed)

# function to extract the xml file from each zip
extract_xml <- function(x, remove = FALSE){
  target <- x |> 
    str_extract("[^/]+$") |> 
    str_replace_all(".zip", ".xml")
  
  directory <- x |> 
    str_extract("^[^/]+")
  
  unzip(x,
        file = target,
        exdir = directory)
  
  if(remove){file.remove(x)}
}

list.files("data", pattern = ".zip", full.names = TRUE) |> 
  purrr::walk(extract_xml)

xml_files <- list.files("data", pattern = ".xml", full.names = TRUE)

return_parsed_table <- function(xml_file){
  nab <- function(pick, xml = xml_file){
    xml_text(xml_find_all(read_xml(xml), xpath = paste0("Grant/", pick)))
  }
  
  tibble(
    AppNumber = read_xml(xml_file) |> 
      xml_children() |> 
      xml_attr("AppNumber")
  ) |> 
    mutate(
      year = nab("YearAwarded"),
      app_type = nab("ApplicantType"),
      institution = nab("Institution"),
      state = nab("InstState"),
      latitude = nab("Latitude"),
      longitude = nab("Longitude"),
      org_type = nab("OrganizationType"),
      program = nab("Program"),
      division = nab("Division"),
      project = nab("ProjectTitle"),
      description = nab("ProjectDesc"),
      discipline = nab("PrimaryDiscipline"),
      approved_outright = nab("ApprovedOutright"),
      approved_matching = nab("ApprovedMatching"),
      award_outright = nab("AwardOutright"),
      award_matching = nab("AwardMatching"),
      original_amount = nab("OriginalAmount"),
      supplement_amount = nab("SupplementAmount")
    ) |> 
    relocate(year) |> 
    mutate(across(approved_outright:original_amount, as.numeric)) |> 
    mutate(
      approved_total = approved_outright + approved_matching,
      .after = approved_matching) |> 
    mutate(
      award_total = award_outright + award_matching,
      .after = award_matching
    )
}

# parsed_table <-
#   rbind(return_parsed_table(xml_files[1]),
#         return_parsed_table(xml_files[2]),
#         return_parsed_table(xml_files[3])) |> 
#   filter(app_type == 1) |> 
#   summarize()
# 
# parsed_table |> 
#   View()

source("R/census_estimates.R")

return_parsed_table(xml_files[6]) |> 
  filter(state %in% c(state.abb, "DC")) |> 
  mutate(year = as.integer(year)) |> 
  select(year, state, award_total) |> 
  summarize(award_total = sum(award_total),
            .by = c(year, state)) |> 
  rowwise() |> 
  mutate(award_per_capita = award_total / get_pop_est(year, state)) |> 
  ggplot(aes(year, award_per_capita)) +
  geom_text(aes(color = state, label = state),
            show.legend = FALSE) +
  scale_y_continuous(labels = scales::label_dollar())

clean_and_sum <- function(file){
  decade <- file |> 
    str_extract_all("[0-9]+") |> 
    substr(1,3)
  
  return_parsed_table(file) |> 
    filter(state %in% c(state.abb, "DC"),
           str_detect(year, decade)) |> 
    mutate(year = as.integer(year)) |> 
    select(year, state, award_total) |> 
    summarize(award_total = sum(award_total),
              .by = c(year, state)) |> 
    rowwise() |> 
    mutate(award_per_capita = award_total / get_pop_est(year, state))
}

if(!file.exists("all_decades3.rds")){
  
  all_decades <- 
    return_parsed_table(xml_files[1]) |>
    rbind(return_parsed_table(xml_files[2])) |> 
    rbind(return_parsed_table(xml_files[3])) |> 
    rbind(return_parsed_table(xml_files[4])) |> 
    rbind(return_parsed_table(xml_files[5])) |> 
    rbind(return_parsed_table(xml_files[6])) #|>
    #rbind(return_parsed_table(xml_files[7])) |> 
    
  
  saveRDS(all_decades, "all_decades.rds")
  
  # adjust for inflation to 2022 amounts
  all_decades2 <- 
    all_decades |> 
    select(year, institution, state, award_total, division) |> 
    distinct() |> 
    mutate(
      amt_2022 = award_total |> 
        priceR::adjust_for_inflation(
          year,
          "US",
          2022))
  
  saveRDS(all_decades2, "all_decades2.rds")
  
  all_decades3 <- all_decades2 |> 
    filter(state %in% c(state.abb, "DC")) |> 
    mutate(year = as.integer(year)) |> 
    summarize(
      award_count = n(),
      award_total = sum(award_total),
      award_adjusted = sum(amt_2022),
      .by = c(year, state)) |> 
    rowwise() |> 
    mutate(
      per_capita_total = award_total / get_pop_est(year, state),
      per_capita_adjusted = award_adjusted / get_pop_est(year, state),
      population = get_pop_est(year, state))
  
  saveRDS(all_decades3, "all_decades3.rds")
}

all_decades3 <- readRDS("all_decades3.rds")

plot_overview <- function(
    df, 
    column,
    title)
{
  df |> 
    ungroup() |> 
    mutate(
      state_median = median({{column}}),
      .by = state) |> 
    ggplot(aes(year, {{column}})) +
    geom_text(aes(color = state_median, label = state),
              show.legend = FALSE) +
    scale_y_continuous(
      labels = scales::label_dollar(),
      expand = expansion(0,)) +
    scale_x_continuous(expand = expansion(0,0)) +
    theme_minimal() +
    labs(
      title = title,
      y = NULL,
      x = NULL) +
    theme(plot.title.position = "plot") +
    coord_cartesian(clip = "off") 
}


