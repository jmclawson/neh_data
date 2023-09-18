library(sf)
library(tigris)

# Keeping a local copy lets me do this work on an airplane
if(!file.exists("us_states.rds")){
  us_states <- states(cb = TRUE)
  
  # The st_simplify function speeds up mapping, as learned from Reddit: https://www.reddit.com/r/Rlanguage/comments/vxyou8/plotting_geospatial_data_is_so_slow_how_to_speed/
  us_states <- us_states |> 
    st_simplify(dTolerance = 75)
  
  saveRDS(us_states, "us_states.rds")
} 

us_states <- readRDS("us_states.rds")

# Join the shape data with the data from NEH. Additionally, transform the projection to something nicer. Because the process can take awhile, I'll save completed steps as an Rds file and skip the processing if the file exists. If it doesn't, I'll complete the process and save that final step.
if(!file.exists("awards_by_state.rds")){
  awards_by_state <- us_states |> 
    rename(state = STUSPS) |> 
    left_join(all_decades3) |> 
    st_transform("ESRI:102008")
  
  saveRDS(awards_by_state, "awards_by_state.rds")
} 

awards_by_state <- readRDS("awards_by_state.rds")

# Prepare the final data frame for mapping. Export it so that this step can be skipped in the future, and check here to see if the exported data exists. (If it doesn't exist, run compile the data and then export it.)
if(!file.exists("awards_map_final.rds")){
  
  ###### Move Alaska and Hawaii #####
  # This process is adapted from https://sesync-ci.github.io/blog/transform-Alaska-Hawaii.html
  # Here, I've turned into a function to simplify the process. Following suggested best practices in Wilke's Fundamentals of Data Visualization, Alaska's size is kept stable.
  
  move_state <- function(
    df, # original spatial dataframe
    choice, # value of "state" column
    rotation, # eg -39 * pi/180
    right, # use negative for left/west
    up# use negative for down/south
  ){
    rot <- function(a){
      matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
    }
    
    df_state <- df |> 
      filter(state == choice)
    
    centroid <- df_state |> 
      st_geometry() |> 
      st_union() |> 
      st_centroid()
    
    transformation <- (st_geometry(df_state) - centroid) * rot(rotation) + centroid + c(right, up)
    
    df_state <- df_state |> 
      st_set_geometry(transformation) |> 
      st_set_crs(st_crs(df))
    
    df |> 
      filter(state != choice) |> 
      rbind(df_state)
  }
  
  # This is the version of the data I'll be mapping from
  awards_map_final <- 
    awards_by_state |> 
    move_state("AK", 
               rotation = -45 * pi/180,
               right = 1350000,
               up = -5500000) |> 
    move_state("HI", 
               rotation = -35 * pi/180,
               right = 6800000,
               up = -2000000) 
  
  saveRDS(awards_map_final, "awards_map_final.rds")
}

# saving all the above steps in an external file lets me skip them and load it here to save time.
awards_map_final <- readRDS("awards_map_final.rds")

# functions to make the maps in the second part of the analysis
map_year <- function(df, the_year){
  df <- df |> 
    filter(year == the_year)
  
  if(min(df$per_capita_adjusted) < 0.01){
    the_caption <- "Inflation-adjusted to 2022 dollars.\nGray indicates awards under 1¢ per cap."
  } else {the_caption <- "Inflation-adjusted to 2022 dollars."}
  
  total_us <- df |> 
    filter(year == the_year) |>
    pull(award_adjusted) |> 
    sum(na.rm = TRUE) |> 
    {\(x) x / 1000000}() |> 
    round(digits = 1)
  
  the_subtitle <- paste0(scales::label_comma()(sum(df$award_count, na.rm = TRUE)), " awards, $", total_us, " million nationally")
  
  df |> 
    filter(year == the_year) |> 
    ggplot() +
    geom_sf(
      aes(fill = per_capita_adjusted,
          color = per_capita_adjusted)) + 
    theme_void() +
    scale_color_viridis_c(
      option = "G",
      labels = scales::label_dollar(),
      trans = "log",
      breaks = c(.05, 1, 20),
      limits = c(0.01, 25)
    ) +
    scale_fill_viridis_c(
      option = "G",
      labels = scales::label_dollar(),
      trans = "log",
      breaks = c(.05, 1, 20),
      limits = c(0.01, 25)
    ) +
    labs(fill = "award per capita",
         title = paste(the_year, "NEH Awards by State"),
          subtitle = the_subtitle,
          caption = the_caption) +
    theme(
      legend.position = "bottom",
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5)) +
    guides(
      fill = guide_colorbar(
        title.vjust = 0.75,
        title.hjust = 0.5),
      color = "none")
}

# Same as above, but it accepts multiple years
map_years <- function(df, the_years){
  df <- df |> 
    filter(year %in% the_years)
  
  if(min(df$per_capita_adjusted) < 0.01){
    the_caption <- "Inflation-adjusted to 2022 dollars.\nGray indicates awards under 1¢ per cap."
  } else {the_caption <- "Inflation-adjusted to 2022 dollars."}
  
  total_us <- df |> 
    filter(year %in% the_years) |>
    pull(award_adjusted) |> 
    sum(na.rm = TRUE) |> 
    {\(x) x / 1000000}() |> 
    round(digits = 1)
  
  the_subtitle <- paste0(scales::label_comma()(sum(df$award_count, na.rm = TRUE)), " awards, $", total_us, " million nationally")
  
  df |> 
    filter(year %in% the_years) |> 
    ggplot() +
    geom_sf(
      aes(fill = per_capita_adjusted,
          color = per_capita_adjusted)) + 
    theme_void() +
    scale_color_viridis_c(
      option = "G",
      labels = scales::label_dollar(),
      trans = "log",
      breaks = c(.05, 1, 20),
      limits = c(0.01, 25)
    ) +
    scale_fill_viridis_c(
      option = "G",
      labels = scales::label_dollar(),
      trans = "log",
      breaks = c(.05, 1, 20),
      limits = c(0.01, 25)
    ) +
    labs(fill = "award per capita",
         title = paste0(min(the_years), "-", max(the_years), " NEH Awards by State"),
         subtitle = the_subtitle,
         caption = the_caption) +
    theme(
      legend.position = "bottom",
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5)) +
    guides(
      fill = guide_colorbar(
        title.vjust = 0.75,
        title.hjust = 0.5),
      color = "none") +
    facet_wrap(~year)
}

# sample use:
# awards_map_final |> 
#   map_year(1969)
