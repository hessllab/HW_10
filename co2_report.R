co2_report <- function(country, year){
  co2_data <- read.csv("./data/co2_emissions_tonnes_per_person_gapminder.csv")
  if (!is.character(country)) {
    stop("country must be a character vector.")
  }
  if (!is.numeric(year)) {
    stop("year must be a numeric vector.")
  }
  xyear <- paste0("X", year)
  co2_yearly <- co2_data[[xyear]]
  nas <- sum(is.na(co2_yearly))
  co2_yearly <- na.omit(co2_yearly)
  quants <- quantile(co2_yearly, probs = seq(0, 1, 0.1))
  gradeA <- quants[9]
  gradeB <- quants[8]
  gradeC <- quants[7]
  gradeD <- quants[6]
  co2_data <- co2_data[co2_data$country %in% country, xyear]
  if (co2_data >= gradeA){
    paste ("Grade: A;", co2_data, "tonnes CO2 per person per year.", nas, "values were not available")
  }
  else if (co2_data >= gradeB){
    paste ("Grade: B;", co2_data, "tonnes CO2 per person per year.", nas, "values were not available")
  }
  else if (co2_data >= gradeC){
    paste ("Grade: C;", co2_data, "tonnes CO2 per person per year.", nas, "values were not available")
  }
  else if (co2_data >= gradeD){
    paste ("Grade: D;", co2_data, "tonnes CO2 per person per year.", nas, "values were not available")
  }
  else if (co2_data < gradeD){
    paste ("Grade: F;", co2_data, "tonnes CO2 per person per year.", nas, "values were not available")
  }
}