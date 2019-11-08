# Usage: co2_report <- ("China", 2014)
co2_report <- function(Country, Year){

# Report errors if inappropriate values are put into the argument.  
  if (!is.character(Country)) {
    stop("Country must be a character.")
  }
  if (!is.numeric(Year)) {
    stop("Year must be a numeric value.")
  }

# First, tailor the year format from 'yyyy' into 'Xyyyy'.  
  Year <- c("X", Year)
  XYear <- paste(Year, collapse = "")

# Read the csv file and extract the target data from the file.  
  emissions <- read.csv(file = "./data/co2_emissions_tonnes_per_person_gapminder.csv")
  target <- emissions[emissions$country == Country, XYear]

# Report an error message if the specified data is NA.  
  if (is.na(target)) {
    stop("CO2 data for the specified country and year is NA.")
  }
  
# Define quantile values (p10, p20 etc.) by using `quantile` to retrieve the cutoffs for grades. The `na.rm = TRUE` will remove NA value when calculating the quantile value.  
  p10 = quantile(emissions[, XYear], 0.10, na.rm = TRUE)
  p20 = quantile(emissions[, XYear], 0.20, na.rm = TRUE)
  p30 = quantile(emissions[, XYear], 0.30, na.rm = TRUE)
  p40 = quantile(emissions[, XYear], 0.40, na.rm = TRUE)

# Put the target values (CO2 emission) that were defined above into the grade categories.  
  if (target <= p10){
    grade <- "A"
  } else if (target <= p20){
    grade <- "B"
  } else if (target <= p30){
    grade <- "C"
  } else if (target <= p40){
    grade <- "D"
  } else{
    grade <- "F"
  }

# Report the above results in a human readable format. 
# Example result: "Grade:F; 7.4 tonnes CO2 per person per year"
  grade_report <- c("Grade:", grade, ";")
  grade_sentence <- paste(grade_report, collapse = "")
  emission_report <- c("tonnes", "CO2", "per", "person", "per", "year")
  emission_sentence <- paste(emission_report, collapse = " ")
  paste(grade_sentence, target, emission_sentence)
}
