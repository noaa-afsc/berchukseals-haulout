get_age_sex_colors <- function() {
  age_sex_colors <- c(
    "ADULT.F" = "#CC6677",
    "SUBADULT" = "#117733",
    "ADULT.M" = "#88CCEE",
    "ALL AGES" = "#DDCC77"
  )
  return(age_sex_colors)
}

get_age_sex_labels <- function() {
  age_sex_labels <- c(
    "ADULT.F" = "adult female",
    "ADULT.M" = "adult male",
    "SUBADULT" = "subadult",
    "YOUNG OF YEAR" = "young of year",
    "ALL AGES" = "all ages"
  )
  return(age_sex_labels)
}