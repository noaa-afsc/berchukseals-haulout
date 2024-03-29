get_age_sex_colors <- function() {
  age_sex_colors <- c(
    "ADULT.F" = "#E73F74",
    "SUBADULT" = "#11A579",
    "ADULT.M" = "#3969AC",
    "ALL AGES" = "#F2B701"
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