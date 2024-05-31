get_age_sex_colors <- function() {
  age_sex_colors <- c(
    "ADULT.F" = "#0072B2",
    "SUBADULT" = "#009E73",
    "ADULT.M" = "#D55E00",
    "ALL AGES" = "#E69F00"
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