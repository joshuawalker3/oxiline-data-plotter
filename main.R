
# Library Load ------------------------------------------------------------

if (!require("pacman"))
  install.packages("pacman")

pacman::p_load(tidyverse)


# Constants ---------------------------------------------------------------

metrics_filter <- c(
  "BMI",
  "Basal Metabolic Rate",
  "Bone Mass",
  "Ideal Body Weight",
  "Left Arm Fat %",
  "Left Arm Fat Mass (kg)",
  "Left Arm Muscle %",
  "Left Leg Fat %",
  "Left Leg Fat Mass (kg)",
  "Left Leg Muscle %",
  "Moisture %",
  "Obesity Degree",
  "Physical Age",
  "Protein %",
  "Right Arm Fat %",
  "Right Arm Fat Mass (kg)",
  "Right Arm Muscle %",
  "Right Leg Fat %",
  "Right Leg Fat Mass (kg)",
  "Right Leg Muscle %",
  "Skeletal Muscle %",
  "Trunk Fat %",
  "Trunk Fat Mass (kg)",
  "Trunk Muscle %",
  "Visceral Fat",
  "Waist-Hip Ratio",
  "Weight (kg)"
)
# Read in Data ------------------------------------------------------------

message(paste0("\nSelect Oxiline Data CSV......"))
file_path <- file.choose()
raw_oxiline_data <- read.csv(file_path)


# Clean Data --------------------------------------------------------------

clean_oxiline_data <- raw_oxiline_data |>
  select(Metric, Value, Time) |>
  mutate(Time = as.Date(Time)) |>
  filter(!(Metric %in% metrics_filter)) |>
  arrange(Metric)


