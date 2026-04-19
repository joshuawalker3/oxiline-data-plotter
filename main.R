
# Library Load ------------------------------------------------------------

if (!require("pacman"))
  install.packages("pacman")

pacman::p_load(ggpubr, slider, tidyverse)


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


# Get Weights -------------------------------------------------------------

weights_only_oxiline_data <- clean_oxiline_data |>
  filter(Metric == "Weight (lb)") |>
  arrange(Time) |>
  mutate(running_avg = slide_dbl(Value, mean, .before = 6, .complete = FALSE))


# Plot Data ---------------------------------------------------------------

clean_oxiline_data |>
  ggplot(aes(x = Time, y = Value)) +
  geom_line(color = "grey30", linewidth = 0.8) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", color = "#E69F00", linetype = "dashed", se = TRUE) +
  facet_wrap(~Metric, scales = "free_y", ncol = 3)

weights_only_oxiline_data |>
  ggplot(aes(x = Time, y = running_avg)) +
  geom_line(color = "grey30", linewidth = 0.8) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", color = "#e69F00", linetype = "dashed", se = TRUE) +
  stat_regline_equation(label.y = max(weights_only_oxiline_data$running_avg + 0.1, na.rm = FALSE))
