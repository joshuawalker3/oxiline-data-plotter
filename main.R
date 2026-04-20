
# Library Load ------------------------------------------------------------

if (!require("pacman"))
  install.packages("pacman")

pacman::p_load(ggpubr, slider, tidyverse)


# Constants ---------------------------------------------------------------

oxiline_metrics_filter <- c(
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

neck_filter <- c("Neck")

waist_filter <- c("Waist")

hips_filter <- c("Hips")

chest_filter <-  c("Chest")

arms_filter <- c("Arm Left", "Arm Right")

thigh_filter <- c("Thigh Left", "Thigh Right")

calf_filter <- c("Calf Left", "Calf Right")
  
  
# Read in Data ------------------------------------------------------------

message(paste0("\nSelect Oxiline Data CSV......"))
oxiline_file_path <- file.choose()
raw_oxiline_data <- read.csv(oxiline_file_path)

message(paste0("\nSelect Body Measurements CSV......"))
body_file_path <- file.choose()
raw_body_measurement_data <- read.csv(body_file_path)


# Clean Data --------------------------------------------------------------

clean_oxiline_data <- raw_oxiline_data |>
  select(Metric, Value, Time) |>
  mutate(Time = as.Date(Time)) |>
  filter(!(Metric %in% oxiline_metrics_filter)) |>
  arrange(Metric)

clean_body_measurement_data <- raw_body_measurement_data |>
  mutate(Time = as.Date(Time)) |>
  arrange(Metric)


# Get Weights -------------------------------------------------------------

weights_only_oxiline_data <- clean_oxiline_data |>
  filter(Metric == "Weight (lb)") |>
  arrange(Time) |>
  mutate(
    Days = as.numeric(difftime(Time, min(Time), units = "days")),
    Week_Num = paste0("Week ", (floor(Days / 7) + 1)),
    Running_Avg = slide_dbl(Value, mean, .before = 3, .after = 3, .complete = FALSE)
    )


# Isolate Body Measurements -----------------------------------------------

neck_measurements <- clean_body_measurement_data |>
  filter(Metric %in% neck_filter)

waist_measurements <- clean_body_measurement_data |>
  filter(Metric %in% waist_filter)

hips_measurements <- clean_body_measurement_data |>
  filter(Metric %in% hips_filter)

chest_measurements <- clean_body_measurement_data |>
  filter(Metric %in% chest_filter)

arms_measurements <- clean_body_measurement_data |>
  filter(Metric %in% arms_filter)

thigh_measurements <- clean_body_measurement_data |>
  filter(Metric %in% thigh_filter)

calf_measurements <- clean_body_measurement_data |>
  filter(Metric %in% calf_filter)


# Plot Data ---------------------------------------------------------------

clean_oxiline_data |>
  ggplot(aes(x = Time, y = Value)) +
  geom_line(color = "grey30", linewidth = 0.8) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", color = "#E69F00", linetype = "dashed", se = TRUE) +
  facet_wrap(~Metric, scales = "free_y", ncol = 3) +
  labs(
    title = "Oxiline Measurements",
    x = "Date",
    y = "Oxiline Scale Measurement"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

neck_measurements |>
  ggplot(aes(x = Time, y = Value)) +
  geom_line(color = "grey30", linewidth = 0.8) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", color = "#E69F00", linetype = "dashed", se = TRUE) +
  stat_regline_equation(label.y = max(neck_measurements$Value + 0.1, na.rm = TRUE)) +
  labs(
    title = "Neck Circumference",
    x = "Date",
    y = "Circumference (Inches)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

waist_measurements |>
  ggplot(aes(x = Time, y = Value)) +
  geom_line(color = "grey30", linewidth = 0.8) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", color = "#E69F00", linetype = "dashed", se = TRUE) +
  stat_regline_equation(label.y = max(waist_measurements$Value + 0.1, na.rm = TRUE)) +
  labs(
    title = "Waist Circumference",
    x = "Date",
    y = "Circumference (Inches)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

hips_measurements |>
  ggplot(aes(x = Time, y = Value)) +
  geom_line(color = "grey30", linewidth = 0.8) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", color = "#E69F00", linetype = "dashed", se = TRUE) +
  stat_regline_equation(label.y = max(hips_measurements$Value + 0.1, na.rm = TRUE)) +
  labs(
    title = "Hips Circumference",
    x = "Date",
    y = "Circumference (Inches)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

chest_measurements |>
  ggplot(aes(x = Time, y = Value)) +
  geom_line(color = "grey30", linewidth = 0.8) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", color = "#E69F00", linetype = "dashed", se = TRUE) +
  stat_regline_equation(label.y = max(chest_measurements$Value + 0.1, na.rm = TRUE)) +
  labs(
    title = "Chest Circumference",
    x = "Date",
    y = "Circumference (Inches)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

arms_measurements |>
  ggplot(aes(x = Time, y = Value)) +
  geom_line(color = "grey30", linewidth = 0.8) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", color = "#E69F00", linetype = "dashed", se = TRUE) +
  stat_regline_equation(label.y = max(arms_measurements$Value + 0.1, na.rm = TRUE)) +
  facet_wrap(~Metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Bicep Circumference",
    x = "Date",
    y = "Circumference (Inches)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

thigh_measurements |>
  ggplot(aes(x = Time, y = Value)) +
  geom_line(color = "grey30", linewidth = 0.8) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", color = "#E69F00", linetype = "dashed", se = TRUE) +
  stat_regline_equation(label.y = max(thigh_measurements$Value + 0.1, na.rm = TRUE)) +
  facet_wrap(~Metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Thigh Circumference",
    x = "Date",
    y = "Circumference (Inches)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

calf_measurements |>
  ggplot(aes(x = Time, y = Value)) +
  geom_line(color = "grey30", linewidth = 0.8) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", color = "#E69F00", linetype = "dashed", se = TRUE) +
  stat_regline_equation(label.y = max(calf_measurements$Value + 0.1, na.rm = TRUE)) +
  facet_wrap(~Metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Calf Circumference",
    x = "Date",
    y = "Circumference (Inches)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

weights_only_oxiline_data |>
  ggplot(aes(x = Days, y = Running_Avg)) +
  geom_line(color = "grey30", linewidth = 0.8) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", color = "#e69F00", linetype = "dashed", se = TRUE) +
  stat_regline_equation(label.y = max(weights_only_oxiline_data$Running_Avg + 0.1, na.rm = TRUE)) +
  labs(
    title = "Body Weight Overall Trend",
    x = "Days Since Start (4/5/26)",
    y = "Weight (Lbs)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

weights_only_oxiline_data |>
  ggplot(aes(x = Days, y = Running_Avg, group = Week_Num, color = Week_Num)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", linetype = "dashed", se = TRUE) +
  stat_regline_equation(aes(label = ..eq.label..), label.x.npc = "center") +
  labs(
    title = "Body Weight Trends By Week",
    x = "Days Since Start (4/5/26)",
    y = "Weight (Lbs)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

