
# Part 1: Titanic Dataset (Lab 7b)
# Part 2: Built-in Dataset - mtcars
# ============================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
install.packages('ggrepel')

setwd("C:/Users/ammir/Documents/DS_TEB2043/Lab 8")

cat("================================================================\n")
cat("   TEB2164 Lab 8 - Data Visualization Report\n")
cat("================================================================\n\n")


# ============================================================
# PART 1: TITANIC DATASET - IMPROVED VISUALIZATION
# ============================================================

cat("################################################################\n")
cat("  PART 1: TITANIC DATASET\n")
cat("################################################################\n\n")

# ---- Load & Clean (carried over from Lab 7b) ----
titanic <- read_csv("titanic.csv")

titanic_clean <- titanic %>%
  mutate(
    Age      = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age),
    Embarked = ifelse(is.na(Embarked),
                      names(sort(table(Embarked), decreasing = TRUE))[1],
                      Embarked),
    Cabin      = ifelse(is.na(Cabin), "Unknown", Cabin),
    Survived   = factor(Survived, levels = c(0,1),
                        labels = c("Did Not Survive","Survived")),
    Pclass     = factor(Pclass, levels = c(1,2,3),
                        labels = c("1st Class","2nd Class","3rd Class")),
    Sex        = factor(Sex),
    Embarked   = factor(Embarked, levels = c("C","Q","S"),
                        labels = c("Cherbourg","Queenstown","Southampton")),
    FamilySize = SibSp + Parch,
    AgeGroup   = cut(Age,
                     breaks = c(0, 12, 17, 35, 60, 100),
                     labels = c("Child","Teen","Young Adult","Adult","Senior"))
  )

cat(">>> Data loaded and cleaned. Rows:", nrow(titanic_clean),
    "| Columns:", ncol(titanic_clean), "\n\n")


# ----------------------------------------------------------
# PLOT T1 — PIE CHART: Overall Survival Proportion
# ----------------------------------------------------------
cat("--- PLOT T1: PIE CHART - Overall Survival ---\n")

survival_rate <- titanic_clean %>%
  count(Survived) %>%
  mutate(Percent  = round(n / sum(n) * 100, 1),
         Label    = paste0(Survived, "\n", n, " (", Percent, "%)"))

ggplot(survival_rate, aes(x = "", y = n, fill = Survived)) +
  geom_col(width = 1, color = "white", linewidth = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            size = 4.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("Did Not Survive" = "#E24B4A",
                               "Survived"        = "#1D9E75")) +
  labs(title    = "Overall Survival on the Titanic",
       subtitle = "38.4% of 891 passengers survived",
       fill     = "") +
  theme_void(base_size = 13) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
        legend.position = "none")

ggsave("T1_pie_survival.png", width = 6, height = 6, dpi = 150)
cat("[SAVED] T1_pie_survival.png\n")
cat("[INSIGHT] Only 38.4% (342 out of 891) passengers survived the Titanic disaster.\n\n")


# ----------------------------------------------------------
# PLOT T2 — BAR CHART: Survival by Passenger Class
# ----------------------------------------------------------
cat("--- PLOT T2: BAR CHART - Survival by Class ---\n")

class_survival <- titanic_clean %>%
  group_by(Pclass, Survived) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Pclass) %>%
  mutate(Percent = round(n / sum(n) * 100, 1))

print(class_survival)

ggplot(class_survival, aes(x = Pclass, y = Percent, fill = Survived)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = c("Did Not Survive" = "#E24B4A",
                               "Survived"        = "#1D9E75")) +
  labs(title    = "Survival Rate by Passenger Class",
       subtitle = "Higher class = significantly better survival odds",
       x = "Passenger Class", y = "Percentage (%)", fill = "") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        legend.position = "top") +
  ylim(0, 85)

ggsave("T2_bar_class_survival.png", width = 7, height = 5, dpi = 150)
cat("[SAVED] T2_bar_class_survival.png\n")
cat("[INSIGHT] 1st class had 63% survival vs only 24.2% for 3rd class passengers.\n\n")


# ----------------------------------------------------------
# PLOT T3 — HISTOGRAM: Age Distribution by Survival
# ----------------------------------------------------------
cat("--- PLOT T3: HISTOGRAM - Age Distribution ---\n")

age_stats <- titanic_clean %>%
  group_by(Survived) %>%
  summarise(Mean = round(mean(Age), 1), .groups = "drop")

print(age_stats)

ggplot(titanic_clean, aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 5, color = "white", alpha = 0.85,
                 position = "identity") +
  geom_vline(data = age_stats, aes(xintercept = Mean, color = Survived),
             linetype = "dashed", linewidth = 1) +
  scale_fill_manual(values  = c("Did Not Survive" = "#E24B4A",
                                "Survived"        = "#378ADD")) +
  scale_color_manual(values = c("Did Not Survive" = "#A32D2D",
                                "Survived"        = "#0C447C")) +
  facet_wrap(~Survived, ncol = 1) +
  labs(title    = "Age Distribution by Survival Outcome",
       subtitle = "Dashed line = mean age per group",
       x = "Age (years)", y = "Number of Passengers",
       fill = "", color = "Mean Age") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        legend.position = "top")

ggsave("T3_histogram_age.png", width = 7, height = 6, dpi = 150)
cat("[SAVED] T3_histogram_age.png\n")
cat("[INSIGHT] Survivors had a mean age of 28.3 vs 30.6 for non-survivors.\n")
cat("[INSIGHT] Young children (0-10) had noticeably higher survival rates.\n\n")


# ----------------------------------------------------------
# PLOT T4 — BOX PLOT: Fare by Class and Survival
# ----------------------------------------------------------
cat("--- PLOT T4: BOX PLOT - Fare by Class & Survival ---\n")

fare_stats <- titanic_clean %>%
  group_by(Pclass) %>%
  summarise(Median_Fare = round(median(Fare), 2),
            Mean_Fare   = round(mean(Fare), 2), .groups = "drop")

print(fare_stats)

ggplot(titanic_clean, aes(x = Pclass, y = Fare, fill = Survived)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5,
               outlier.fill = "grey70", alpha = 0.85) +
  scale_fill_manual(values = c("Did Not Survive" = "#E24B4A",
                               "Survived"        = "#1D9E75")) +
  scale_y_log10(labels = scales::label_dollar(prefix = "£")) +
  labs(title    = "Fare Distribution by Passenger Class and Survival",
       subtitle = "Log scale used — 1st class fares vary widely",
       x = "Passenger Class", y = "Fare (£, log scale)", fill = "") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        legend.position = "top")

ggsave("T4_boxplot_fare.png", width = 8, height = 5, dpi = 150)
cat("[SAVED] T4_boxplot_fare.png\n")
cat("[INSIGHT] 1st class survivors paid noticeably higher fares than non-survivors.\n")
cat("[INSIGHT] 3rd class fares were tightly clustered at the low end regardless of outcome.\n\n")


# ----------------------------------------------------------
# PLOT T5 — SCATTER PLOT: Age vs Fare coloured by Survival
# ----------------------------------------------------------
cat("--- PLOT T5: SCATTER PLOT - Age vs Fare ---\n")

ggplot(titanic_clean, aes(x = Age, y = Fare, color = Survived,
                          shape = Survived)) +
  geom_point(alpha = 0.55, size = 2) +
  scale_color_manual(values = c("Did Not Survive" = "#E24B4A",
                                "Survived"        = "#1D9E75")) +
  scale_y_log10(labels = scales::label_dollar(prefix = "£")) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  labs(title    = "Age vs Fare — coloured by Survival",
       subtitle = "Passengers who paid more tended to survive more",
       x = "Age (years)", y = "Fare (£, log scale)",
       color = "", shape = "") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        legend.position = "top")

ggsave("T5_scatter_age_fare.png", width = 8, height = 5, dpi = 150)
cat("[SAVED] T5_scatter_age_fare.png\n")
cat("[INSIGHT] Higher fares strongly associate with survival across all age groups.\n")
cat("[INSIGHT] Most non-survivors are clustered in the low-fare, younger age range.\n\n")


# ----------------------------------------------------------
# PLOT T6 — LINE GRAPH: Survival Rate by Age Group & Sex
# ----------------------------------------------------------
cat("--- PLOT T6: LINE GRAPH - Survival Rate by Age Group & Sex ---\n")

agegroup_sex <- titanic_clean %>%
  group_by(AgeGroup, Sex) %>%
  summarise(SurvivalRate = round(mean(Survived == "Survived") * 100, 1),
            Count = n(), .groups = "drop") %>%
  filter(!is.na(AgeGroup))

print(agegroup_sex)

ggplot(agegroup_sex, aes(x = AgeGroup, y = SurvivalRate,
                         color = Sex, group = Sex)) +
  geom_line(linewidth = 1.3) +
  geom_point(aes(size = Count), alpha = 0.85) +
  geom_text(aes(label = paste0(SurvivalRate, "%")),
            vjust = -1, size = 3.5, fontface = "bold") +
  scale_color_manual(values = c("female" = "#7F77DD",
                                "male"   = "#378ADD")) +
  scale_size_continuous(range = c(3, 8), name = "Passengers") +
  labs(title    = "Survival Rate by Age Group and Sex",
       subtitle = "Point size = number of passengers in that group",
       x = "Age Group", y = "Survival Rate (%)", color = "Sex") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        legend.position = "top") +
  ylim(0, 105)

ggsave("T6_line_agegroup_sex.png", width = 8, height = 5, dpi = 150)
cat("[SAVED] T6_line_agegroup_sex.png\n")
cat("[INSIGHT] Female survival rate stays high (70-100%) across almost all age groups.\n")
cat("[INSIGHT] Male survival peaks at children (Child group) then drops sharply.\n\n")


# ============================================================
# PART 2: BUILT-IN DATASET — mtcars
# Story: Presenting to a Car Manufacturing Executive
# ============================================================

cat("################################################################\n")
cat("  PART 2: BUILT-IN DATASET — mtcars\n")
cat("  Story: Car Performance Analysis for Manufacturing Team\n")
cat("################################################################\n\n")

data(mtcars)

mtcars_clean <- mtcars %>%
  mutate(
    cyl  = factor(cyl,  levels = c(4, 6, 8),
                  labels = c("4 Cylinder","6 Cylinder","8 Cylinder")),
    am   = factor(am,   levels = c(0, 1),
                  labels = c("Automatic","Manual")),
    gear = factor(gear, levels = c(3, 4, 5),
                  labels = c("3-Speed","4-Speed","5-Speed")),
    vs   = factor(vs,   levels = c(0, 1),
                  labels = c("V-shaped","Straight"))
  ) %>%
  tibble::rownames_to_column("CarModel")

cat(">>> mtcars dataset loaded.\n")
cat("    Rows:", nrow(mtcars_clean), "| Columns:", ncol(mtcars_clean), "\n\n")

cat(">>> Dataset preview:\n")
print(head(mtcars_clean))

cat("\n>>> Summary statistics:\n")
print(summary(mtcars_clean %>% select(mpg, hp, wt, qsec)))


# ----------------------------------------------------------
# PLOT M1 — PIE CHART: Proportion of Cylinder Types
# ----------------------------------------------------------
cat("\n--- PLOT M1: PIE CHART - Cylinder Distribution ---\n")

cyl_count <- mtcars_clean %>%
  count(cyl) %>%
  mutate(Percent = round(n / sum(n) * 100, 1),
         Label   = paste0(cyl, "\n", n, " cars\n(", Percent, "%)"))

print(cyl_count)

ggplot(cyl_count, aes(x = "", y = n, fill = cyl)) +
  geom_col(width = 1, color = "white", linewidth = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("4 Cylinder" = "#1D9E75",
                               "6 Cylinder" = "#378ADD",
                               "8 Cylinder" = "#D85A30")) +
  labs(title    = "Fleet Composition by Engine Cylinder Count",
       subtitle = "mtcars dataset — 32 car models",
       fill     = "") +
  theme_void(base_size = 13) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
        legend.position = "none")

ggsave("M1_pie_cylinders.png", width = 6, height = 6, dpi = 150)
cat("[SAVED] M1_pie_cylinders.png\n")
cat("[INSIGHT] 8-cylinder cars dominate the fleet at 43.8%, followed by 4-cylinder at 34.4%.\n\n")


# ----------------------------------------------------------
# PLOT M2 — BAR CHART: Average MPG by Cylinder & Transmission
# ----------------------------------------------------------
cat("--- PLOT M2: BAR CHART - Avg MPG by Cylinder & Transmission ---\n")

mpg_summary <- mtcars_clean %>%
  group_by(cyl, am) %>%
  summarise(Avg_MPG = round(mean(mpg), 1), .groups = "drop")

print(mpg_summary)

ggplot(mpg_summary, aes(x = cyl, y = Avg_MPG, fill = am)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(Avg_MPG, " mpg")),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = c("Automatic" = "#7F77DD",
                               "Manual"    = "#EF9F27")) +
  labs(title    = "Average Fuel Efficiency (MPG) by Cylinders & Transmission",
       subtitle = "Manual cars consistently outperform automatic in fuel efficiency",
       x = "Engine Type", y = "Average MPG", fill = "Transmission") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        legend.position = "top") +
  ylim(0, 35)

ggsave("M2_bar_mpg_cyl.png", width = 8, height = 5, dpi = 150)
cat("[SAVED] M2_bar_mpg_cyl.png\n")
cat("[INSIGHT] 4-cylinder manual cars average 28.1 mpg — the most fuel efficient.\n")
cat("[INSIGHT] 8-cylinder automatics average only 15.1 mpg — the least efficient.\n\n")


# ----------------------------------------------------------
# PLOT M3 — HISTOGRAM: Horsepower Distribution
# ----------------------------------------------------------
cat("--- PLOT M3: HISTOGRAM - Horsepower Distribution ---\n")

hp_stats <- mtcars_clean %>%
  summarise(Mean_HP   = round(mean(hp), 1),
            Median_HP = round(median(hp), 1))
print(hp_stats)

ggplot(mtcars_clean, aes(x = hp, fill = cyl)) +
  geom_histogram(binwidth = 25, color = "white", alpha = 0.9) +
  geom_vline(xintercept = hp_stats$Mean_HP,
             linetype = "dashed", color = "#2C2C2A", linewidth = 1) +
  annotate("text", x = hp_stats$Mean_HP + 8, y = 5.5,
           label = paste0("Mean = ", hp_stats$Mean_HP, " hp"),
           hjust = 0, size = 3.8, color = "#2C2C2A") +
  scale_fill_manual(values = c("4 Cylinder" = "#1D9E75",
                               "6 Cylinder" = "#378ADD",
                               "8 Cylinder" = "#D85A30")) +
  labs(title    = "Horsepower Distribution Across All Car Models",
       subtitle = "8-cylinder cars dominate the high-power range",
       x = "Horsepower (hp)", y = "Number of Cars", fill = "Engine") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        legend.position = "top")

ggsave("M3_histogram_hp.png", width = 8, height = 5, dpi = 150)
cat("[SAVED] M3_histogram_hp.png\n")
cat("[INSIGHT] Mean horsepower across all models is 146.7 hp.\n")
cat("[INSIGHT] 8-cylinder engines fill the 150-350 hp range almost exclusively.\n\n")


# ----------------------------------------------------------
# PLOT M4 — BOX PLOT: MPG by Cylinder and Transmission
# ----------------------------------------------------------
cat("--- PLOT M4: BOX PLOT - MPG by Cylinder ---\n")

ggplot(mtcars_clean, aes(x = cyl, y = mpg, fill = am)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2,
               outlier.fill = "grey80", alpha = 0.85) +
  scale_fill_manual(values = c("Automatic" = "#7F77DD",
                               "Manual"    = "#EF9F27")) +
  labs(title    = "MPG Distribution by Engine Type and Transmission",
       subtitle = "Manual transmission shows higher spread and median MPG",
       x = "Engine Cylinders", y = "Miles Per Gallon (MPG)", fill = "Transmission") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        legend.position = "top")

ggsave("M4_boxplot_mpg.png", width = 8, height = 5, dpi = 150)
cat("[SAVED] M4_boxplot_mpg.png\n")
cat("[INSIGHT] 4-cyl manual cars show the widest MPG range (21-33 mpg).\n")
cat("[INSIGHT] 8-cylinder cars have the least variability — all performing poorly on MPG.\n\n")


# ----------------------------------------------------------
# PLOT M5 — SCATTER PLOT: Weight vs MPG by Cylinder
# ----------------------------------------------------------
cat("--- PLOT M5: SCATTER PLOT - Weight vs MPG ---\n")

ggplot(mtcars_clean, aes(x = wt, y = mpg, color = cyl, label = CarModel)) +
  geom_point(size = 3.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1),
              color = "#2C2C2A", linewidth = 0.8, linetype = "dashed") +
  ggrepel::geom_text_repel(size = 2.8, max.overlaps = 8,
                           color = "grey40") +
  scale_color_manual(values = c("4 Cylinder" = "#1D9E75",
                                "6 Cylinder" = "#378ADD",
                                "8 Cylinder" = "#D85A30")) +
  labs(title    = "Vehicle Weight vs Fuel Efficiency",
       subtitle = "Heavier cars consistently achieve lower MPG",
       x = "Weight (1000 lbs)", y = "Miles Per Gallon (MPG)",
       color = "Engine") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        legend.position = "top")

ggsave("M5_scatter_weight_mpg.png", width = 9, height = 6, dpi = 150)
cat("[SAVED] M5_scatter_weight_mpg.png\n")
cat("[INSIGHT] Strong negative correlation — as weight increases, MPG decreases.\n")
cat("[INSIGHT] Lightest cars (Toyota Corolla, Fiat 128) achieve the best fuel economy.\n\n")


# ----------------------------------------------------------
# PLOT M6 — LINE GRAPH: MPG trend ranked by Horsepower
# ----------------------------------------------------------
cat("--- PLOT M6: LINE GRAPH - MPG vs Horsepower Trend ---\n")

mtcars_sorted <- mtcars_clean %>%
  arrange(hp)

ggplot(mtcars_sorted, aes(x = hp, y = mpg, color = cyl, group = 1)) +
  geom_line(color = "grey80", linewidth = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_manual(values = c("4 Cylinder" = "#1D9E75",
                                "6 Cylinder" = "#378ADD",
                                "8 Cylinder" = "#D85A30")) +
  labs(title    = "Fuel Efficiency Trend as Horsepower Increases",
       subtitle = "As engine power grows, fuel efficiency declines",
       x = "Horsepower (hp)", y = "Miles Per Gallon (MPG)",
       color = "Engine") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        legend.position = "top")

ggsave("M6_line_hp_mpg.png", width = 9, height = 5, dpi = 150)
cat("[SAVED] M6_line_hp_mpg.png\n")
cat("[INSIGHT] The overall trend shows MPG drops as horsepower rises.\n")
cat("[INSIGHT] Low-HP 4-cylinder models cluster above 25 mpg; high-HP 8-cyl below 20 mpg.\n\n")




cat("================================================================\n")
cat("   ALL PLOTS SAVED SUCCESSFULLY\n")
cat("================================================================\n")
cat("\nPART 1 — TITANIC (6 plots):\n")
cat("  T1_pie_survival.png\n")
cat("  T2_bar_class_survival.png\n")
cat("  T3_histogram_age.png\n")
cat("  T4_boxplot_fare.png\n")
cat("  T5_scatter_age_fare.png\n")
cat("  T6_line_agegroup_sex.png\n")
cat("\nPART 2 — MTCARS (6 plots):\n")
cat("  M1_pie_cylinders.png\n")
cat("  M2_bar_mpg_cyl.png\n")
cat("  M3_histogram_hp.png\n")
cat("  M4_boxplot_mpg.png\n")
cat("  M5_scatter_weight_mpg.png\n")
cat("  M6_line_hp_mpg.png\n")

cat("================================================================\n")