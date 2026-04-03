# ============================================================
# TEB2164 Introduction to Data Science
# Lab 7b - Titanic Data Analysis
# ============================================================

# ---- 1. Load Libraries ----
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

cat("================================================================\n")
cat("   TEB2164 - Lab 7b: Titanic Data Analysis Report\n")
cat("================================================================\n\n")

setwd("C:/Users/ammir/Documents/DS_TEB2043/Lab 7")

# ---- 2. Import Data ----
titanic <- read_csv("titanic.csv")

cat(">>> [IMPORT] Dataset loaded successfully.\n")
cat("    Rows:", nrow(titanic), "| Columns:", ncol(titanic), "\n\n")


# ---- 3. Initial Exploration ----
cat("SECTION 1: INITIAL DATA EXPLORATION\n")

print(colnames(titanic))
print(head(titanic))
glimpse(titanic)
summary(titanic)


# ---- 4. Missing Value Detection ----
cat("\nSECTION 2: MISSING VALUE ANALYSIS\n")

missing_counts <- colSums(is.na(titanic))
missing_pct <- round(missing_counts / nrow(titanic) * 100, 1)

missing_df <- data.frame(
  Column = names(missing_counts),
  Missing = as.integer(missing_counts),
  Percent = missing_pct
) %>%
  filter(Missing > 0) %>%
  arrange(desc(Missing))

print(missing_df)

# FIXED PLOT (removed xlim NA)
ggplot(missing_df, aes(x = reorder(Column, Missing), y = Missing, fill = Column)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = paste0(Missing, " (", Percent, "%)")),
            hjust = -0.1, size = 3.8) +
  coord_flip() +
  scale_fill_manual(values = c("Age" = "#378ADD", "Cabin" = "#7F77DD", "Embarked" = "#1D9E75")) +
  labs(title = "Missing Values Before Cleaning",
       x = "Column", y = "Count of Missing Values") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold")) +
  coord_cartesian(ylim = c(0, max(missing_df$Missing) * 1.25))

ggsave("plot_01_missing_values.png", width = 7, height = 4, dpi = 150)


# ---- 5. Data Cleaning ----
cat("\nSECTION 3: DATA CLEANING\n")

titanic_clean <- titanic %>%
  mutate(
    Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age),
    Embarked = ifelse(is.na(Embarked),
                      names(sort(table(Embarked), decreasing = TRUE))[1],
                      Embarked),
    Cabin = ifelse(is.na(Cabin), "Unknown", Cabin),
    Survived = factor(Survived, levels = c(0, 1),
                      labels = c("Did Not Survive", "Survived")),
    Pclass = factor(Pclass, levels = c(1, 2, 3),
                    labels = c("1st Class", "2nd Class", "3rd Class")),
    Sex = factor(Sex),
    Embarked = factor(Embarked, levels = c("C", "Q", "S"),
                      labels = c("Cherbourg", "Queenstown", "Southampton")),
    FamilySize = SibSp + Parch
  )

print(colSums(is.na(titanic_clean)))


# ---- 6. Survival Overview ----
cat("\nSECTION 4: SURVIVAL OVERVIEW\n")

survival_rate <- titanic_clean %>%
  count(Survived) %>%
  mutate(Percent = round(n / sum(n) * 100, 1))

print(survival_rate)

ggplot(survival_rate, aes(x = Survived, y = n, fill = Survived)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, "\n(", Percent, "%)")), vjust = -0.4) +
  theme_minimal()

ggsave("plot_02_overall_survival.png", width = 6, height = 5)


# ---- 7. Survival by Sex ----
cat("\nSECTION 5: SURVIVAL BY SEX\n")

sex_survival <- titanic_clean %>%
  group_by(Sex, Survived) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Sex) %>%
  mutate(Percent = round(n / sum(n) * 100, 1))

print(sex_survival)

ggplot(sex_survival, aes(x = Sex, y = Percent, fill = Survived)) +
  geom_col(position = "stack") +
  theme_minimal()

ggsave("plot_03_survival_by_sex.png", width = 6, height = 5)


# ---- 8. Survival by Class ----
cat("\nSECTION 6: SURVIVAL BY CLASS\n")

class_survival <- titanic_clean %>%
  group_by(Pclass, Survived) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Pclass) %>%
  mutate(Percent = round(n / sum(n) * 100, 1))

print(class_survival)

ggplot(class_survival, aes(x = Pclass, y = Percent, fill = Survived)) +
  geom_col(position = "dodge") +
  theme_minimal()

ggsave("plot_04_survival_by_class.png", width = 7, height = 5)


# ---- 9. Export Cleaned Data ----
write_csv(titanic_clean, "titanic_cleaned.csv")

cat("\nAnalysis Completed Successfully.\n")