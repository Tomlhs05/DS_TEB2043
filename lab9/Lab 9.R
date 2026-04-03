
library(ggplot2)
library(corrplot)
library(reshape2)
library(gridExtra)
library(scales)


setwd("C:/Users/ammir/Documents/DS_TEB2043/Lab 9")

cat("Working Directory:\n")
print(getwd())

# ===================== Activity 1 =====================

data("ToothGrowth")
df_tooth <- ToothGrowth

cat("=== ToothGrowth Dataset Preview ===\n")
print(head(df_tooth))
cat("\nDimensions:", dim(df_tooth), "\n")
cat("\nSummary:\n")
print(summary(df_tooth))

df_tooth$supp_num <- as.numeric(df_tooth$supp)

corr_matrix <- cor(df_tooth[, c("len", "dose", "supp_num")],
                   method = "pearson")

cat("\n=== Pearson Correlation Matrix ===\n")
print(round(corr_matrix, 4))

# ---- Corrplot (DISPLAY + SAVE) ----
corrplot(corr_matrix,
         method = "color",
         type = "lower",
         addCoef.col = "black",
         tl.col = "#2E4057",
         tl.srt = 30)

png("activity1_heatmap_corrplot.png", width = 700, height = 600, res = 120)
corrplot(corr_matrix,
         method = "color",
         type = "lower",
         addCoef.col = "black",
         tl.col = "#2E4057",
         tl.srt = 30)
dev.off()

# ---- Heatmap (ggplot) ----
colnames(corr_matrix) <- rownames(corr_matrix) <-
  c("Tooth Length", "Dose", "Supplement\n(OJ=1, VC=2)")

melted_corr <- melt(corr_matrix)

p_heatmap <- ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 3)), size = 5) +
  scale_fill_gradient2(low = "#E84855", mid = "white", high = "#048A81",
                       midpoint = 0) +
  labs(title = "ToothGrowth – Correlation Heatmap") +
  theme_minimal()

print(p_heatmap)
ggsave("activity1_heatmap_ggplot.png",
       plot = p_heatmap,
       width = 7, height = 5, dpi = 150)

# ---- Scatter Plot ----
p_scatter <- ggplot(ToothGrowth, aes(x = dose, y = len, color = supp)) +
  geom_jitter(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Tooth Length vs Dose") +
  theme_minimal()

print(p_scatter)
ggsave("activity1_scatter.png",
       plot = p_scatter,
       width = 7, height = 5, dpi = 150)

cat("\n=== Activity 1: Key Observations ===\n")
cat("len vs dose  :", round(cor(ToothGrowth$len, ToothGrowth$dose), 4), "\n")
cat("len vs supp  :", round(cor(ToothGrowth$len, as.numeric(ToothGrowth$supp)), 4), "\n")
cat("dose vs supp :", round(cor(ToothGrowth$dose, as.numeric(ToothGrowth$supp)), 4), "\n")


# ===================== Activity 2 =====================

data("mtcars")
X <- mtcars[, c("mpg", "disp", "hp", "wt", "qsec")]

cat("\n=== mtcars Original Summary ===\n")
print(summary(X))

# ---- Normalization ----
X_log <- log1p(X)
X_std <- as.data.frame(scale(X))

min_max_scale <- function(x) (x - min(x)) / (max(x) - min(x))
X_minmax <- as.data.frame(lapply(X, min_max_scale))

# ---- Prepare data ----
label_melt <- function(df, method) {
  df$method <- method
  df$car <- rownames(mtcars)
  melt(df, id.vars = c("car", "method"), variable.name = "feature")
}

all_data <- rbind(
  label_melt(X, "Original"),
  label_melt(X_log, "Log"),
  label_melt(X_std, "Standard"),
  label_melt(X_minmax, "MinMax")
)

# ---- Boxplot ----
p_box <- ggplot(all_data[all_data$method != "Original", ],
                aes(x = feature, y = value, fill = method)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Normalization Comparison")

print(p_box)
ggsave("activity2_boxplot.png",
       plot = p_box,
       width = 8, height = 5, dpi = 150)

# ---- Density Plot ----
mpg_data <- all_data[all_data$feature == "mpg", ]

p_density <- ggplot(mpg_data, aes(x = value, fill = method)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~method, scales = "free") +
  theme_minimal() +
  labs(title = "MPG Distribution After Normalization")

print(p_density)
ggsave("activity2_density.png",
       plot = p_density,
       width = 8, height = 5, dpi = 150)

# ---- Summary ----
cat("\n=== Summary: Log Transformation ===\n")
print(apply(X_log, 2, function(x) round(summary(x), 3)))

cat("\n=== Summary: Standard Scaling ===\n")
print(apply(X_std, 2, function(x) round(summary(x), 3)))

cat("\n=== Summary: Min-Max Scaling ===\n")
print(apply(X_minmax, 2, function(x) round(summary(x), 3)))

# ---- Checks ----
cat("\nRange Min-Max:\n")
print(sapply(X_minmax, range))

cat("\nStandard Scaling Mean & SD:\n")
print(round(colMeans(X_std), 6))
print(round(apply(X_std, 2, sd), 6))

# ---- Final Output ----
cat("\nSaved Files:\n")
print(list.files())

cat("\nDone! All plots saved successfully.\n")