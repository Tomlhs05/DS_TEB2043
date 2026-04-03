# Set the working directory
setwd("C:/Users/ammir/Documents/DS_TEB2043/Lab 10")

# --- Activity 1: Linear Regression ---
data("Theoph")
model <- lm(Dose ~ Wt, data = Theoph)

# Save the Regression Plot
png("Linear_Regression_Plot.png", width = 800, height = 600)
plot(Theoph$Wt, Theoph$Dose, 
     main = "Regression: Weight vs Dose",
     xlab = "Weight (kg)", ylab = "Dose (mg/kg)",
     pch = 19, col = "blue")
abline(model, col = "red", lwd = 2)
dev.off() # Closes the file and saves it

# Prediction Logic
new_weights <- data.frame(Wt = c(90, 95, 100))
predictions <- predict(model, newdata = new_weights)
print(cbind(new_weights, Predicted_Dose = predictions))


# --- Activity 2: K-NN Classifier ---
library(class)
df <- ChickWeight

# Standardize numerical data
standardize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
df_norm <- as.data.frame(lapply(df[, 1:2], standardize))

# Split Data (70/30)
set.seed(123)
sample_size <- floor(0.7 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = sample_size)

train_x <- df_norm[train_ind, ]
test_x <- df_norm[-train_ind, ]
train_y <- df$Diet[train_ind]
test_y <- df$Diet[-train_ind]

# Find Optimal K and Save Accuracy Plot
accuracy <- c()
for(i in 1:20) {
  knn_fit <- knn(train = train_x, test = test_x, cl = train_y, k = i)
  accuracy[i] <- sum(knn_fit == test_y) / length(test_y)
}

png("KNN_Accuracy_Plot.png", width = 800, height = 600)
plot(accuracy, type = "b", xlab = "K Value", ylab = "Accuracy", 
     main = "Finding Optimal K", col = "darkgreen", pch = 16)
dev.off()

# Final Model Output
optimal_k <- which.max(accuracy)
final_knn <- knn(train = train_x, test = test_x, cl = train_y, k = optimal_k)
conf_matrix <- table(Predicted = final_knn, Actual = test_y)

print(paste("Optimal K value is:", optimal_k))
print("Confusion Matrix:")
print(conf_matrix)