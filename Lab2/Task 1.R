weight <- as.numeric(readline("Enter your weight (kg): "))
height <- as.numeric(readline("Enter your height (m): "))
bmi = weight / (height * height)

Underweight = FALSE
Normal = FALSE
Overweight = FALSE
Obese = FALSE

if (bmi <= 18.4) {
  Underweight = TRUE
} else if (bmi >= 18.5 & bmi <= 24.9) {
  Normal = TRUE
} else if (bmi >= 25 & bmi <= 39.9) {
  Overweight = TRUE
} else {
  Obese = TRUE
}

cat("Underweight:", Underweight, "\n")
cat("Normal:", Normal, "\n")
cat("Overweight:", Overweight, "\n")
cat("Obese:", Obese, "\n")

