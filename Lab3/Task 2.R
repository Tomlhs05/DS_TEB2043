students <- list(
  Robert = 59,
  Hemsworth = 71,
  Scarlett = 83,
  Evans = 68,
  Pratt = 65,
  Larson = 57,
  Holland = 62,
  Paul = 92,
  Simu = 92,
  Renner = 59
)

scores <- unlist(students)

highest <- max(scores)
lowest <- min(scores)
average <- mean(scores)

student_highest <- names(scores)[scores == highest]
student_lowest <- names(scores)[scores == lowest]

cat("Highest Score:", highest, "\n")
cat("Lowest Score:", lowest, "\n")
cat("Average Score:", average, "\n")
cat("Student with highest score:", student_highest, "\n")
cat("Student with lowest score:", student_lowest, "\n")
