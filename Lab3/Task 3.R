students <- list(
  Robert = c(Chemistry = 59, Physics = 89),
  Hemsworth = c(Chemistry = 71, Physics = 86),
  Scarlett = c(Chemistry = 83, Physics = 65),
  Evans = c(Chemistry = 68, Physics = 52),
  Pratt = c(Chemistry = 65, Physics = 60),
  Larson = c(Chemistry = 57, Physics = 67),
  Holland = c(Chemistry = 62, Physics = 40),
  Paul = c(Chemistry = 92, Physics = 77),
  Simu = c(Chemistry = 92, Physics = 90),
  Renner = c(Chemistry = 59, Physics = 61)
)

chemistry_scores <- sapply(students, function(x) x["Chemistry"])
physics_scores <- sapply(students, function(x) x["Physics"])

chem_fail_count <- sum(chemistry_scores <= 49)
phys_fail_count <- sum(physics_scores <= 49)

chem_highest <- max(chemistry_scores)
phys_highest <- max(physics_scores)

chem_top_students <- names(chemistry_scores)[chemistry_scores == chem_highest]
phys_top_students <- names(physics_scores)[physics_scores == phys_highest]

cat("Number of students failed Chemistry:", chem_fail_count, "\n")
cat("Number of students failed Physics:", phys_fail_count, "\n")
cat("Highest Chemistry score:", chem_highest, "\n")
cat("Top student(s) in Chemistry:", chem_top_students, "\n")
cat("Highest Physics score:", phys_highest, "\n")
cat("Top student(s) in Physics:", phys_top_students, "\n")
