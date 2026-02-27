scores <- c(33, 24, 54, 94, 16, 89, 60, 6, 77, 61, 13, 44, 26, 24, 73, 73, 90, 39, 90, 54)

grades <- character(length(scores))

grades[scores >= 90 & scores <= 100] <- "A"
grades[scores >= 80 & scores <= 89] <- "B"
grades[scores >= 70 & scores <= 79] <- "C"
grades[scores >= 60 & scores <= 69] <- "D"
grades[scores >= 50 & scores <= 59] <- "E"
grades[scores <= 49] <- "F"

grade_count <- table(grades)

pass <- scores > 49

grade_count
pass
