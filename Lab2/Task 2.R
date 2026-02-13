
str1 <- readline("Enter first string: ")
str2 <- readline("Enter second string: ")

str1 <- tolower(str1)
str2 <- tolower(str2)


result <- FALSE


if (str1 == str2) {
  result <- TRUE
}

cat("This program compares 2 strings. Both inputs are similar:", result, "\n")
