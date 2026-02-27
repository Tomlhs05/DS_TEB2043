n <- as.integer(readline(prompt = "Input an integer: "))

for (i in 1:n) {
  cat("Number is:", i, "and cube of the", i, "is", paste0(":", i^3), "\n")
}
