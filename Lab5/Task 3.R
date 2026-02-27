cat("Check whether an n digits number is Armstrong or not:\n")
cat("--------------------------------------------------------------\n")

n <- as.integer(readline(prompt = "Input an integer: "))

digits <- as.integer(strsplit(as.character(n), "")[[1]])
num_digits <- length(digits)
armstrong_sum <- sum(digits^num_digits)

if (armstrong_sum == n) {
  cat(n, "is an Armstrong number.\n")
} else {
  cat(n, "is not an Armstrong number.\n")
}