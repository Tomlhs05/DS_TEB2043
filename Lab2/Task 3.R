name <- readline(prompt = "Enter your name: ")
phone <- readline(prompt = "Enter your phone number: ")

name_upper <- toupper(name)

phone_length <- nchar(phone)

first_three <- substr(phone, 1, 3)
last_four <- substr(phone, phone_length - 3, phone_length)

cat("Name in uppercase:", name_upper, "\n")
cat("Phone number (partial):", paste0(first_three, "****", last_four), "\n")
