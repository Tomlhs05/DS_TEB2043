# TASK 1
dataStudent <- data.frame(
  name = c("Anastasia","Dima","Michael","Mathhew","Laura", "Kevin", "Jonas"),
  score = c(12.5,9.0,16.5,12.0,9.0,8.0,19.0),
  attempts = c(1,3,2,3,2,1,2)  
)

# TASK 2
dataStudent$qualify <- c("yes","no","yes","no","no","no", "yes")

print(dataStudent)

# TASK 3
dataStudent <- rbind(dataStudent, data.frame(
  name = "Emily",
  score = 14.5,
  attempts = 2,
  qualify = "yes"
))

print(dataStudent)

#TASK 3
str(dataStudent)
summary(dataStudent)

dataStudent$qualify <- as.factor(dataStudent$qualify)
table(dataStudent$qualify)
