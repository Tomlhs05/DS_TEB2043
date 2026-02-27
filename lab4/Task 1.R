age = c(55,57,56,52,51,59,58,53,59,55,60,60,60,60,52,55,56,51,60,
        52,54,56,52,57,54,56,58,53,53,50,55,51,57,60,57,55,51,50,57,58);

occurence <- table(age)
print(occurence)

age_ranges <- cut(age, breaks = c(49, 52, 54, 56, 58, 60), right = TRUE)

#cut divides numeric data into bins(ranges)
#breaks defines the ranges of these bins
"

49 < age ≤ 52 → ages 50, 51, 52

52 < age ≤ 54 → ages 53, 54

54 < age ≤ 56 → ages 55, 56

56 < age ≤ 58 → ages 57, 58

58 < age ≤ 60 → ages 59, 60



"



table(age_ranges)
