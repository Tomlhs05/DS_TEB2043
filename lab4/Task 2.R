

V1 = c(2,3,1,5,4,6,8,7,9)
mat1 <- matrix(V1, nrow = 3, ncol = 3)
print(mat1)

####transposing mat1 to be a new matrix, mat2



mat2 <- t(mat1)
print(mat2)


################
print(mat1+mat2)
print(mat1-mat2)
print(mat1*mat2)
print(mat1/mat2)