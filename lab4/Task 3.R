
array1 <- array(1:24, dim = c(2, 4, 3))  # 2 rows, 4 cols, 3 tables
print("First Array:")
print(array1)


array2 <- array(101:130, dim = c(3, 2, 5))  # 3 rows, 2 cols, 5 tables
print("Second Array:")
print(array2)



#array[row_index, column_index, table_index]


second_row_second_matrix_array1 <- array1[2, , 2]
print("Second row of the second matrix of the first array:")
print(second_row_second_matrix_array1)


element_3_3_first_matrix_array2 <- array2[3, 2, 1]
print("Element in 3rd row, 2nd column of the first matrix of the second array:")
print(element_3_3_first_matrix_array2)
