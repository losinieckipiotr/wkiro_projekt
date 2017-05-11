import csv
import sys


def isfloat(x):
    try:
        a = float(x)
    except ValueError:
        return False
    else:
        return True


def isint(x):
    try:
        a = float(x)
        b = int(a)
    except ValueError:
        return False
    else:
        return a == b


def list_to_float(in_list):
    list_float = []
    for row in in_list:
        row_float = []
        for data in row:
            if isfloat(data) and not isint(data):
                row_float.append(float(data))
        if row_float:
            list_float.append(row_float)
    return list_float


file1 = open(sys.argv[1])
list1 = list(csv.reader(file1))
list1 = list_to_float(list1)

file2 = open(sys.argv[2])
list2 = list(csv.reader(file2))
list2 = list_to_float(list2)

if not (len(list1) == len(list2) and len(list1[0]) == len(list2[0])):
    print("Not the same size of data arrays")
    exit(1)

elements_no = 0
sum_relative_errors = 0
sum_absolute_errors = 0
for i in range(0, len(list1)):
    for j in range(0, len(list1[i])):
        if list1[i][j] == 0:
            continue
        delta = abs(list1[i][j] - list2[i][j])
        relative_error = (delta / list1[i][j]) * 100
        sum_relative_errors += relative_error
        sum_absolute_errors += delta
        elements_no += 1
average_relative_error = sum_relative_errors / elements_no
average_absolute_error = sum_absolute_errors / elements_no
print("Average absolute error of two data sets is: " + str(average_absolute_error))
print("Average relative error of two data sets is: " + str(average_relative_error) + "%")
