import pandas as pd
import numpy as np
import datetime
import igraph
import matplotlib.pyplot as plt

swipes = pd.read_csv("swipes_w_day.csv")
swipes['date_time'] = pd.to_datetime(swipes['date_time'])
students = pd.read_csv("students_updated1.csv")

fy_swipes = swipes[swipes['class_year'] == 2015]
fy_swipes_smaller = fy_swipes[0:30000]
fy_students = students[students['CLASS_YR'] == 2015]
num_fy_students = len(fy_students)

start_row = [0]*num_fy_students
adj_matrix = np.array(start_row)
total_matrix = np.array(start_row)

for i in range(1, num_fy_students):
    adj_matrix = np.vstack([adj_matrix, start_row])
    total_matrix = np.vstack([total_matrix, start_row])

# create mapping from student id to index
id_to_index = dict()
index_to_id = dict()

for i in range(num_fy_students):
    current_student = fy_students.iloc[i]
    student_id = current_student['Student']
    id_to_index[student_id] = i
    index_to_id[i] = student_id

fy_swipes['index'] = fy_swipes['student_id'].map(id_to_index)

delta = datetime.timedelta(0, 25)

for i in range(len(fy_swipes)):
    row = fy_swipes.iloc[i]
    current_student = row['index']
    current_time = row['date_time']
    prev_time = current_time - delta
    forward_time = current_time + delta

    interval = fy_swipes[(fy_swipes['date_time'] >= prev_time) & (fy_swipes['date_time'] <= forward_time)]

    for j in range(len(interval)):
        interval_row = interval.iloc[j]
        interval_index = interval_row['index']
        if current_student != interval_index:
            adj_matrix[current_student, interval_index] = 1
            total_matrix[current_student, interval_index] += 1

print('generating graph')

print(total_matrix)

g = igraph.Graph.Adjacency(adj_matrix.tolist())
# g.es['weight'] = total_matrix[total_matrix.nonzero()]
layout = g.layout_drl()
fig, ax = plt.subplots()
igraph.plot(g, layout=layout)
plt.show()
