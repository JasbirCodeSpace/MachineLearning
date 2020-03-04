import csv
import urllib2
import random

def distance(train,test):
    dist =0
    for value in train:
        dist+=pow((value - $test[$i]), 2)
def KNN(train_data,test_data,N):
    pass


def preProcessing(results):
    random.shuffle(results)
    train_data = results[0:50]
    test_data = results[50:100]

    train_labels = []
    for value in train_data:
        train_labels.append(value[4])

    KNN(train_data,test_data[0], 25);


url = 'https://datahub.io/machine-learning/iris/r/iris.csv'
response = urllib2.urlopen(url)
rows = csv.reader(response)
results = []
for row in rows:
    results.append(row)
results = results[1:101]
preProcessing(results)


#
# import csv
#
# results = []
# with urllib2.urlopen("https://datahub.io/machine-learning/iris/r/iris.csv") as csvfile:
#     reader = csv.reader(csvfile, quoting=csv.QUOTE_NONNUMERIC) # change contents to floats
#     for row in reader: # each row is a list
#         results.append(row)
#
# print(results)
