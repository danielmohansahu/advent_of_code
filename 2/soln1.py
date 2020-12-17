import csv

if __name__ == "__main__":
    # load data
    data = []
    with open("input1.txt", "r") as csvfile:
       reader = csv.reader(csvfile)
       for row in reader:
           if len(row) != 0:
               data.append(row[0].split())

    # go through data (list of lists of conditions / passwords)
    valid = 0
    for cond, letter, password in data:
        min_,max_ = cond.split("-")
        if int(min_) <= password.count(letter[0]) <= int(max_):
            valid += 1
    print(valid)

