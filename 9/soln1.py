import csv
import copy

def is_valid(sequence, number):
    """ Returns True if the number is a sum of two discrete numbers in 
    the preceding sequence.
    """
    success = False
    for i,num in enumerate(sequence):
        if (number - num) in sequence[i:]:
            success = True
            break
    return success

if __name__ == "__main__":
    # load data
    data = []
    with open("input1.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        for row in reader:
            if (len(row) != 0):
                data.append(int(row[0]))

    # part #1: find first invalid number
    for i,num in enumerate(data):
        # skip first N numbers
        if i < 25:
            continue
        if not is_valid(data[i-25:i],num):
            print("First invalid number: {}".format(num))
            invalid = num
            break

    # part #2: iterate through the list and find the first 
    #  contiguous set that add up to the invalid number
    success = False
    for i,num in enumerate(data):
        sum_ = num
        j = i + 1
        range_ = [num]
        while True:
            # add the next number and check
            sum_ += data[j]
            range_.append(data[j])
            if sum_ > invalid:
                break
            elif sum_ == invalid:
                success = True
                print("Found sequence: ({}:{}), ({}:{})".format(i,j,num,range_[-1]))
                print("Answer: {}".format(min(range_) + max(range_)))
                
            # otherwise continue
            j += 1


        # check stop conditions
        if success:
            break

