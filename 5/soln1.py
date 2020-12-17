import csv

ROWS = 127
COLS = 7

def bsp_to_number(string):
    # convert string (binary space partitioning) to (row,col,num)
    row_range = [0,ROWS]
    col_range = [0,COLS]
    
    # decode row number
    for s in string[:7]:
        if s == "F":
            row_range[1] = (row_range[1] + row_range[0] + 1) / 2 - 1
        elif s == "B":
            row_range[0] = (row_range[1] + row_range[0] + 1) / 2
        else:
            print(s)
            import pdb;pdb.set_trace()

    # decode column number
    for s in string[7:]:
        if s == "L":
            col_range[1] = (col_range[1] + col_range[0] + 1) / 2 - 1
        elif s == "R":
            col_range[0] = (col_range[1] + col_range[0] + 1) / 2
        else:
            print(s)
            import pdb;pdb.set_trace()
    
    # assert expectations
    if (row_range[0] != row_range[1]) or (col_range[0] != col_range[1]):
        print(row_range)
        import pdb;pdb.set_trace()

    return (row_range[0], col_range[0], row_range[0] * 8 + col_range[0])

if __name__ == "__main__":
    # load data
    data = []
    with open("input1.txt", "r") as csvfile:
       reader = csv.reader(csvfile)
       for row in reader:
           # check if this is a new passport
           if len(row) != 0:
               data.append(row[0])

    # find largest seat ID
    ids = []
    for seat in data:
        _,_,id_ = bsp_to_number(seat)
        ids.append(id_)
    ids.sort()

    print("max seat number: {}".format(max(ids)))

    # search for the only seat between others that's missing
    candidates = []
    for i,id_ in enumerate(ids):
        # skip edges
        if i == 0 or i == len(ids) - 1:
            continue

        # check for missing seats
        if id_ - ids[i-1] == 2:
            candidates.append(id_ - 1)

    print("Potential seats: {}".format(candidates))


