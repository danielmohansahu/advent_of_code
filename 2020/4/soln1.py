import csv

required_keys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] # "cid"
required_keys.sort()
required_keys = set(required_keys)

if __name__ == "__main__":
    # load data
    data = []
    with open("input1.txt", "r") as csvfile:
       reader = csv.reader(csvfile)
       line = []
       for row in reader:
           # check if this is a new passport
           if len(row) == 0:
               data.append(line)
               line = []
           else:
               # otherwise append to our growing line
               line.append(row[0].split())

    # flatten list into dictionary
    passports = []
    for sublist in data:
        passport = {}
        for list_ in sublist:
            for item in list_:
                key,value = item.split(":")
                passport[key] = value
        passports.append(passport)
            
    # check which passports are correct
    good = 0
    for passport in passports:
        keys = passport.keys()
        keys.sort()
        keys = set(keys)
        if len(required_keys - keys) == 0:
            good += 1
        else:
            print(required_keys-keys,keys-required_keys)
    print(good)

