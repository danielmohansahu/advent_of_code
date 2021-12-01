import csv

required = {
    "byr": lambda x : x.isnumeric() and (1920 <= int(x) <= 2002),
    "iyr": lambda x : x.isnumeric() and (2010 <= int(x) <= 2020),
    "eyr": lambda x : x.isnumeric() and (2020 <= int(x) <= 2030),
    "hgt": lambda x : (x[-2:] == "cm" and (150 <= int(x[:-2]) <= 193)) or (x[-2:] == "in" and (59 <= int(x[:-2]) <= 76)),
    "hcl": lambda x : (x[0] == "#") and (all(c in ["0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"] for c in x[1:])),
    "ecl": lambda x : x in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
    "pid": lambda x : x.isnumeric() and len(x) == 9
}

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
        # check for each field and if it's valid
        bad = False 
        for key, req in required.items():
            if key not in passport:
                # this passport is bad
                bad = True
                break
            # otherwise check for validity
            if not req(passport[key]):
                # if key in ["hgt"]:
                #     print(key, passport[key])
                bad = True
                break
        # check if we've raised any bad flags
        if not bad:
            good += 1
    print(good)

