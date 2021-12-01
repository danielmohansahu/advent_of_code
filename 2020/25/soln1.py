import csv
import copy

# not 20201225?
MOD=20201227

def transform(subject_number, loop_size):
    """ Transform the given subject_number via the given loop size.
    """
    value = 1
    for i in range(loop_size):
        value *= subject_number
        value = value%MOD
    return value

def reverse_transform(subject_number, key):
    """ Return the loop size that yields the given key from the given subject number.
    """
    value = 1
    count = 0
    while value != key:
        value *= subject_number
        value = value%MOD
        count += 1
    return count

if __name__ == "__main__":
    # load data
    card_pub = None
    door_pub = None
    with open("input.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        # read in card then door public key
        card_pub = int(next(reader)[0])
        door_pub = int(next(reader)[0])

    # part #1, determine encryption key
    card_loop = reverse_transform(7, card_pub)
    door_loop = reverse_transform(7, door_pub)

    assert(transform(card_pub, door_loop) == transform(door_pub, card_loop))
    key = transform(card_pub, door_loop)
    print("Encryption Key: {}".format(key))

