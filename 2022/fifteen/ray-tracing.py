#!/usr/bin/env python3
""" Ray tracing beacons to save my bacon. """

# STL
from dataclasses import dataclass

# INPUTS
# INPUT = "test.txt"
# PART_A_ROW = 10
# PART_B_BOUND = 20
# PART_B_MULTIPLE = 4000000
INPUT = "input.txt"
PART_A_ROW = 2000000
PART_B_BOUND = 4000000
PART_B_MULTIPLE = 4000000

@dataclass
class Sensor:
    x: int      # sensor x coordinate (column)
    y: int      # sensor y coordinate (row)
    b_x: int    # beacon x coordinate (column)
    b_y: int    # beacon y coordinate (row)
    d: int      # sensor's range (manhattan distance)

    # return true if the given point is within range
    def in_range(self, x, y):
        return abs(x - self.x) + abs(y - self.y) <= self.d

    # return the point range covered in this row, or None
    def points_in_row(self, y):
        diff = self.d - abs(y - self.y)
        if diff < 0:
            return None
        return [self.x - diff, self.x + diff]

def parse(filename):
    """ Parse the input and return a list of sensors. """
    sensors = []
    with open(filename,"r") as inputfile:
        for line in inputfile.readlines():
            parsed = line.replace("Sensor at x=", "").replace(", y=", " ").replace(": closest beacon is at x=", " ").replace(", y=", " ")
            x,y,bx,by = [int(c) for c in parsed.split()]
            sensors.append(Sensor(x,y,bx,by,abs(bx-x) + abs(by-y)))
    return sensors

def merge_bounds(bounds):
    """ Eliminate overlap in the given bounds, and return the total point count within.
     https://codereview.stackexchange.com/questions/69242/merging-overlapping-intervals
    """
    # sort input by start index
    bounds.sort(key=lambda x: x[0])
    merged = []

    # low and high represent the bounds of the current run of merges
    low, high = bounds[0]
    
    for iv in bounds[1:]:
        if iv[0] <= high + 1:  # new interval overlaps current run
            high = max(high, iv[1])  # merge with the current run
        else:  # current run is over
            merged.append([low, high])  # yield accumulated interval
            low, high = iv  # start new run
    
    merged.append([low,high])
    return merged

def get_bounds_for_row(row, sensors):
    bounds = []
    for sensor in sensors:
        if (r := sensor.points_in_row(row)) is not None:
            bounds.append(r)
    return merge_bounds(bounds)

if __name__ == "__main__":
    sensors = parse(INPUT)

    # Part A: check the number of points in the given row
    bounds = get_bounds_for_row(PART_A_ROW, sensors)
    count = 0
    for a,b in bounds:
        count += (b - a)

    print(f"Part A: {count} points in Row {PART_A_ROW} are excluded.")

    # For part B we need to iterate through all rows in our range and find the 
    #  only one with a viable candidate
    candidate = None
    for row in range(PART_B_BOUND + 1):
        bounds = get_bounds_for_row(row, sensors)
        # check if there's an uncovered spot in this row
        for a,b in bounds:
            if a > 0:
                print(f"Part B candidate: ({a-1},{row})")
                candidate = (a-1, row)
            elif b < PART_B_BOUND:
                print(f"Part B candidate: ({b+1},{row})")
                candidate = (b+1, row)
    freq = candidate[0] * PART_B_MULTIPLE + candidate[1]
    print(f"Part B: Found signal at {candidate} with frequency {freq}.")

                






















