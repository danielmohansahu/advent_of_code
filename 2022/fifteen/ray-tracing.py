#!/usr/bin/env python3
""" Ray tracing beacons to save my bacon. """

# STL
from dataclasses import dataclass

# INPUTS
# INPUT = "test.txt"
# PART_A_ROW = 10
INPUT = "input.txt"
PART_A_ROW = 2000000

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
        if iv[0] <= high:  # new interval overlaps current run
            high = max(high, iv[1])  # merge with the current run
        else:  # current run is over
            merged.append([low, high])  # yield accumulated interval
            low, high = iv  # start new run
    
    merged.append([low,high])
    print(f"{bounds} -> {merged}.")

    # count points in range
    count = 0
    for a,b in merged:
        count += (b - a)
    return count
    
if __name__ == "__main__":
    sensors = parse(INPUT)

    # Part A: check the number of points in the given row
    bounds = []
    for sensor in sensors:
        if (r := sensor.points_in_row(PART_A_ROW)) is not None:
            bounds.append(r)

    print(f"Part A: {merge_bounds(bounds)} points in Row {PART_A_ROW} are excluded.")

