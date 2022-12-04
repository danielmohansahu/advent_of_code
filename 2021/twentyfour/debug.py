#!/usr/bin/env python3
# helper script to analyze subroutines

def process(iter_, x_offset, y_offset, z_offset):
    """ Print out the possibilities for these offsets
    """
    print("""
Requirements for {}:
    if (Z_i % 26 + {}) == W:
        Z = Z_i / {}
    else:
        Z = 26 * Z_i / {} + W + {}""".format(
            iter_, x_offset, z_offset, z_offset, y_offset))

if __name__ == "__main__":
    # print requirements for each iteration
    process(1, 14, 12, 1)
    process(2, 15, 7, 1)
    process(3, 12, 1, 1)
    process(4, 11, 2, 1)
    process(5, -5, 4, 26)
    process(6, 14, 15, 1)
    process(7, 15, 11, 1)
    process(8, -13, 5, 26)
    process(9, -16, 3, 26)
    process(10, -8, 9, 26)
    process(11, 15, 2, 1)
    process(12, -8, 3, 26)
    process(13, 0, 3, 26)
    process(14, -4, 11, 26)

