import copy
import numpy as np

class Square:
    def __init__(self, id_, data):
        self.id = id_
        self.data = data
        self.neighbors = {}

        # useful constants
        self.rotation_map = {
            "top": "left",
            "right": "top",
            "bottom": "right",
            "left": "bottom"
        }
        self.flip_map = {
            0: {
                "top": "bottom",
                "bottom": "top",
                "left": "left",
                "right": "right"},
            1: {
                "top": "top",
                "bottom": "bottom",
                "left": "right",
                "right": "left"}
        }


    def is_corner(self):
        """ Return true if we have exactly two adjacent squares.
        """
        neighbors = len(self.neighbors)

        # sanity check that we've been initialized
        try:
            assert(0 < neighbors <= 4)
        except:
            import pdb;pdb.set_trace()
        return (neighbors == 2)

    def is_side(self):
        """ Return true if we have exactly two adjacent squares.
        """
        neighbors = len(self.neighbors)
        # sanity check that we've been initialized
        assert(0 < neighbors <= 4)
        return (neighbors == 3)

    def edges(self):
        """ Return a list of all possible edges.
        """
        return set([self.top(), self.bottom(), self.left(), self.right()])

    def top(self):
        return "".join("1" if d else "0" for d in self.data[0,:])

    def bottom(self):
        return "".join("1" if d else "0" for d in self.data[-1,:])

    def left(self):
        return "".join("1" if d else "0" for d in self.data[:,0])

    def right(self):
        return "".join("1" if d else "0" for d in self.data[:,-1])

    def adjacent(self, side, strict=False):
        """ Check if the given side is one of our edges.

        If not 'strict' we allow flipped edges
        """
        if strict:
            return side in self.edges()
        else:
            return (side in self.edges()) or (side[::-1] in self.edges())

    def set_neighbor(self, side, id_):
        # lots of sanity checks; this should only get set once, etc.
        assert(side in ["top", "bottom", "left", "right"])
        assert(id_ not in self.neighbors.values())
        assert(side not in self.neighbors)
        self.neighbors[side] = id_

    def flip(self, axis):
        """ Return a copy flipped about the given axis
        """
        assert(axis in [0,1])

        clone = Square(self.id, np.flip(self.data,axis))
        # orient neighbors as well
        clone.neighbors = {self.flip_map[axis][k]:v for k,v in self.neighbors.items()}
        return clone

    def rotate(self):
        """ Return a copy rotated by 90 degrees (CCW)
        """
        clone = Square(self.id, np.rot90(self.data))
        # orient neighbors as well
        clone.neighbors = {self.rotation_map[k]:v for k,v in self.neighbors.items()}
        return clone

    def copy(self):
        """ Return a copy.
        """
        clone = Square(self.id, self.data)
        clone.neighbors = self.neighbors
        return clone


