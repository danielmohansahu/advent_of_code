import numpy as np

class Square:
    def __init__(self, id_, data):
        self.id = id_
        self.data = data
        self.neighbors = {}

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

    def set_neighbor(self, side, id_):
        # lots of sanity checks; this should only get set once, etc.
        assert(side in ["top", "bottom", "left", "right"])
        assert(id_ not in self.neighbors.values())
        assert(side not in self.neighbors)
        self.neighbors[side] = id_

