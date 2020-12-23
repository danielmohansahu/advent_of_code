import numpy as np

class Square:
    def __init__(self, id_, data):
        self.id = id_
        self.data = data
        self.neighbors = []

    def edges(self):
        """ Return a list of all possible edge hashes.
        """
        shape = self.data.shape[0]
        opts = set([
            hash(str(self.data[0,:shape])),
            hash(str(self.data[shape-1,:shape])),
            hash(str(self.data[:shape,0])),
            hash(str(self.data[:shape,shape-1])),
        ])
        return opts

    def adjacent(self, other):
        """ Return True if this edge is adjacent to another square.
        """
        # sanity check
        assert(self.id != other.id)

        # compare edges
        common_edges = set.intersection(self.edges(),other.edges())
        assert(len(common_edges) <= 1)
        
        # save this, if adjacent
        if len(common_edges) == 1:
            self.neighbors.append(other.id)
            return True

        return False

    def flip(self, axis):
        """ Return a flipped copy of this square along the desired axis.
        """
        return Square(self.id, np.flip(self.data,axis))

        
