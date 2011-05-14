-record(nodestate, {
        x1 = 0,
        y1 = 0,
        z1 = 0,
        x2 = ?GRIDSIZE_X,
        y2 = ?GRIDSIZE_Y,
        z2 = ?GRIDSIZE_Z,
        drops = dict:new(),
        parent = nil}).

