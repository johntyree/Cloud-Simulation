-record(nodestate, {
        x1 = 0,
        y1 = 0,
        z1 = 0,
        x2 = ?GRIDSIZE_X - 1,
        y2 = ?GRIDSIZE_Y - 1,
        z2 = ?GRIDSIZE_Z - 1,
        drops = dict:new(),
        parent,
        deity
    }).

