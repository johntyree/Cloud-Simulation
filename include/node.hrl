-record(nodestate, {
        x1 = 0 :: integer(),
        y1 = 0 :: integer(),
        z1 = 0 :: integer(),
        x2 = ?GRIDSIZE_X - 1 :: integer(),
        y2 = ?GRIDSIZE_Y - 1 :: integer(),
        z2 = ?GRIDSIZE_Z - 1 :: integer(),
        drops = dict:new(),
        parent :: pid,
        deity :: pid
    }).

-record(nodeinfo, {
        state = #nodestate{},
        size :: integer(),
        volume :: float()
    }).
