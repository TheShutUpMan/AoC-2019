from collections import namedtuple, queue
import numpy as np

with open('inputs/input6.txt') as file:
    coordinates = file.read().strip().split('\n')
    points = [[int(x), int(y)] for x,y in [x.split(', ') for x in coordinates]]
    minX = min(points, key = lambda x:x[0])[0]
    minY = min(points, key = lambda x:x[1])[1]
    maxX = max(points, key = lambda x:x[0])[0]
    maxY = max(points, key = lambda x:x[1])[1]
    for point in points:
        point[0] -= minX
        point[1] -= minY
    maxX -= minX
    maxY -= minY

    nodedt = np.dtype([('dist', np.int32), ('point', np.int32)])
    array = np.zeros((maxX+1, maxY+1), dtype=nodedt)
    fringe = queue()
    Node_ = namedtuple("Node", "coords dist point")
    for point in enumerate(points):
        fringe.add(np.array(point, dt=nodedt))

