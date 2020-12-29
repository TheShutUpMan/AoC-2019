from functools import reduce, lru_cache, partial
from itertools import chain, combinations, cycle, product, starmap
from collections import defaultdict, deque, namedtuple, Counter
from pprint import pp
neighbors = [(i,j,k) for i in (-1,0,1) for j in (-1,0,1) for k in (-1,0,1)
             if i != 0 or j != 0 or k != 0]

hyperneighbors = [(x,y,z,w) for x in (-1,0,1) for y in (-1,0,1) for z in (-1,0,1)
                  for w in (-1,0,1) if x != 0 or y != 0 or z != 0 or w != 0]

def inRange(dims, indices):
    result = True
    for n,d in enumerate(dims):
        result = result and (0 <= indices[n] < d)
    return result

def countActive(state):
    count = 0
    for plane in state:
        for row in plane:
            for item in row:
                if item == '#':
                    count += 1
    return count

def doCycle(state):
    dimZ = len(state)
    dimY = len(state[0])
    dimX = len(state[0][0])
    dims = (dimX,dimY,dimZ)
    newState = []
    for z in range(dimZ + 2):
        plane = []
        for y in range(dimY+2):
            row = []
            for x in range(dimX+2):
                count = 0
                for i,j,k in neighbors:
                    xi,yj,zk = x+i-1,y+j-1,z+k-1
                    if inRange(dims, (xi,yj,zk)):
                        if state[zk][yj][xi] == '#':
                            count += 1
                if not inRange(dims, (x-1,y-1,z-1)) or state[z-1][y-1][x-1] == '.':
                    if count == 3:
                        row += '#'
                    else:
                        row += '.'
                else:
                    if count == 2 or count == 3:
                        row += '#'
                    else:
                        row += '.'
            plane.append(row)
        newState.append(plane)
    return newState

def hyperCountActive(state):
    count = 0
    for cube in state:
        for plane in cube:
            for row in plane:
                for item in row:
                    if item == '#':
                        count += 1
    return count

def hyperCycle(state):
    dimW = len(state)
    dimZ = len(state[0])
    dimY = len(state[0][0])
    dimX = len(state[0][0][0])
    dims = (dimX,dimY,dimZ,dimW)
    print(dims)
    newState = []
    for w in range(dimW + 2):
        cube = []
        for z in range(dimZ + 2):
            plane = []
            for y in range(dimY+2):
                row = []
                for x in range(dimX+2):
                    count = 0
                    for i,j,k,l in hyperneighbors:
                        xi,yj,zk,wl = x+i-1,y+j-1,z+k-1,w+l-1
                        if inRange(dims, (xi,yj,zk,wl)):
                            if state[wl][zk][yj][xi] == '#':
                                count += 1
                    if not inRange(dims, (x-1,y-1,z-1,w-1)) or state[w-1][z-1][y-1][x-1] == '.':
                        if count == 3:
                            row += '#'
                        else:
                            row += '.'
                    else:
                        if count == 2 or count == 3:
                            row += '#'
                        else:
                            row += '.'
                plane.append(row)
            cube.append(plane)
        newState.append(cube)
    return newState

def part1():
    with open("17.txt") as f:
        state = [f.read().splitlines()]
        for _ in range(6):
            state = doCycle(state)
            for plane in state:
               for row in plane:
                   print(''.join(row))
               print('\n')
        return countActive(state)

def part2():
    with open('17.txt') as f:
        state = [[f.read().splitlines()]]
        print(state)
        for _ in range(6):
            state = hyperCycle(state)
            for cube in state:
                for plane in cube:
                    for row in plane:
                        print(''.join(row))
                print()
        return hyperCountActive(state)

if __name__ == "__main__":
    print(part2())
