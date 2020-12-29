import numpy as np
from timer import timer

@timer
def day3p1():
    with open('inputs/input3.txt') as input:
        entries = input.read().strip().split('\n')
        fabric = np.zeros((1000,1000))
        counter = 0
        for entry in entries:
            _, _, loc, dim = entry.strip().split()
            xpos, ypos = map(int, loc.replace(':', '').split(','))
            height, width = map(int, dim.split('x'))
            for i in range(height):
                for j in range(width):
                    if fabric[i+xpos, j+ypos] == 1:
                        fabric[i+xpos, j+ypos] = 2
                        counter += 1
                    elif fabric[i+xpos, j+ypos] == 2:
                        continue
                    else:
                        fabric[i+xpos, j+ypos] = 1
        print(counter)
@timer
def day3p2():
    with open('inputs/input3.txt') as input:
        entries = input.read().strip().split('\n')
        fabric = np.zeros((1000,1000))
        ID_dict = {i:1 for i in range(1, len(entries))}
        for entry in entries:
            ID, _, loc, dim = entry.strip().split()
            ID = int(ID.replace('#',''))
            xpos, ypos = map(int, loc.replace(':', '').split(','))
            height, width = map(int, dim.split('x'))
            for i in range(height):
                for j in range(width):
                    if fabric[i+xpos, j+ypos] != 0:
                        ID_dict[ID] = 0
                        ID_dict[fabric[i+xpos, j+ypos]] = 0
                    else:
                        fabric[i+xpos, j+ypos] = ID
        print([i for i in range(1, len(entries)) if ID_dict[i] == 1])

day3p1()
day3p2()
