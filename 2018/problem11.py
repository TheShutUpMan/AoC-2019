from multiprocessing import Pool
def make_grid(serial_number):
    grid = list()
    for i in range(300):
        row = list()
        for j in range(300):
            power_level = ((i+11)*(j+1) + serial_number) * (i+11)
            row.append((power_level % 1000) // 100 - 5)
        grid.append(row)
    return grid

def find_max_square(grid):
    max_ = (0,0)
    for i in range(1, len(grid) - 1):
        for j in range(1, len(grid[0]) - 1):
            newmax = 0
            for k in [-1,0,1]:
                for h in [-1,0,1]:
                    newmax += grid[i+k][j+h]
            if newmax > max_[0]:
                max_ = (newmax, (i,j))
    return max_

def find_max_square2(grid, gridsize):
    max_ = (0,0)
    for i in range(len(grid) - gridsize + 1):
        for j in range(len(grid[0]) - gridsize + 1):
            newmax = 0
            for k in range(gridsize):
                for h in range(gridsize):
                    newmax += grid[i+k][j+h]
            if newmax > max_[0]:
                max_ = (newmax, (i,j))
    print(max_, gridsize)
    return (max_, gridsize)
def make_take(gridsize):
    return find_max_square2(make_grid(5153), gridsize)

grid = make_grid(5153)
p = Pool(8)
findx = lambda x: find_max_square2(grid, x)
j = p.map(make_take, range(1,301))
print (max(j))

