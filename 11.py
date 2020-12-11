import copy

def count_occupied(rows):
    print(rows)
    count = 0
    for row in rows:
        for elem in row:
            if elem == "#":
                count += 1
    return count

adjacent = [(1,0),(0,1),(-1,0),(0,-1),(1,1),(-1,-1),(-1,1),(1,-1)]

def generation(rows):
    new_rows = []
    for i, row in enumerate(rows):
        new_rows.append([])
        for j, seat in enumerate(row):
            if seat == "L":
                adjacent_occupied = False
                for i_, j_ in adjacent:
                    if 0 <= i + i_ < len(rows) and 0 <= j + j_ < len(row):
                        if rows[i+i_][j+j_] == '#':
                            adjacent_occupied = True
                if not adjacent_occupied:
                    new_rows[i].append('#')
                else:
                    new_rows[i].append('L')
            elif seat == '#':
                adjacent_count = 0
                for i_, j_ in adjacent:
                    if 0 <= i + i_ < len(rows) and 0 <= j + j_ < len(row):
                        if rows[i+i_][j+j_] == '#':
                            adjacent_count += 1
                if adjacent_count >= 4:
                    new_rows[i].append('L')
                else:
                    new_rows[i].append('#')
            else:
                new_rows[i].append('.')
    return new_rows

def get_seat(position, increment, seats):
    (i_,j_) = increment
    (i,j) = position
    (i,j) = (i+i_,j+j_)
    while 0 <= i < len(seats) and 0 <= j < len(seats[i]):
        if seats[i][j] == "#":
            return True
        elif seats[i][j] == "L":
            return False
        (i,j) = (i+i_,j+j_)
    return False

def generation_2(rows):
    new_rows = []
    for i, row in enumerate(rows):
        new_rows.append([])
        for j, seat in enumerate(row):
            if seat == "L":
                adjacent_occupied = False
                for i_, j_ in adjacent:
                    if get_seat((i,j),(i_,j_),rows):
                            adjacent_occupied = True
                if not adjacent_occupied:
                    new_rows[i].append('#')
                else:
                    new_rows[i].append('L')
            elif seat == '#':
                adjacent_count = 0
                for i_, j_ in adjacent:
                    if get_seat((i,j),(i_,j_),rows):
                        adjacent_count += 1
                if adjacent_count >= 5:
                    new_rows[i].append('L')
                else:
                    new_rows[i].append('#')
            else:
                new_rows[i].append('.')
    return new_rows



def part1():
    with open('11.txt') as f:
        data = list(map(list, f.read().splitlines()))
        data_next = generation(data)
        while data_next != data:
            data = data_next
            print(len(data))
            data_next = generation(data)
        return(count_occupied(data))


def part2():
    with open('11.txt') as f:
        data = list(map(list,f.read().splitlines()))
        data_next = generation(data)
        while data_next != data:
            data = data_next
            print(len(data))
            data_next = generation_2(data)
        return(count_occupied(data))


if __name__ == "__main__":
    print(part2())
