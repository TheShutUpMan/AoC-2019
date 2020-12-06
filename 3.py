import functools
import operator

def part1(n, hill):
        xpos = 0
        count = 0
        row_len = len(hill[0])
        for row in hill:
            if row[xpos] == '#':
                count += 1
            xpos = (xpos + n) % row_len
        return(count)

def part2():
    count_2 = 0
    hill = None
    with open('3.txt') as f:
        hill = f.read().strip().split('\n')
    row_len = len(hill[0])
    xpos_2 = 0
    count_2 = 0
    for i,row in enumerate(hill):
        if i % 2 == 0:
            if row[xpos_2] == '#':
                count_2 += 1
            xpos_2 = (xpos_2 + 1) % row_len
    print(count_2 * part1(1,hill) * part1(3,hill) * part1(5,hill) * part1(7,hill))

if __name__ == "__main__":
    part2()

