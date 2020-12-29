from functools import reduce, lru_cache, partial
from itertools import chain, combinations, cycle, product, starmap
from collections import defaultdict, deque, namedtuple, Counter
from re import sub

def in_range(tup, value):
    f1,f2 = tup
    low1,high1 = f1
    low2,high2 = f2
    if (low1<=value<=high1) or (low2<=value<=high2):
        return True
    return False

def part1():
    data = []
    with open("16.txt") as f:
        data = f.read().strip().split('\n\n')
    fields = []
    for n, i in enumerate(data):
        data[n] = data[n].split('\n')
    for i in data[0]:
        value_ranges = sub('.*: ','', i).split(' or ')
        for n,j in enumerate(value_ranges):
            value_ranges[n] = tuple(map(int, j.split('-')))
        fields.append(value_ranges)
    tickets = [tuple(map(int, i.split(','))) for i in data[2][1:]]
    invalid = []
    for ticket in tickets:
        for value in ticket:
            valid = False
            for f in fields:
                valid = valid or in_range(f,value)
            if not valid:
                invalid.append(value)
    return(sum(invalid))

def part2():
    data = []
    with open("16.txt") as f:
        data = f.read().strip().split('\n\n')
    for n, i in enumerate(data):
        data[n] = data[n].split('\n')

    # Parse fields
    fields = []
    for i in data[0]:
        value_ranges = sub('.*: ','', i).split(' or ')
        for n,j in enumerate(value_ranges):
            value_ranges[n] = tuple(map(int, j.split('-')))
        fields.append(value_ranges)

    my_ticket = tuple(map(int, data[1][1].split(',')))

    # Finding valid tickets - all values in (some) range
    tickets = [tuple(map(int, i.split(','))) for i in data[2][1:]]
    valid_tickets = []
    for ticket in tickets:
        valid_t = True
        for value in ticket:
            valid = False
            for f in fields:
                valid = in_range(f, value) or valid
            if not valid:
                valid_t = False
                break
        if valid_t:
            valid_tickets.append(ticket)

    # Finding valid indices for each field
    valid_indices = []
    for f in fields:
        current_indices = set()
        for ix in range(len(fields)):
            valid = True
            for ticket in valid_tickets:
                if not in_range(f,ticket[ix]):
                    valid = False
                    break
            if valid:
                current_indices.add(ix)
        valid_indices.append(current_indices)

    # Figure out actual indices from index lists
    actual_indices = set()
    while True:
        empty = True
        for n,i in enumerate(valid_indices):
            if len(i) == 1:
                empty = False
                i_copy = set(i)
                ix = i.pop()
                for j in valid_indices:
                    j.difference_update(i_copy)
                actual_indices.add((n,ix))
        if empty:
            break
    actual_indices = list(map(lambda x: x[1], sorted(list(actual_indices))))
    departures = [n for n,i in enumerate(data[0]) if i.startswith('departure')]
    mult = 1
    for i in departures:
        mult *= my_ticket[actual_indices[i]]
    return mult

if __name__ == "__main__":
    print(part1())
    print(part2())
