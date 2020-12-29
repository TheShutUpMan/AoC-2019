from functools import reduce, lru_cache, partial
from itertools import chain, combinations, cycle, product, starmap
from collections import defaultdict, deque, namedtuple, Counter

from sympy.ntheory.modular import crt

def part1():
    with open("13.txt") as f:
        data = f.read().splitlines()
        time = int(data[0])
        busses = data[1].split(',')
        busses = [int(i) for i in busses if i != 'x']
        print(busses)
        busses_time = [(i, i-(time % i)) for i in busses]
        return min(busses_time, key=lambda x: x[1])

def check_busses(timestamp, busses):
    for n, i in enumerate(busses):
        if n == 0 or timestamp % (i + n) != 0:
            return False
    return True


def part2():
    with open("13.txt") as f:
        data = f.read().splitlines()
        busses = []
        for n, i in enumerate(data[1].split(',')):
            if i != 'x':
                busses.append((int(i), n))
    # Solution is finding t in (t%i) + n = 0 for all i,n
    crt_sol = crt(*zip(*busses))
    return crt_sol[1] - crt_sol[0]

if __name__ == "__main__":
    print(part2())
