from functools import reduce, lru_cache, partial
from itertools import chain, combinations, cycle, product, starmap
from collections import defaultdict, deque, namedtuple, Counter

def part1(count):
    numbers = defaultdict(int)
    nums = [14,3,1,0,9,5]
    counter = 0
    turn = 1
    last = (0,False)
    current = 0
    for i in nums:
        numbers[i] = turn
        counter += 1
        turn += 1
        last = (i, False)
    while turn <= count:
        lastN, lastNew = last
        if not lastNew:
            current = 0
        else:
            current = (numbers[lastN] - lastNew)

        if numbers[current]:
            last = (current, numbers[current])
        else:
            last = (current, False)
            counter += 1
        numbers[current] = turn
        turn += 1
    return current

def part2():
    return part1(30000000)
if __name__ == "__main__":
    print(part2())
