from functools import reduce, lru_cache, partial
from itertools import chain, combinations, cycle, product, starmap, count
from collections import defaultdict, deque, namedtuple, Counter

puzzle = [int(i) for i in "784235916"]

test = [int(i) for i in "389125467"]

def doMove(labels):
    current = labels[0]
    clockwise = labels[1:4]
    destination = current
    while True:
        destination = (destination - 1) % (len(labels) + 1)
        try:
            destIx = labels.index(destination)
            if destIx < 4:
                continue
            break
        except ValueError:
            continue
    newLabels = labels[4:destIx+1] + clockwise + labels[destIx+1:] + [current]
    return newLabels

def part1():
    cups = [int(i) for i in "784235916"]
    for _ in range(100):
        cups = doMove(cups)
    print(cups)


def part2():
    cups = [int(c) for c in "784235916"]

    million_cups = cups + list(range(len(cups) + 1, 1000000 + 1))
    after_10m = run(million_cups, 10000000)
    print(after_10m[0] * after_10m[1])

def run(cups, num_iterations):
    d = {c1: c2 for c1, c2 in zip(cups, cups[1:] + [cups[0]])}
    cur = cups[0]
    for x in range(num_iterations):
        x = cur
        pickup = [x := d[x] for _ in range(3)]
        dest = next(
            cup for i in count(1)
            if (cup if (cup := cur - i) > 0 else (cup := len(cups) + cup)) not in pickup
        )

        d[cur], d[pickup[-1]], d[dest] = d[pickup[-1]], d[dest], d[cur]
        cur = d[cur]
    x = 1
    return [x := d[x] for _ in cups]

if __name__ == "__main__":
    #print(part1())
    main()
