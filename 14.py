from functools import reduce, lru_cache, partial
from itertools import chain, combinations, cycle, product, starmap
from collections import defaultdict, deque, namedtuple, Counter

def interpret_bitmask(mask):
    ones = mask.replace("X", "0")
    zeros = mask.replace("X", "1")
    return(int(ones,2), int(zeros,2))

def apply_bitmask(mask, num):
    ones, zeros = mask
    return (num | ones) & zeros

def part1():
    with open("14.txt") as f:
        data = f.read().splitlines()
        data = [i.split(' = ') for i in data]
        mask = interpret_bitmask(data[0][1])
        mem = dict()
        for instr, i in data:
            if instr == "mask":
                mask = interpret_bitmask(i)
            else:
                index = int(instr[4:-1])
                mem[index] = apply_bitmask(mask, int(i))
        return sum(mem.values())

values = {"1":["1"], "0":["_"], "X":["1","0"]}
def interpret_bitmask_2(mask):
    if len(mask) == 1:
        return values[mask]
    else:
        return [x + y for x in values[mask[0]] for y in interpret_bitmask_2(mask[1:])]

def apply_bitmask_2_once(mask, num):
    ones = mask.replace("_", "0")
    zeros = mask.replace("_", "1")
    return (num | int(ones,2)) & int(zeros,2)

def apply_bitmask_2(masks, num):
    return map(lambda x: apply_bitmask_2_once(x, num), masks)

def part2():
    with open("14.txt") as f:
        data = f.read().splitlines()
        data = [i.split(' = ') for i in data]
        mask = ""
        mem = dict()
        for instr, i in data:
            if instr == "mask":
                mask = interpret_bitmask_2(i)
            else:
                index = int(instr[4:-1])
                for ix in apply_bitmask_2(mask, index):
                    mem[ix] = int(i)
        return sum(mem.values())


if __name__ == "__main__":
    print(part2())
