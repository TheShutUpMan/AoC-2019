from functools import reduce, lru_cache, partial
from itertools import chain, combinations, cycle, product, starmap
from collections import defaultdict, deque, namedtuple, Counter

def part1():
    cardpk = 13316116
    doorpk = 13651422

    card_num = 0
    card_found = False
    door_num = 0
    door_found = False
    value = 1
    subject_number = 7
    while True:
        value *= subject_number
        value = (value % 20201227)
        if not card_found:
            card_num += 1
            if value == cardpk:
                card_found = True
                if door_found:
                    break
        if not door_found:
             door_num += 1
             if value == doorpk:
                 door_found = True
                 if card_found:
                     break

    encryption_key = 1
    for _ in range(door_num):
        encryption_key *= cardpk
        encryption_key %= 20201227

    enc2 = 1
    for _ in range(card_num):
        enc2 *= doorpk
        enc2 %= 20201227

    return (encryption_key, enc2)

def part2():
    return None

if __name__ == "__main__":
    print(part1())
    print(part2())
