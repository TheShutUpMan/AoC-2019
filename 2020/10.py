from collections import defaultdict

def part1():
    with open('10.txt') as f:
        adapters = sorted(map(int, f.read().strip().split('\n')))
        print(adapters)
        val = 0
        ones = 0
        threes = 1
        for i in adapters:
            if val+1 == i:
                ones += 1
            if val+3 == i:
                threes += 1
            val = i
        print(ones * threes)

seen = defaultdict(int)

def count_combinations(val, adapters):
    if seen[val]:
        return seen[val]
    elif adapters == ():
        seen[val] = 1
        return 1
    else:
        if len(adapters) > 2 and adapters[2] == val + 3:
            seen[val] = count_combinations(adapters[0], adapters[1:]) +\
                        count_combinations(adapters[1], adapters[2:]) +\
                        count_combinations(adapters[2], adapters[3:])
        elif len(adapters) > 1 and adapters[1] <= val + 3:
            seen[val] = count_combinations(adapters[0], adapters[1:]) +\
                        count_combinations(adapters[1], adapters[2:])
        else:
            seen[val] = count_combinations(adapters[0], adapters[1:])
        return seen[val]

def part2():
    with open('10.txt') as f:
        adapters = tuple(sorted(map(int, f.read().strip().split('\n'))))
        print(count_combinations(0,adapters))

if __name__ == "__main__":
    part2()
