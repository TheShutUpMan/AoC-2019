from functools import reduce

def part1():
    print(sum(map(lambda x: len(set(x.replace("\n",""))),
                  open("6.txt").read().split('\n\n'))))

def part2():
    data = map(lambda x: x.split('\n'), open("6.txt").read().strip().split('\n\n'))
    print(sum(map(lambda x: len(reduce(lambda y,z: y.intersection(z), map(set, x))), data)))

if __name__ == "__main__":
    part2()
