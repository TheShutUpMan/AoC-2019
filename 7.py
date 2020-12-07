from collections import namedtuple
import queue

Bag = namedtuple('Bag', ['colour', 'count'])

def to_bag(string):
    if string == "no other":
        return None
    words = string.split(' ')
    count = int(words[0])
    colour = words[1] + " " + words[2]
    return Bag(colour, count)

def find_bag(bag, bags):
    if bag == "shiny gold":
        return True
    for i in bags[bag]:
        if i is not None and find_bag(i.colour, bags):
            return True
    return False

def part1():
    with open("7.txt") as f:
        text = f.read().strip().replace(' bags', '').replace(' bag','').replace('.','')
        lines = [x.split(" contain ") for x in text.split('\n')]
        bags = {x[0]: list(map(to_bag, x[1].split(', '))) for x in lines}
        found_count = 0
        for bag in bags:
            if find_bag(bag, bags):
                found_count += 1
        print(found_count -1 )

def bag_count(bag_col, bags):
    count = 1
    for bag in bags[bag_col]:
        if bag is not None:
            count += bag.count * bag_count(bag.colour, bags)
    return count

def part2():
    with open("7.txt") as f:
        text = f.read().strip().replace(' bags', '').replace(' bag','').replace('.','')
        lines = [x.split(" contain ") for x in text.split('\n')]
        bags = {x[0]: list(map(to_bag, x[1].split(', '))) for x in lines}
        print(bag_count("shiny gold", bags) - 1)

if __name__ == "__main__":
    part1()
