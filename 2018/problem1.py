from collections import defaultdict

from timer import timer


@timer
def day1p1():
    with open('inputs/input1.txt') as input:
        text = str(input.read())
        numbers = list(map(int, text.strip().split('\n')))
        print(sum(numbers))


@timer
def day1p2():
    with open('inputs/input1.txt') as input:
        text = str(input.read())
        numbers = list(map(int, text.strip().split('\n')))
        numbers_dict = defaultdict(int)
        counter = 0
        index = -1

        while True:
            index = (index + 1) % len(numbers)
            numbers_dict[counter] += 1

            if numbers_dict[counter] == 2:
                print(counter)

                break
            counter += numbers[index]


day1p1()
day1p2()
