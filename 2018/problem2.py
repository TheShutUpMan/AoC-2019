from collections import defaultdict

from timer import timer


@timer
def day2p1():
    with open('inputs/input2.txt') as input:
        ID = input.read().strip().split('\n')
        count2 = 0
        count3 = 0

        for id in ID:
            counter = defaultdict(int)

            for letter in id:
                counter[letter] += 1

            if 2 in counter.values():
                count2 += 1

            if 3 in counter.values():
                count3 += 1
        print(count2 * count3)


@timer
def day2p2():
    with open('inputs/input2.txt') as input:
        ID = input.read().strip().split('\n')

        for elem1 in ID:
            for elem2 in ID:
                counter = 0

                for i, s in enumerate(elem1):
                    if s != elem2[i]:
                        counter += 1

                if counter == 1:
                    print(elem1, elem2)
                    return


day2p1()
day2p2()
