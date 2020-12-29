import pdb

pdb.set_trace()


def day5p1():
    with open('input5.txt') as file:
        text = file.read().strip()
        correspondance = {}
        for i in "abcdefghijklmnopqrstuvwxyz":
            correspondance[i] = i.upper()
            correspondance[i.upper()] = i
        index = 0
        print(correspondance)
        while index != len(text) - 1:
            if text[index] == correspondance[text[index + 1]]:
                text = text[:index] + text[index + 2:]
                if index > 0:
                    index -= 1
            else:
                index += 1
            print(index)
        print(len(text))


def day5p2():
    with open('input5.txt') as file:
        text = file.read().strip()
        correspondance = {}
        for i in "abcdefghijklmnopqrstuvwxyz":
            correspondance[i] = i.upper()
            correspondance[i.upper()] = i
        minlength = 100000
        for i in correspondance.keys():
            textcpy = text[:]
            textcpy = textcpy.replace(i, '')
            textcpy = textcpy.replace(correspondance[i], '')
            index = 0
            while index != len(textcpy) - 1:
                if textcpy[index] == correspondance[textcpy[index + 1]]:
                    textcpy = textcpy[:index] + textcpy[index + 2:]
                    if index > 0:
                        index -= 1
                else:
                    index += 1
            if len(textcpy) < minlength:
                minlength = len(textcpy)
        print(minlength)


day5p2()
