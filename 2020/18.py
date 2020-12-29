from functools import reduce, lru_cache, partial
from itertools import chain, combinations, cycle, product, starmap
from collections import defaultdict, deque, namedtuple, Counter

def tokenize(line):
    line = line.replace(' ','')
    retLine = []
    index = 0
    while index < len(line):
        if line[index] in '+*()':
            retLine.append(line[index])
            index += 1
        else:
            num = []
            while index < len(line) and line[index] in '0123456789':
                num += line[index]
                index += 1
            retLine.append(int(''.join(num)))
    return retLine

def evaluate(line):
    values = [[]]
    stackDepth = 0
    ops = [[]]
    for current in line:
        if isinstance(current, int):
            try:
                op = ops[stackDepth].pop()
                val = values[stackDepth].pop()
                values[stackDepth].append(eval(f"{val}{op}{current}"))
            except IndexError:
                values[stackDepth].append(current)
        elif current == '(':
            stackDepth += 1
            values.append([])
            ops.append([])
        elif current == ')':
            parenVal = values[stackDepth][0]
            del values[stackDepth]
            stackDepth -= 1
            try:
                op = ops[stackDepth].pop()
                val = values[stackDepth].pop()
                values[stackDepth].append(eval(f"{val}{op}{parenVal}"))
            except IndexError:
                values[stackDepth].append(parenVal)
        elif current == "+":
            ops[stackDepth].append('+')
        elif current == "*":
            ops[stackDepth].append('*')
    return values[0][0]

def eval_precedence(maths):
    if isinstance(maths,str):
        while "(" in maths:
            deepest = None
            for i in range(len(maths)):
                if maths[i] == "(":
                    deepest = i
                if maths[i] == ")":
                    maths = maths[:deepest] + eval_precedence(maths[deepest+1:i]) + maths[i+1:]
                    break
        maths = deque(maths.split(" "))
    while "+" in maths:
        plusPos = maths.index("+")
        a = int(maths[plusPos-1])
        b = int(maths[plusPos+1])
        maths[plusPos] = str(a + b)
        del maths[plusPos + 1]
        del maths[plusPos - 1]
    while len(maths) != 1:
        a = int(maths.popleft())
        op = maths.popleft()
        b = int(maths.popleft())
        if op == "*":
            maths.insert(0,str(a * b))
        elif op == "+":
            assert False
    return maths[0]

def part1():
    with open("18.txt") as f:
        data = f.read().splitlines()
        sum_ = 0
        for i in data:
            sum_ += evaluate(tokenize(i))
        return sum_

def part2():
    with open("18.txt") as f:
        data = f.read().splitlines()
        sum_ = 0
        for i in data:
            sum_ += int(eval_precedence(i))
        return sum_

if __name__ == "__main__":
    print(part1())
    print(part2())
