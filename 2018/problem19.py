from collections import namedtuple

with open('inputs/input19.txt') as file:
    pointer_reg = int(file.readline()[4])
    program = file.read().strip().split('\n')
    instructions = {
        'addi': lambda a, b, x: x[a] + b,
        'addr': lambda a, b, x: x[a] + x[b],
        'mulr': lambda a, b, x: x[a] * x[b],
        'muli': lambda a, b, x: x[a] * b,
        'banr': lambda a, b, x: x[a] & x[b],
        'bani': lambda a, b, x: x[a] & b,
        'borr': lambda a, b, x: x[a] | x[b],
        'bori': lambda a, b, x: x[a] | b,
        'setr': lambda a, b, x: x[a],
        'seti': lambda a, b, x: a,
        'gtir': lambda a, b, x: 1 if a > x[b] else 0,
        'gtri': lambda a, b, x: 1 if x[a] > b else 0,
        'gtrr': lambda a, b, x: 1 if x[a] > x[b] else 0,
        'eqir': lambda a, b, x: 1 if a == x[b] else 0,
        'eqri': lambda a, b, x: 1 if x[a] == b else 0,
        'eqrr': lambda a, b, x: 1 if x[a] == x[b] else 0,
    }
    Instruction = namedtuple('Instruction', 'name params')
    for i, line in enumerate(program):
        line = line.split(' ')
        program[i] = Instruction(line[0], tuple(map(int, line[1:])))

    regs = [1, 0, 0, 0, 0, 0]

    def exec_instruction(instr, a, b, c, x):
        xn = x[:]
        xn[c] = instr(a, b, x)
        return xn

    while True:
        ic = regs[pointer_reg]
        try:
            instruction = program[ic]
        except IndexError:
            break
        interpreted = instructions[instruction.name]
        regs = exec_instruction(interpreted, *instruction.params, regs)
        if regs[1] == 6 and regs[4] == 1 and regs[3] == 0:
            regs[4] = regs[5]
        regs[pointer_reg] += 1

    print(regs)
