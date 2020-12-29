from collections import namedtuple

with open('inputs/input16.txt') as file:
    split_input = file.read().strip().split('\n\n')
    linebreak_count = 0
    Transition = namedtuple('Transition', 'before instruction after')
    transitions = list()
    program = ""
    prog_flag = False
    for line in split_input:
        if prog_flag:
            program = line
            break
        if len(line) == 0:
            prog_flag = True
        else:
            before, instruction, after = line.split('\n')
            before = before.replace('Before: ', '')
            after = after.replace('After: ', '')
            transitions.append(Transition(before = eval(before),
                                           instruction = [int(i) for i in instruction.split(' ')],
                                           after = eval(after)))

    def exec_instruction(instr, a, b, c, x):
        xn = x[:]
        xn[c] = instr(a,b,x)
        return xn

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

    equality_dict = {i:list() for i in range(16)} 
    for elem in transitions:
        curr = equality_dict[elem.instruction[0]]
        curr.append(set())
        for name,instr in instructions.items():
            if exec_instruction(instr, *elem.instruction[1:], elem.before) == elem.after:
                curr[-1].add(name)

    for key, value in equality_dict.items():
        newval = set(instructions.keys())
        for set_ in value:
            newval = newval.intersection(set_)
        equality_dict[key] = newval

    while True:
        sorted_eq = sorted(equality_dict.items(), key = lambda x:len(x[1]))
        one_elem = set()
        for i in sorted_eq:
            if len(i[1]) == 1:
                one_elem = one_elem.union(i[1])
            else:
                equality_dict[i[0]] = i[1].difference(one_elem)
        if len(one_elem) == 16:
            for key, value in equality_dict.items():
                equality_dict[key] = value.pop()
            break

        
    program = program.strip().split('\n')
    program = [[int(j) for j in i.split(' ')] for i in program]
    initial = [0,0,0,0]

    def map_instruction(state, instructs):
        if instructs == []:
            return state
        instr = instructs[0]
        executable = instructions[equality_dict[instr[0]]]
        state = exec_instruction(executable, *instr[1:], state)
        return map_instruction(state, instructs[1:])

    print(map_instruction(initial, program))
