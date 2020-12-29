import copy

def part1():
    with open("8.txt") as f:
        program = f.read().strip().split('\n')
        acc = 0
        pc = 0
        visited = set()
        while pc not in visited:
            visited.add(pc)
            instr = program[pc].split(' ')
            if instr[0] == "acc":
                acc += int(instr[1])
                pc += 1
            elif instr[0] == "jmp":
                pc += int(instr[1])
            else:
                pc += 1
        print(acc)

def part2():
    with open("8.txt") as f:
        program = [x.split(' ') for x in f.read().strip().split('\n')]
        acc = 0
        for n, (instr, val) in enumerate(program):
            if instr == "acc":
                continue
            newProgram = copy.deepcopy(program)
            if instr == "nop":
                newProgram[n] = ("jmp", val)
            elif instr == "jmp":
                newProgram[n] = ("nop", val)
            pc = 0
            acc = 0
            visited = set()
            terminated = False
            while pc not in visited:
                if pc >= len(newProgram):
                    terminated = True
                    break
                visited.add(pc)
                instr, val = newProgram[pc]
                if instr == "acc":
                    acc += int(val)
                    pc += 1
                elif instr == "jmp":
                    pc += int(val)
                else:
                    pc += 1
            if terminated:
                break
        print(acc)


if __name__ == "__main__":
    part2()
