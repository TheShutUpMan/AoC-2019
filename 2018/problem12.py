with open('inputs/input12.txt') as input:
    state = "." * 40 + input.readline().strip() + "." * 400
    transitions = input.read().strip().split('\n')
    transitions = {i:j for i,j in (k.split(' => ') for k in transitions)}
    print(len(state))
    for i in range(201):
        print(state)
        empty_state = list("." * len(state))
        for i, _ in enumerate(empty_state):
            if i == 0:
                empty_state[i] = transitions[".." + state[:3]]
            elif i == 1:
                empty_state[i] = transitions["." + state[:4]]
            elif i == len(state) - 2:
                empty_state[i] = transitions[state[i-2:] + "."]
            elif i == len(state) - 1:
                empty_state[i] = transitions[state[i-2:] + ".."]
            else:
                empty_state[i] = transitions[state[i-2:i+3]]
        state = ''.join(empty_state)
    print(state)
    counter = 0
    for n, plant in enumerate(state):
        if plant == '#':
            counter += n - 40
    print(counter)
