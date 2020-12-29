class Fighter:
    def __init__(self, name, position, scene):
        self.name = name
        self.position = position
        self.scene = scene

    def turn(self):
        pass

    def move(self):
        pass

    def attack(self):
        pass

with open("inputs/input15.txt") as input:
    scene = input.read().strip().split('\n')
    fighters = list()
    for i, line in enumerate(scene):
        for j, tile in enumerate(line):
            if tile == 'G' or tile == 'E':
                fighters.append(Fighter(tile, (i,j), scene))
    while True:
        pass 
