class Cart:
    def __init__(self, position, init_dir, tracks):
        self.position = position
        self.direction = {'^': 'U', 'v': 'D', '>': 'R', '<': 'L'}[init_dir]

        if init_dir == '^' or init_dir == 'V':
            tracks[position[0]][position[1]] = '|'
        else:
            tracks[position[0]][position[1]] = '-'
        self.tracks = tracks
        self.turns = 0

    def __repr__(self):
        return str(self.position)

    def move(self):
        if self.direction == 'U':
            self.position[0] -= 1
        elif self.direction == 'D':
            self.position[0] += 1
        elif self.direction == 'R':
            self.position[1] += 1
        else:
            self.position[1] -= 1
        track_under = self.tracks[self.position[0]][self.position[1]]

        if track_under == '/':
            self.direction = {'R': 'U', 'U': 'R',
                              'L': 'D', 'D': 'L'}[self.direction]
        elif track_under == '\\':
            self.direction = {'R': 'D', 'D': 'R',
                              'L': 'U', 'U': 'L'}[self.direction]
        elif track_under == '+':
            self.turns += 1

            if self.turns % 3 == 1:
                self.direction = {'L': 'D', 'D': 'R',
                                  'R': 'U', 'U': 'L'}[self.direction]
            elif self.turns % 3 == 0:
                self.direction = {'L': 'U', 'U': 'R',
                                  'R': 'D', 'D': 'L'}[self.direction]


class Mineshaft:
    def __init__(self):
        with open("inputs/input13.txt") as inp:
            self.tracks = list(inp.read().split('\n'))

            for n, i in enumerate(self.tracks):
                self.tracks[n] = list(i)
        self.carts = list()

    def main(self):
        for i, line in enumerate(self.tracks):
            for j, char in enumerate(line):
                if char in "<>^v":
                    self.carts.append(Cart([i, j], char, self.tracks))

        while True:
            positions = [(i, i.position) for i in self.carts]
            update_order = sorted(
                self.carts, key=lambda x: x.position[0]*1000 + x.position[1])

            for n, i in enumerate(update_order):
                i.move()
                count = 0

                for j in positions:
                    if i.position == j[0].position:
                        count += 1

                if count > 1:
                    print(f"Collision at {i}. Current carts: {self.carts}")

                    for n, cart in enumerate(self.carts):
                        if cart.position == i.position:
                            del self.carts[n]

                            break

                    for n, cart in enumerate(self.carts):
                        if cart.position == i.position:
                            del self.carts[n]

                            break

            if len(self.carts) == 1:
                print(self.carts)

                break


a = Mineshaft()
a.main()
