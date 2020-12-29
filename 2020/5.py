def seat_id(row, col):
    return row * 8 + col

def decode_pass(pass_):
    bot = 0
    top = 127
    left = 0
    right = 7
    for partition in pass_:
        if partition == 'F':
            top = (top + bot) // 2
        if partition == 'B':
            q,r = divmod(top + bot, 2)
            bot = q + r
        if partition == 'L':
            right = (right + left) // 2
        if partition == 'R':
            q,r = divmod(left + right, 2)
            left = q + r
    if bot != top or left != right:
        raise Exception(f"top: {top}, bot: {bot}, left:{left}, right:{right}")
    return (bot,left)


def part1():
    with open("5.txt") as f:
        passes = f.read().strip().split('\n')
        high_id = 0
        for pass_ in passes:
           high_id = max(seat_id(*decode_pass(pass_)), high_id)
        return(high_id)

def index_key (x):
    return x[0]*128 + x[1]

def part2():
    with open("5.txt") as f:
        passes = f.read().strip().split('\n')
        indices = set(map(decode_pass, passes))
        all_seats = set((i,j) for i in range(128) for j in range(8))
        return all_seats - indices


if __name__ == "__main__":
    print(part2())
