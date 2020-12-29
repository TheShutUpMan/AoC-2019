turnRight = {"E": "S", "S": "W", "W":"N", "N": "E"}
turnLeft = {"S": "E", "W": "S", "N":"W", "E": "N"}

def leftN(d, n):
    for _ in range(n):
        d = turnLeft[d]
    return d

def rightN(d, n):
    for _ in range(n):
        d = turnRight[d]
    return d

instructions1 = {
    "F": lambda x, y, d, n: instructions1[d],
    "N": lambda x, y, d, n: (x,y+n,d),
    "S": lambda x, y, d, n: (x,y-n,d),
    "E": lambda x, y, d, n: (x+n,y,d),
    "W": lambda x, y, d, n: (x-n,y,d),
    "L": lambda x, y, d, n: (x,y,leftN(d, n//90)),
    "R": lambda x, y, d, n: (x,y+n, rightN(d, n//90)),
}

def part1():
    with open('12.txt') as f:
        data = f.read().splitlines()
        x = 0
        y = 0
        facing = "E"
        for i in data:
            direction = i[0]
            num = int(i[1:])
            if direction == "F":
                direction = facing
            if direction == "N":
                y += num
            elif direction == "S":
                y -= num
            elif direction == "E":
                x += num
            elif direction == "W":
                x -= num
            elif direction == "L":
                facing = leftN(facing, num//90)
            elif direction == "R":
                facing = rightN(facing, num//90)
        return abs(x) + abs(y)


def part2():
    with open('12.txt') as f:
        data = f.read().splitlines()
        x = 0
        y = 0
        wpx = 10
        wpy = 1
        for i in data:
            direction = i[0]
            num = int(i[1:])
            if direction == "F":
                x += wpx * num
                y += wpy * num
            elif direction == "N":
                wpy += num
            elif direction == "S":
                wpy -= num
            elif direction == "E":
                wpx += num
            elif direction == "W":
                wpx -= num
            elif direction == "L":
                for _ in range(num//90):
                    (wpx, wpy) = (-wpy, wpx)
            elif direction == "R":
                for _ in range(num//90):
                    (wpx, wpy) = (wpy, -wpx)
            print(x,y,wpx,wpy)
        return abs(x) + abs(y)

if __name__ == "__main__":
    print(part1())

