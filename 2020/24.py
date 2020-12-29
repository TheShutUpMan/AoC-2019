def parse_tiles(line):
    tiles = []
    buffer = ""
    for t in line:
        if t == "e" or t == "w":
            tiles.append(buffer + t)
            buffer = ""
        elif t == "n" or t == "s":
            buffer = t
    return tiles

# axial coordinates
dir_coords = {
    "e" : (1,0),
    "se": (0,1),
    "sw": (-1,1),
    "w" : (-1,0),
    "nw": (0,-1),
    "ne": (1,-1),
}

test = """sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"""

def decode_directions(directions):
    x,z = 0,0
    for d in directions:
        xp, zp = dir_coords[d]
        x,z = x+xp, z+zp
    return (x,z)

def get_neighbors(tile):
    q,r = tile
    neighbors = []
    for qp,rp in dir_coords.values():
        neighbors.append((q+qp, r+rp))
    return neighbors

def part1():
    moveList = map(parse_tiles, open("24.txt").read().splitlines())
    black_set = set()
    for moves in moveList:
        decoded = decode_directions(moves)
        if decoded in black_set:
            black_set.remove(decoded)
        else:
            black_set.add(decoded)
    return black_set

def part2():
    black_set = part1()
    for _ in range(100):
        new_set = set()
        white_visited = set()
        for tile in black_set:
            neighbors = get_neighbors(tile)
            count = 0
            for n in neighbors:
                if n in black_set:
                    count += 1
                elif n not in white_visited:
                    white_neighbors = get_neighbors(n)
                    w_count = 0
                    for wn in white_neighbors:
                        if wn in black_set:
                            w_count += 1
                    if w_count == 2:
                        new_set.add(n)
                    white_visited.add(n)
            if 1 <= count <= 2:
                new_set.add(tile)
        black_set = new_set
    return black_set



if __name__ == "__main__":
    #print(len(part1()))
    print(len(part2()))
