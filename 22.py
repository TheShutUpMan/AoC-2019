from functools import reduce, lru_cache, partial
from itertools import chain, combinations, cycle, product, starmap
from collections import defaultdict, deque, namedtuple, Counter

def part1():
    with open("22.txt") as f:
        player1, player2 = [list(map(int, i.splitlines())) for i in f.read().strip().split('\n\n')]
        round_count = 1
        while len(player1) != 0 and len(player2) != 0:
            player1, player2 = round(player1,player2)
            round_count += 1
        return score(player1) + score(player2)

def part2():
    with open("22.txt") as f:
        player1, player2 = [list(map(int, i.splitlines())) for i in f.read().strip().split('\n\n')]
        player1, player2 = rec_game(player1,player2,1,False)
        return score(player1) + score(player2)

def round(p1, p2):
    c1 = p1.pop(0)
    c2 = p2.pop(0)
    if c1 > c2:
        p1.append(c1)
        p1.append(c2)
    else:
        p2.append(c2)
        p2.append(c1)
    return (p1,p2)

def rec_game(p1, p2, game = 1, print_rounds = True):
    seen = []
    round_ = 1
    while len(p1) != 0 and len(p2) != 0:
        if print_rounds:
            print(f'-- Round {round_} (Game {game}) --')
            print(f"Player 1's deck: {', '.join(map(str,p1))}")
            print(f"Player 2's deck: {', '.join(map(str,p2))}")
        if (p1,p2) in seen:
            if print_rounds:
                print("Combination seen before, player 1 wins by default")
            return ([1],[])
        seen.append((p1.copy(),p2.copy()))
        c1 = p1.pop(0)
        c2 = p2.pop(0)
        if print_rounds:
            print(f"Player 1 plays: {c1}")
            print(f"Player 2 plays: {c2}")
        winner = 0
        if len(p1) >= c1 and len(p2) >= c2:
            if print_rounds:
                print("Playing a sub-game to determine the winnner...\n")
            if len(rec_game(p1[:c1].copy(), p2[:c2].copy(), game + 1, print_rounds)[0]) == 0:
                winner = 2
                if print_rounds:
                    print("\nPlayer 2 wins the sub-game!")
            else:
                if print_rounds:
                    print("\nPlayer 1 wins the sub-game!")
                winner = 1
        elif c1 > c2:
            if print_rounds:
                print("Player 1 wins!")
            winner = 1
        else:
            if print_rounds:
                print("Player 2 wins!")
            winner = 2
        if winner == 1:
            p1.append(c1)
            p1.append(c2)
        else:
            p2.append(c2)
            p2.append(c1)
        round_ += 1
    return (p1,p2)

def score(p):
    counter = len(p)
    score_total = 0
    for i in p:
        print(f"+ {i} * {counter}")
        score_total += counter * int(i)
        counter -= 1
    return score_total

if __name__ == "__main__":
    print(part1())
    print(part2())
