from itertools import combinations

def is_sum(n, prev):
    for i,j in combinations(prev, 2):
        if i+j == n:
            return True
    return False


def part1(nums):
    ix = 25
    while is_sum(nums[ix], nums[ix-25:ix]):
        ix += 1
    print(ix)
    return nums[ix]

def part2(nums):
    invalid = part1(nums)
    print(invalid)
    for i in range(len(nums)-1):
        for j in range(i+1, len(nums)):
            if sum(nums[i:j+1]) == invalid:
                return (min(nums[i:j+1]) + max(nums[i:j+1]))

if __name__ == "__main__":
    with open("9.txt") as f:
        nums = list(map(int, f.read().strip().split('\n')))
        print(part2(nums))
