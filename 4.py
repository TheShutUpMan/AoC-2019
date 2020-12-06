import re

fields = set(["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"])

def hgt_handler(string):
    match = re.match(r"(\d+)(in|cm)",string)
    if not match:
        return False
    height, unit = match.groups()
    height = int(height)
    if unit == "cm":
        return height >= 150 and height <= 193
    elif unit == "in":
        return height >= 59 and height <= 76
    return False


rules = {
    "byr": lambda x: int(x) >= 1920 and int(x) <= 2002,
    "iyr": lambda x: int(x) >= 2010 and int(x) <= 2020,
    "eyr": lambda x: int(x) >= 2020 and int(x) <= 2030,
    "hgt": hgt_handler,
    "hcl": lambda x: True if re.match(r'#[1234567890abcdef]{6}',x) else False,
    "ecl": lambda x: True if re.match(r'amb|blu|brn|gry|grn|hzl|oth',x) else False,
    "pid": lambda x: True if re.match(r'\d{9}',x) else False,
    "cid": lambda _: True,
}

def parseField(string):
    pair = string.split(':')
    return(pair[0], pair[1])

def part1():
    with open('4.txt') as f:
        passports = f.read().strip().split('\n\n')
        valid = 0
        for i in passports:
            pairs = list(map(parseField, re.split(" |\n", i)))
            passport_fields = set([p for p,_ in pairs])
            if fields.issubset(passport_fields):
                valid += 1
        return valid

# Gives a wrong answer for one of them, I don't know which
def part2():
    with open('4.txt') as f:
        passports = f.read().strip().split('\n\n')
        valid = 0
        for i in passports:
            pairs = list(map(parseField, re.split(" |\n", i)))
            passport_fields = set([p for p,_ in pairs])
            if fields.issubset(passport_fields):
                validated = True
                for field, value in pairs:
                    if not rules[field](value):
                        validated = False
                        break
                if validated:
                    valid += 1
        return valid



if __name__ == "__main__":
    print(part2())
