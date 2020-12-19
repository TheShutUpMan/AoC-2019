from functools import reduce, lru_cache, partial
from itertools import chain, combinations, cycle, product, starmap
from collections import defaultdict, deque, namedtuple, Counter

import re, sys

def part1():
    with open("19_2.txt") as f:
        #raw_rules = f.read().strip()
        raw_rules = test_2_rules.strip()
        raw_rules = raw_rules.splitlines()
        rules = parse_rules(raw_rules)
        print(''.join(render_dict(rules, rename_raw_rules(rules))))
#        rule_0 = re.compile(to_re(full_rule(rules, 0)))
#        match_count = 0
#    for string in strings.split('\n'):
#            if rule_0.fullmatch(string) is not None:
#                match_count += 1
#    return match_count

# Tuple for choice, list for sequence
def parse_rules(raw_rules):
    for n,i in enumerate(raw_rules):
        tag, match = i.split(': ')
        if match.startswith('"'):
            raw_rules[n] = (int(tag),match.replace('"',''))
        else:
            newMatch = []
            for i in match.split(' | '):
                curr = []
                for j in i.split(' '):
                    curr.append(int(j))
                newMatch.append(curr)
            if len(newMatch) > 1:
                newMatch = tuple(newMatch)
            else:
                newMatch = newMatch[0]
            raw_rules[n] = (int(tag), newMatch)
    return dict(raw_rules)

def full_rule(rules, rule):
    full = []
    if isinstance(rule, str):
        return rule
    elif isinstance(rule, int):
        return full_rule(rules, rules[rule])
    elif isinstance(rule, list):
        for r in rule:
            full.append(full_rule(rules, r))
    else: # Tuple - choice
        for r in rule:
            full.append(full_rule(rules, r))
        full = tuple(full)
    return full

def to_re(rule):
    if isinstance(rule, str):
        return rule
    elif isinstance(rule, tuple):
        return("(" + '|'.join(map(to_re, rule)) + ")")

    else:
        return("(" + ''.join(map(to_re, rule)) + ")")

def rename_raw_rules(rule_dict):
    keys = rule_dict.keys()
    rename_dict = dict()
    for k in keys:
        new_name = ""
        for c in str(k):
            new_name += chr(int(c) + 65)
        rename_dict[k] = new_name
    print(98 in rename_dict.keys())
    return rename_dict

def render_dict(rule_dict, rename_dict):
    output = []
    for k, i in sorted(rule_dict.items()):
        output += f"{rename_dict[k]}: {render_rule(i, rename_dict)}\n"
    return output

def render_rule(rule, rename_dict):
    if isinstance(rule, tuple):
        return ' | '.join(render_rule(i, rename_dict) for i in rule)
    if isinstance(rule, list):
        return ' '.join(render_rule(i, rename_dict) for i in rule)
    if isinstance(rule, int):
        return rename_dict[rule]
    else:
        return f'"{rule}"'

def build(n, d, rules):
        if d > 6:
            if n == 8:
                return ""
            if n == 11:
                return ""
        r = rules[n]
        if isinstance(r, str):
            return r

        p = []
        for rs in r:
            cs = ""
            for i in rs:
                cs += build(i, d + 1, rules)
            p.append(cs)

        return "({})".format("|".join(p))


def part2():
    src = open("19.txt", "r").readlines()
    src = [r.strip() for r in src if r.strip()]

    rules = {}
    messages = []

    for line in src:
        if line[0].isdigit():
            t = line.split(": ")
            n = int(t[0])

            if n == 8:
                t[1] = "42 | 42 8"
            if n == 11:
                t[1] = "42 31 | 42 11 31"

            r = t[1].replace('"', "")
            if r.isalpha():
                rules[n] = r
            else:
                rules[n] = []
                for rst in r.split(" | "):
                    rules[n].append([int(x) for x in rst.split(" ")])
        else:
            messages.append(line)

    rxs = "^" + build(0, 0, rules) + "$"
    rx = re.compile(rxs)

    c = 0
    for m in messages:
        if rx.match(m):
            c += 1
    print(c)

if __name__ == "__main__":
    part2()
