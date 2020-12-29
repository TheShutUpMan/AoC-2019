from functools import reduce, lru_cache, partial
from itertools import chain, combinations, cycle, product, starmap
from collections import defaultdict, deque, namedtuple, Counter

def parts():
    with open("21.txt") as f:
        data = f.read().splitlines()
        allergen_dict = defaultdict(set)
        ingredient_counts = defaultdict(int)
        for i in data:
            paren_ix = i.index('(')
            allergens = i[paren_ix + 1:-1].split(', ')
            ingredients = i[:paren_ix-1].split(' ')
            for a in allergens:
                allergen_dict[a].add(frozenset(ingredients))
            for i in ingredients:
                ingredient_counts[i] += 1
        allergen_ingredients = set()
        while len(allergen_dict) > 0:
            found_allergens = set()
            for k,v in allergen_dict.items():
                possible_choices = reduce(lambda x, y: x.intersection(y), v)
                if len(possible_choices) == 0:
                    raise Exception("Invalid dict")
                elif len(possible_choices) == 1:
                    allergen_ingredient, *_ = possible_choices
                    found_allergens.add(k)
                    allergen_ingredients.add((k, allergen_ingredient))
                    for key, val in allergen_dict.items():
                        allergen_dict[key] = set(map(lambda x:x.difference(possible_choices), val))
            for i in found_allergens:
                del allergen_dict[i]

        # Part 1
        good_ingredients = set(ingredient_counts.keys()).difference(
                           set(map(lambda x: x[1], allergen_ingredients)))
        good_count = 0
        for i in good_ingredients:
            good_count += ingredient_counts[i]
        print(f"Good ingredients appear {good_count} times")

        # Part 2
        ordered_ingredients = sorted(list(allergen_ingredients))
        print(','.join(map(lambda x: x[1], ordered_ingredients)))

if __name__ == "__main__":
    parts()
