def day14p1():
    elf1 = 0
    elf2 = 6
    recipes = [3, 7, 1, 0, 1, 0, 1, 2]
    while len(recipes) < 540561 + 10:
        new_recipes = recipes[elf1] + recipes[elf2]
        recipes.extend([int(i) for i in str(new_recipes)])
        elf1 = (elf1 + recipes[elf1] + 1) % len(recipes)
        elf2 = (elf2 + recipes[elf2] + 1) % len(recipes)
    return recipes


def day14p2():
    elf1 = 0
    elf2 = 6
    recipes = [3, 7, 1, 0, 1, 0, 1, 2]
    test_index = 0
    target_sequence = [int(i) for i in str(540561)]
    print(target_sequence)
    while True:
        new_recipes = recipes[elf1] + recipes[elf2]
        recipes.extend([int(i) for i in str(new_recipes)])
        elf1 = (elf1 + recipes[elf1] + 1) % len(recipes)
        elf2 = (elf2 + recipes[elf2] + 1) % len(recipes)
        test_index += 1
        if recipes[test_index:test_index +
                   len(target_sequence, )] == target_sequence:
            return test_index
        if new_recipes >= 10:
            test_index += 1
            if recipes[test_index:test_index +
                       len(target_sequence, )] == target_sequence:
                return test_index


a = day14p2()
