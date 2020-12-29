def main1():
    with open('2.txt') as f:
        strings = f.read().strip().split('\n')
        valid_count = 0
        for string in strings:
            str_ = string.split(' ')
            lowhigh = str_[0].split('-')
            low = int(lowhigh[0])
            high = int(lowhigh[1])
            letter = str_[1][0]
            passwd = str_[2]
            counter = 0
            for i in passwd:
                if letter == i:
                    counter += 1
            if counter >= low and counter <= high:
                valid_count += 1
        print(valid_count)

def main2():
    with open('2.txt') as f:
        strings = f.read().strip().split('\n')
        valid_count = 0
        for string in strings:
            str_ = string.split(' ')
            lowhigh = str_[0].split('-')
            low = int(lowhigh[0])
            high = int(lowhigh[1])
            letter = str_[1][0]
            passwd = str_[2]
            low_valid = passwd[low-1] == letter
            high_valid = passwd[high-1] == letter
            if low_valid != high_valid:
                valid_count += 1
        print(valid_count)


if __name__ == "__main__":
    main2()
