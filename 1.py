with open("1.txt") as f:
    a = list(map(int, f.read().split()))
    for n,i in enumerate(a):
        for m,j in enumerate(a):
            for o,k in enumerate(a):
                if n!=m and m!=k and k != n and i+j+k == 2020:
                    print(i * j*k)

