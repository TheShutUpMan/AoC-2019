import datetime
import heapq
import re
from collections import defaultdict

from timer import timer


@timer
def day4p1():
    with open('inputs/input4.txt') as file:
        text = file.read().strip().split('\n')
        events = list()
        for i in text:
            subbed = re.sub(r'[^\w]+', ' ', i).strip().split()
            t = datetime.datetime(*(list(map(int, subbed[:5]))))
            heapq.heappush(events, (t, ' '.join(subbed[5:])))
        guards = defaultdict(list)
        guard = str()
        guard_sleep = defaultdict(int)
        while events:
            current = heapq.heappop(events)
            if current[1][0] == 'G':
                guard = re.search(r'\d+', str(current[1])).group()
                guards[guard].append((current[0], 'awake'))
            elif current[1][0] == 'w':
                guard_sleep[guard] += (
                    current[0] - guards[guard][-1][0]).total_seconds() // 60
                print(guard_sleep[guard])
                guards[guard].append((current[0], 'awake'))
            else:
                guards[guard].append((current[0], 'asleep'))
        sleepy_guard = max(
            (guard_sleep[guard], guard) for guard in guard_sleep.keys())
        sleep_minutes = defaultdict(int)
        asleep = 0
        for i in guards[sleepy_guard[1]]:
            if i[1] == 'asleep':
                asleep = i[0].time().minute
            else:
                if asleep > 0:
                    counter = asleep
                    awake = i[0].time().minute
                    while counter != awake:
                        sleep_minutes[counter] += 1
                        counter = (counter + 1) % 60
                asleep = -1
        print(
            int(
                max((sleep_minutes[mins], mins)
                    for mins in sleep_minutes.keys())[1]) * int(
                        sleepy_guard[1]))


@timer
def day4p2():
    with open('inputs/input4.txt') as file:
        text = file.read().strip().split('\n')
        events = list()
        for i in text:
            subbed = re.sub(r'[^\w]+', ' ', i).strip().split()
            t = datetime.datetime(*(list(map(int, subbed[:5]))))
            heapq.heappush(events, (t, ' '.join(subbed[5:])))
        guards = defaultdict(list)
        guard = str()
        guard_sleep = defaultdict(int)
        while events:
            current = heapq.heappop(events)
            if current[1][0] == 'G':
                guard = re.search(r'\d+', str(current[1])).group()
                guards[guard].append((current[0], 'awake'))
            elif current[1][0] == 'w':
                guard_sleep[guard] += (
                    current[0] - guards[guard][-1][0]).total_seconds() // 60
                print(guard_sleep[guard])
                guards[guard].append((current[0], 'awake'))
            else:
                guards[guard].append((current[0], 'asleep'))
        sleepy_guard = max(
            (guard_sleep[guard], guard) for guard in guard_sleep.keys())
        sleep_minutes = {}
        asleep = 0
        for ID, guard in guards.items():
            sleep_minutes[ID] = defaultdict(int)
            for i in guard:
                if i[1] == 'asleep':
                    asleep = i[0].time().minute
                else:
                    if asleep > 0:
                        counter = asleep
                        awake = i[0].time().minute
                        while counter != awake:
                            sleep_minutes[ID][counter] += 1
                            counter = (counter + 1) % 60
                    asleep = -1
        minutes = list()
        for ID, dic in sleep_minutes.items():
            print(ID, dic)
            if dic:
                maxint = max((dic[i], i) for i in dic.keys())
                minutes.append((*maxint, ID))
        maxmin = max(minutes)
        result = int(maxmin[1]) * int(maxmin[2])
        print(result)
        return result


day4p1()
day4p2()
