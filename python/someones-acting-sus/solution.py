import sys
import math

def dissection(path, rooms):
    pairs = []
    for i in range(len(path)):
        if path[i] in rooms:
            break
    previous = i
    for next in range(1, len(path)):
        if path[next] in rooms:
            pairs.append((previous, next))
            previous = next
    return pairs

def verification(pairs, path, rooms):
    for pair in pairs:
        length = pair[1] - pair[0]
        initial_room = rooms.find(path[pair[0]])
        final_room = rooms.find(path[pair[1]])
        forwards = []
        for i in range(length+1):
            forwards.append(
                rooms[(initial_room + i) % len(rooms)]
            )
        backwards = []
        for i in range(length+1):
            backwards.append(
                rooms[(initial_room - i) % len(rooms)]
            )
        forwards.extend(backwards)
        if rooms[final_room] in forwards:
            continue
        else:
            print("SUS")
            return
    print("NOT SUS")
    return

l = int(input())
rooms = input()
n, _ = [int(i) for i in input().split()]
for i in range(n):
    path = input()
    pairs = dissection(path, rooms)
    verification(pairs, path, rooms)
