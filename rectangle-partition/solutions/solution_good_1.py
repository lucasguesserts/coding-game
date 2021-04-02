import sys
import math

# Auto-generated code below aims at helping you parse
# the standard input according to the problem statement.

w, h, count_x, count_y = [int(i) for i in input().split()]
X = [0]
Y = [0]
for i in input().split():
    x = int(i)
    X.append(x)
for i in input().split():
    y = int(i)
    Y.append(y)
X.append(w)
Y.append(h)

def coordinatesToMapping(coordinates):
    minValue = min(coordinates)
    maxValue = max(coordinates)
    mapping = dict([[i, 0] for i in range(minValue, maxValue+1, 1)])
    for initialIndex, initialCoordinate in enumerate(coordinates):
        for finalCoordinate in coordinates[initialIndex+1:]:
            measure = finalCoordinate - initialCoordinate
            mapping[measure] += 1
    return mapping

mappingX = coordinatesToMapping(X)
mappingY = coordinatesToMapping(Y)

def countSquares(mappingX, mappingY):
    squares = 0
    maxMeasure = min(
        max(mappingX.keys()),
        max(mappingY.keys())
    )
    for measure in range(maxMeasure+1):
        squares += mappingX[measure] * mappingY[measure]
    return squares


# To debug: print("Debug messages...", file=sys.stderr, flush=True)

print(countSquares(mappingX, mappingY))

