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

def coordinatesToLimits(coordinates):
    limits = []
    for initialIndex, initialCoordinate in enumerate(coordinates):
        for finalCoordinate in coordinates[initialIndex+1:]:
            limits.append(
                [initialCoordinate, finalCoordinate]
            )
    return limits

limitsX = coordinatesToLimits(X)
limitsY = coordinatesToLimits(Y)

class Rectangle:
    def __init__(self, limitX, limitY):
        self.x = limitX
        self.y = limitY
        self.width = self.x[1] - self.x[0]
        self.height = self.y[1] - self.y[0]
        self.isSquare = (self.width == self.height)
        return

rectangles = [
    Rectangle(limitX, limitY)
    for limitX in limitsX
    for limitY in limitsY
]

rectangleIsSquare = [
    rectangle.isSquare
    for rectangle in rectangles
]

# To debug: print("Debug messages...", file=sys.stderr, flush=True)

print(rectangleIsSquare.count(True))

