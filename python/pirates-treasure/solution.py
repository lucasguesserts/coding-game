import sys
import math
import itertools


def getLengthX(matrix):
    return len(matrix[0])


def getLengthY(matrix):
    return len(matrix)


def getLastX(matrix):
    return getLengthX(matrix) - 1


def getLastY(matrix):
    return getLengthY(matrix) - 1


def getValue(position, matrix):
    x, y = position
    return matrix[y][x]


def getNeighbors(position, matrix):
    x, y = position
    xMax = getLengthX(matrix)
    yMax = getLengthY(matrix)
    xi = max(0, x - 1)
    xf = min(xMax, x + 1 + 1)
    yi = max(0, y - 1)
    yf = min(yMax, y + 1 + 1)
    neighbors = [(x, y) for x in range(xi, xf) for y in range(yi, yf)]
    neighbors.remove((x, y))
    return neighbors


class Result:
    def __init__(self, position=None):
        self.position = position
        if position is None:
            self.found = False
        else:
            self.found = True

    def __str__(self):
        return f"[found: {self.found}, position: {self.position}]"


def isTreasure(position, matrix):
    value = getValue(position, matrix)
    neighbors = getNeighbors(position, matrix)
    if value == 1:
        return False
    else:
        return all(map(lambda x: getValue(x, matrix) == 1, neighbors))


def findTreasure(matrix):
    xMax = getLengthX(matrix)
    yMax = getLengthY(matrix)
    for position in itertools.product(range(xMax), range(yMax)):
        found = isTreasure(position, matrix)
        if found:
            return Result(position)
    return Result()


def getMatrix():
    matrix = []
    w = int(input())
    h = int(input())
    for i in range(h):
        matrix.append([])
        row = matrix[-1]
        for j in input().split():
            row.append(int(j))
    return matrix


if __name__ == "__main__":
    matrix = getMatrix()
    result = findTreasure(matrix)
    x, y = result.position
    print(f"{x} {y}")
