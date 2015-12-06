# Ancient Nordic Elvish translation

import numpy
import re

instructions = [line.rstrip("\n") for line in open("day6.txt", "r")]
theSize      = 1000
lights       = numpy.zeros((theSize, theSize))

def get_cells(start, end):
    [startx, starty] = re.sub(r'\s', '', start).split(',')
    [endx,   endy]   = re.sub(r'\s', '', end).split(',')
    return [(x, y) for x in range(int(startx), int(endx)+1)
                   for y in range(int(starty), int(endy)+1)]

for instruction in instructions:
    inst    = instruction.split()
    command = inst[0]
    if command == "turn":
        onoff   = inst[1]
        cells = get_cells(inst[2], inst[4])
        if onoff == "on":
            for (x, y) in cells:
                lights[x, y] += 1
        elif onoff == "off":
            for (x, y) in cells:
                lights[x, y] -= 1
                if lights[x, y] < 0:
                    lights[x, y] = 0

    elif command == "toggle":
        cells = get_cells(inst[1], inst[3])
        for (x, y) in cells:
            lights[x, y] += 2.0

print "Nonzero:", lights.sum()
