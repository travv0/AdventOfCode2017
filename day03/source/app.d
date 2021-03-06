import std.stdio;
import std.math : abs;
import std.algorithm : sum, uniq, map, bringToFront, canFind, sort;
import std.range : empty;
import std.array : array;
import std.traits : EnumMembers;

@safe:

void main() {
	immutable input = 361527;
	writeln("Part 1: ", getDistance(input));
	writeln("Part 2: ", findGreaterThanInput(input));
}

pure:
nothrow:

private uint findGreaterThanInput(uint square) {
	uint[Coords] memory;

	uint sumAdjacent(Coords coords) {
		immutable coordsToCheck = [
			Coords(-1, -1), Coords(-1, 0), Coords(-1, 1), Coords(0, -1),
			Coords(0, 0), Coords(0, 1), Coords(1, -1), Coords(1, 0), Coords(1, 1)
		];
		return coordsToCheck.map!(a => coords + a in memory ? memory[coords + a] : 0).sum;
	}

	memory[Coords(0, 0)] = 1;
	auto direction = Directions.init;
	auto turnDistance = 1, distance = 0, coords = Coords(1, 0);
	uint sum;
	do {
		sum = sumAdjacent(coords);
		memory[coords] = sum;
		distance++;

		final switch (direction) {
		case Directions.up:
			coords.y--;
			break;
		case Directions.left:
			coords.x--;
			break;
		case Directions.down:
			coords.y++;
			break;
		case Directions.right:
			coords.x++;
		}

		if (distance == turnDistance) {
			if ([Directions.up, Directions.down].canFind(direction))
				turnDistance++;
			direction = direction.next;
			distance = 0;
		}
	}
	while (sum < square);
	return sum;
}

@nogc:

uint getDistance(uint square) {
	if (square <= 1)
		return 0;

	auto step = 2, distanceFromCenter = 1, countAround = 0;
	for (auto i = 2;; i += step) {
		if (step % 2 != 0)
			step++;

		if (i + distanceFromCenter >= square)
			return square - i + distanceFromCenter;

		if (countAround == 3)
			countAround = 0, step++, distanceFromCenter++;
		else
			countAround++;
	}
}

unittest {
	assert(getDistance(1) == 0);
	assert(getDistance(12) == 3);
	assert(getDistance(23) == 2);
	assert(getDistance(1024) == 31);
}

private struct Coords {
	int x;
	int y;

	Coords opBinary(string op)(Coords rhs) {
		return mixin("Coords(x " ~ op ~ " rhs.x, y " ~ op ~ " rhs.y)");
	}
}

private enum Directions {
	up,
	left,
	down,
	right
}

private T next(T)(T e) if (is(T == enum)) {
	return cast(T)((e + 1) % EnumMembers!T.length);
}
