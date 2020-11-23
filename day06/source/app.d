import std.stdio : writeln;
import std.array : split;
import std.algorithm : map, maxIndex, countUntil;
import std.conv : to;
import std.string : strip;
import std.array : array;
import std.file : readText;

@safe:

void main() {
	immutable banks = readText("input.txt").parse;
	uint count;
	long cycles;
	banks.countCyclesUntilLoop(count, cycles);
	writeln("Part 1: ", count);
	writeln("Part 2: ", cycles);
}

pure:

private uint[] parse(string input) {
	return input.strip.split('\t').map!(to!uint).array;
}

nothrow:

private uint[] redistribute(const uint[] banks) {
	auto bs = banks.dup;
	immutable n = bs.maxIndex;
	auto blocks = bs[n];
	bs[n] = 0;
	long i = n;
	while (blocks > 0) {
		i = (i + 1) % bs.length;
		bs[i]++, blocks--;
	}
	return bs;
}

unittest {
	assert(redistribute([0, 2, 7, 0]) == [2, 4, 1, 2]);
	assert(redistribute([2, 4, 1, 2]) == [3, 1, 2, 3]);
	assert(redistribute([3, 1, 2, 3]) == [0, 2, 3, 4]);
}

private void countCyclesUntilLoop(const uint[] banks, out uint count, out long cycles) {
	auto bs = banks.dup;
	uint[][] seen;
	count = 0;
	while ((cycles = seen.countUntil(bs) + 1) == 0) {
		seen = bs ~ seen;
		bs = bs.redistribute;
		count++;
	}
}

unittest {
	uint count;
	long cycles;
	countCyclesUntilLoop([0, 2, 7, 0], count, cycles);
	assert(count == 5);
	assert(cycles == 4);
}
