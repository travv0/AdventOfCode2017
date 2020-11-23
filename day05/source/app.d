import std.stdio : writeln;
import std.string : strip;
import std.array : split, array;
import std.conv : to;
import std.algorithm : map;
import std.file : readText;

@safe:

void main() {
	immutable instructions = readText("input.txt").parse;
	writeln("Part 1: ", instructions.countJumpsToEscape!1);
	writeln("Part 2: ", instructions.countJumpsToEscape!2);
}

pure:

private int[] parse(string input) {
	return input.strip.split('\n').map!(line => line.strip.to!int).array;
}

nothrow:

private uint countJumpsToEscape(uint part)(const int[] instructions)
		if (part == 1 || part == 2) {
	auto ins = instructions.dup;
	auto i = 0;
	auto jumps = 0;
	while (i < ins.length) {
		immutable offset = ins[i];
		if (part == 1 || offset < 3)
			ins[i]++;
		else
			ins[i]--;
		i += offset, jumps++;
	}
	return jumps;
}

unittest {
	immutable input = "0
3
0
1
-3";
	immutable instructions = input.parse;
	assert(instructions.dup.countJumpsToEscape!1 == 5);
	assert(instructions.dup.countJumpsToEscape!2 == 10);
}
