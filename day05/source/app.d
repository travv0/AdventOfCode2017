import std.stdio : writeln;
import std.string : strip;
import std.array : split, array;
import std.conv : to;
import std.algorithm : map;
import std.file : readText;

void main() @safe {
	immutable instructions = readText("input.txt").parse;
	writeln("Part 1: ", instructions.dup.countJumpsToEscape!1);
	writeln("Part 2: ", instructions.dup.countJumpsToEscape!2);
}

private uint countJumpsToEscape(uint part)(int[] instructions) @safe pure nothrow @nogc
		if (part == 1 || part == 2) {
	auto i = 0;
	auto jumps = 0;
	while (i < instructions.length) {
		immutable offset = instructions[i];
		if (part == 1 || offset < 3)
			instructions[i]++;
		else
			instructions[i]--;
		i += offset, jumps++;
	}
	return jumps;
}

private int[] parse(string input) @safe pure {
	return input.strip.split('\n').map!(to!int).array;
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
