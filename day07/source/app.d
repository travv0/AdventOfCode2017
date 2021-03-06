import std.stdio : writeln;
import std.regex : ctRegex, matchFirst;
import std.array : split;
import std.string : strip, split;
import std.conv : to;
import std.algorithm : map, find, filter, any, canFind, sum, sort, uniq, countUntil;
import std.array : array;
import std.file : readText;
import std.range : walkLength, isRandomAccessRange;

@safe:

void main() {
	auto program = "input.txt".readText.parse;
	writeln("Part 1: ", program.name);
	writeln("Part 2: ", program.correctedBadTowerWeight);
}

private Program parse(string input) {
	struct P {
		string name;
		uint weight;
		string[] children;
	}

	P[] programs;
	immutable regex = ctRegex!r"(\w+) \((\d+)\)( -> (.+))?";
	immutable lines = input.strip.split('\n');
	foreach (line; lines) {
		auto captures = line.matchFirst(regex);
		programs ~= P(captures[1], to!uint(captures[2]), captures[4].split(", "));
	}

	P findRoot() {
		auto potentials = programs.filter!(p => p.children.length != 0);
		foreach (program; potentials) {
			if (!potentials.any!(p => p.children.canFind(program.name))) {
				return program;
			}
		}
		assert(0);
	}

	Program buildProgram(const P root) @safe {
		auto children = root.children.map!(c => buildProgram(programs.find!(p => p.name == c)[0]));
		return Program(root.name, root.weight, children.array);
	}

	return buildProgram(findRoot());
}

pure:
@nogc:
nothrow:

private struct Program {
	string name;
	uint weight;
	Program[] children;
}

private uint calculateWeight(const Program program) {
	return program.weight + program.children.map!calculateWeight.sum;
}

private uint correctedBadTowerWeight(const Program program) {
	uint correctedBadTowerWeight(const Program program, int offset) {
		auto weights = program.children.map!calculateWeight;
		if (weights.uniq.walkLength == 1)
			return program.weight - offset;
		auto badWeightPos = badWeightPosition(weights);
		auto badChild = program.children[badWeightPos],
			goodChild = program.children[(badWeightPos + 1) % weights.length];
		return correctedBadTowerWeight(badChild,
				calculateWeight(badChild) - calculateWeight(goodChild));
	}

	return correctedBadTowerWeight(program, 0);
}

private long badWeightPosition(Range)(Range weights) if (isRandomAccessRange!Range) {
	assert(weights.length >= 3);
	if (weights[0] == weights[1] || weights[0] == weights[2])
		return weights.countUntil!(w => w != weights[0]);
	return 0;
}
