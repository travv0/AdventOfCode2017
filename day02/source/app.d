import std.stdio;
import std.conv : to;
import std.file : readText;
import std.string : split, strip;
import std.algorithm : map, maxElement, minElement, fold;

@safe:

void main() {
	immutable input = readText("input.txt");
	writeln("Part 1: ", checksum(input));
	writeln("Part 2: ", sumDivisibleValues(input));
}

pure:

private uint checksum(string input) {
	auto sum = 0;
	immutable lines = input.strip.split('\n');
	foreach (line; lines) {
		auto cells = line.strip.split('\t').map!(to!uint);
		sum += cells.maxElement - cells.minElement;
	}
	return sum;
}

private uint sumDivisibleValues(string input) {
	auto sum = 0;
	immutable lines = input.strip.split('\n');
	foreach (line; lines) {
		auto cells = line.strip.split('\t').map!(to!uint);
		outer: foreach (a; cells) {
			foreach (b; cells) {
				if (a != b && a % b == 0) {
					sum += a / b;
					break outer;
				}
			}
		}
	}
	return sum;
}

unittest {
	immutable input = "5	1	9	5
7	5	3
2	4	6	8
";
	assert(checksum(input) == 18);
}

unittest {
	immutable input = "5	9	2	8
9	4	7	3
3	8	6	5
";
	assert(sumDivisibleValues(input) == 9);
}
