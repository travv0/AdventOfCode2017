import std.stdio : writeln;
import std.array : split, array;
import std.conv : to;
import std.algorithm : map, maxElement;
import std.string : strip;

void main() {
	Cpu cpu;
	cpu.runInstructions!"input.txt";
	writeln("Part 1: ", cpu.values.maxElement);
	writeln("Part 2: ", cpu.highestValue);
}

@safe:
pure:

private enum Dir {
	inc,
	dec,
}

private struct Cond {
	string register;
	string comp;
	int value;
}

private struct Instruction {
	string register;
	Dir dir;
	int amount;
	Cond cond;
}

private Instruction[] parse(string input)() {
	immutable lines = input.split('\n');
	Instruction[] r;
	static foreach (line; lines) {
		r ~= parseLine!line;
	}
	return r;
}

unittest {
	immutable input = "a inc 1 if b < 5\nc dec -10 if a >= 1";
	assert(parse!input == [
			Instruction("a", Dir.inc, 1, Cond("b", "<", 5)),
			Instruction("c", Dir.dec, -10, Cond("a", ">=", 1))
			]);
}

private Instruction parseLine(string line)() {
	immutable parts = line.split(' ');
	Cond cond = {register: parts[4], comp: parts[5], value: to!int(parts[6])};
	return Instruction(parts[0], to!Dir(parts[1]), to!int(parts[2]), cond);
}

unittest {
	assert(parseLine!"b inc 5 if a > 1" == Instruction("b", Dir.inc, 5, Cond("a", ">", 1)));
	assert(parseLine!"c dec -20 if c == 10" == Instruction("c", Dir.dec,
			-20, Cond("c", "==", 10)));
}

private struct Cpu {
	int[string] registers;
	alias registers this;

	int highestValue = int.min;

	void runInstructions(string filename)() {
		immutable input = import(filename);
		immutable instructions = parse!(input.strip);
		static foreach (instruction; instructions) {
			mixin(
					`if ("` ~ instruction.register ~ `" !in registers) registers["`
					~ instruction.register ~ `"] = 0;`);
			mixin(
					`if ("` ~ instruction.cond.register ~ `" !in registers) registers["`
					~ instruction.cond.register ~ `"] = 0;`);
			mixin(`if (registers["` ~ instruction.cond.register ~ `"] ` ~ instruction.cond.comp ~ ` `
					~ instruction.cond.value.stringof ~ `) registers["` ~ instruction.register ~ `"] ` ~ (
						instruction.dir == Dir.dec ? "-" : "+") ~ `= `
					~ instruction.amount.stringof ~ `;`);
			mixin(`if (registers["` ~ instruction.register
					~ `"] > highestValue) highestValue = registers["` ~ instruction.register ~ `"];`);
		}
	}
}
