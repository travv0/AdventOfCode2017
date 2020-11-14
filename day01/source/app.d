import std.stdio;
import std.conv : to;
import std.file : readText;
import std.string : strip;

void main() {
	const input = readText("input.txt").strip;
	writeln("Part 1: ", solveCaptcha!1(input));
	writeln("Part 2: ", solveCaptcha!2(input));
}

private uint solveCaptcha(uint part)(string captcha) @safe pure 
		if (part == 1 || part == 2) {
	auto sum = 0;
	static if (part == 1) {
		const offset = 1;
	} else {
		const offset = captcha.length / 2;
	}

	foreach (i, c; captcha) {
		if (c == captcha[(i + offset) % captcha.length]) {
			sum += to!uint(captcha[i .. i + 1]);
		}
	}
	return sum;
}

unittest {
	assert(solveCaptcha!1("1122") == 3);
	assert(solveCaptcha!1("1111") == 4);
	assert(solveCaptcha!1("1234") == 0);
	assert(solveCaptcha!1("91212129") == 9);
}

unittest {
	assert(solveCaptcha!2("1212") == 6);
	assert(solveCaptcha!2("1221") == 0);
	assert(solveCaptcha!2("123425") == 4);
	assert(solveCaptcha!2("123123") == 12);
	assert(solveCaptcha!2("12131415") == 4);
}
