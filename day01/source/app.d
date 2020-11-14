import std.stdio;
import std.conv : to;
import std.file : readText;
import std.string : strip;

void main() {
	const input = readText("input.txt").strip;
	writeln("Part 1: ", solveCaptcha(input));
}

private uint solveCaptcha(string captcha) @safe pure {
	auto sum = 0;
	foreach (i, c; captcha) {
		if (c == captcha[(i + 1) % captcha.length]) {
			sum += to!uint(captcha[i .. i + 1]);
		}
	}
	return sum;
}

unittest {
	assert(solveCaptcha("1122") == 3);
	assert(solveCaptcha("1111") == 4);
	assert(solveCaptcha("1234") == 0);
	assert(solveCaptcha("91212129") == 9);
}
