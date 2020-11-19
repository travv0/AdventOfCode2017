import std.stdio : writeln;
import std.file : readText;
import std.string : strip;

void main() @safe {
	Tokenizer tokenizer;
	const tokens = tokenizer.tokenize(readText("input.txt").strip);
	writeln("Part 1: ", tokens.scoreGroups);
	writeln("Part 2: ", tokenizer.garbageCount);
}

private enum Token {
	garbage,
	groupStart,
	groupEnd,
	comma,
	eof,
}

private struct Tokenizer {
	auto garbageCount = 0;

	private enum State {
		inGroup,
		inGarbage,
		canceled,
	}

	private Token[] tokenize(string input) @safe pure {
		auto state = State.inGroup;
		Token[] tokens;

		foreach (c; input) {
			final switch (state) {
			case State.inGroup:
				switch (c) {
				case '{':
					tokens ~= Token.groupStart;
					continue;

				case '<':
					state = State.inGarbage;
					tokens ~= Token.garbage;
					continue;

				case '}':
					tokens ~= Token.groupEnd;
					continue;

				case ',':
					tokens ~= Token.comma;
					continue;

				default:
					throw new Exception("Unexpected character in inGroup state: " ~ c);
				}

			case State.inGarbage:
				switch (c) {
				case '!':
					state = State.canceled;
					continue;

				case '>':
					state = State.inGroup;
					continue;

				default:
					garbageCount++;
					continue;
				}

			case State.canceled:
				state = State.inGarbage;
			}
		}

		tokens ~= Token.eof;

		return tokens;
	}

	unittest {
		Tokenizer tokenizer;
		assert(tokenizer.tokenize("<>") == [Token.garbage, Token.eof]);
		assert(tokenizer.tokenize("<random characters>") == [
				Token.garbage, Token.eof
				]);
		assert(tokenizer.tokenize("<<<<>") == [Token.garbage, Token.eof]);
		assert(tokenizer.tokenize("<{!>}>") == [Token.garbage, Token.eof]);
		assert(tokenizer.tokenize("<!!>") == [Token.garbage, Token.eof]);
		assert(tokenizer.tokenize("<!!!>>") == [Token.garbage, Token.eof]);
		assert(tokenizer.tokenize("<{o\"i!a,<{i<a>") == [
				Token.garbage, Token.eof
				]);

		assert(tokenizer.tokenize("{}") == [
				Token.groupStart, Token.groupEnd, Token.eof
				]);
		assert(tokenizer.tokenize("{{{}}}") == [
				Token.groupStart, Token.groupStart, Token.groupStart,
				Token.groupEnd, Token.groupEnd, Token.groupEnd, Token.eof
				]);
		assert(tokenizer.tokenize("{<a>,<a>,<a>,<a>}") == [
				Token.groupStart, Token.garbage, Token.comma, Token.garbage,
				Token.comma, Token.garbage, Token.comma, Token.garbage,
				Token.groupEnd, Token.eof
				]);
		assert(tokenizer.tokenize("{{<!>},{<!>},{<!>},{<a>}}") == [
				Token.groupStart, Token.groupStart, Token.garbage,
				Token.groupEnd, Token.groupEnd, Token.eof
				]);
	}
}

private uint scoreGroups(const Token[] tokens) @safe pure {
	auto score = 0;
	auto i = 1;
	foreach (token; tokens) {
		switch (token) {
		case Token.groupStart:
			score += i;
			i++;
			break;

		case Token.groupEnd:
			i--;
			break;

		default:
			break;
		}
	}
	return score;
}

unittest {
	Tokenizer tokenizer;
	assert(tokenizer.tokenize("{}").scoreGroups == 1);
	assert(tokenizer.tokenize("{{{}}}").scoreGroups == 6);
	assert(tokenizer.tokenize("{{},{}}").scoreGroups == 5);
	assert(tokenizer.tokenize("{{{},{},{{}}}}").scoreGroups == 16);
	assert(tokenizer.tokenize("{<a>,<a>,<a>,<a>}").scoreGroups == 1);
	assert(tokenizer.tokenize("{{<ab>},{<ab>},{<ab>},{<ab>}}").scoreGroups == 9);
	assert(tokenizer.tokenize("{{<!!>},{<!!>},{<!!>},{<!!>}}").scoreGroups == 9);
	assert(tokenizer.tokenize("{{<a!>},{<a!>},{<a!>},{<ab>}}").scoreGroups == 3);
}
