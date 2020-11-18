import std.stdio : writeln;
import std.typecons : Nullable;
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

		tokenLoop: foreach (c; input) {
			final switch (state) {
			case State.inGroup:
				switch (c) {
				case '{':
					tokens ~= Token.groupStart;
					continue tokenLoop;

				case '<':
					state = State.inGarbage;
					tokens ~= Token.garbage;
					continue tokenLoop;

				case '}':
					tokens ~= Token.groupEnd;
					continue tokenLoop;

				case ',':
					tokens ~= Token.comma;
					continue tokenLoop;

				default:
					throw new Exception("Unexpected character in inGroup state: " ~ c);
				}

			case State.inGarbage:
				switch (c) {
				case '!':
					state = State.canceled;
					continue tokenLoop;

				case '>':
					state = State.inGroup;
					continue tokenLoop;

				default:
					garbageCount++;
					continue tokenLoop;
				}

			case State.canceled:
				state = State.inGarbage;
				continue tokenLoop;
			}
		}

		tokens ~= Token.eof;

		return tokens;
	}

	unittest {
		assert(tokenize("<>") == [Token.garbage, Token.eof]);
		assert(tokenize("<random characters>") == [Token.garbage, Token.eof]);
		assert(tokenize("<<<<>") == [Token.garbage, Token.eof]);
		assert(tokenize("<{!>}>") == [Token.garbage, Token.eof]);
		assert(tokenize("<!!>") == [Token.garbage, Token.eof]);
		assert(tokenize("<!!!>>") == [Token.garbage, Token.eof]);
		assert(tokenize("<{o\"i!a,<{i<a>") == [Token.garbage, Token.eof]);

		assert(tokenize("{}") == [Token.groupStart, Token.groupEnd, Token.eof]);
		assert(tokenize("{{{}}}") == [
				Token.groupStart, Token.groupStart, Token.groupStart,
				Token.groupEnd, Token.groupEnd, Token.groupEnd, Token.eof
				]);
		assert(tokenize("{<a>,<a>,<a>,<a>}") == [
				Token.groupStart, Token.garbage, Token.comma, Token.garbage,
				Token.comma, Token.garbage, Token.comma, Token.garbage,
				Token.groupEnd, Token.eof
				]);
		assert(tokenize("{{<!>},{<!>},{<!>},{<a>}}") == [
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
	assert("{}".tokenize.scoreGroups == 1);
	assert("{{{}}}".tokenize.scoreGroups == 6);
	assert("{{},{}}".tokenize.scoreGroups == 5);
	assert("{{{},{},{{}}}}".tokenize.scoreGroups == 16);
	assert("{<a>,<a>,<a>,<a>}".tokenize.scoreGroups == 1);
	assert("{{<ab>},{<ab>},{<ab>},{<ab>}}".tokenize.scoreGroups == 9);
	assert("{{<!!>},{<!!>},{<!!>},{<!!>}}".tokenize.scoreGroups == 9);
	assert("{{<a!>},{<a!>},{<a!>},{<ab>}}".tokenize.scoreGroups == 3);
}
