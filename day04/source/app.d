import std.stdio : writeln;
import std.algorithm : sort, uniq, count, permutations, any, canFind;
import std.array : split, array;
import std.range : walkLength;
import std.file : readText;
import std.string : strip;
import std.conv : to;

void main() @safe {
	immutable input = readText("input.txt");
	immutable passphrases = input.strip.split('\n');
	writeln("Part 1: ", passphrases.count!(p => !p.hasDupes));
	writeln("Part 2: ", passphrases.count!(p => !p.hasAnagrams));
}

private bool hasDupes(string passphrase) @safe pure {
	auto words = passphrase.split(' ');
	return words.length != words.sort.uniq.walkLength;
}

unittest {
	assert(!"aa bb cc dd ee".hasDupes);
	assert("aa bb cc dd aa".hasDupes);
	assert(!"aa bb cc dd aaa".hasDupes);
}

private bool hasAnagrams(string passphrase) @safe pure {
	immutable words = passphrase.split(' ');
	foreach (i, word; words) {
		if (word.array.permutations.any!(p => words[i + 1 .. $].canFind(to!string(p))))
			return true;
	}
	return false;
}

unittest {
	assert(!"abcde fghij".hasAnagrams);
	assert("abcde xyz ecdab".hasAnagrams);
	assert(!"a ab abc abd abf abj".hasAnagrams);
	assert(!"iiii oiii ooii oooi oooo".hasAnagrams);
	assert("oiii ioii iioi iiio".hasAnagrams);
}
