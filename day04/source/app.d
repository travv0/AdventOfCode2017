import std.stdio : writeln;
import std.algorithm : sort, uniq, count, isPermutation, any;
import std.array : split, array;
import std.range : walkLength;
import std.file : readText;
import std.string : strip;

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
		if (words[i + 1 .. $].any!(w => w.isPermutation(word)))
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
