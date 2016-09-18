
extern crate fnv;

use fnv::FnvHashMap;
use std::fs::File;
use std::ascii::AsciiExt;
use std::io::prelude::*;
use std::env;
use std::str;

// Our basic algorithm, taking a similar approach to Haskell/Basic. Various improvements can be made from here.
//
// 1. read file into lines
// 2. for each line
//    - tokenize into words (lowercase, strip all non ascii) and sort.
//      This allows us to filter all dupe lines (same words despite diff order)
//    - concat the tokens and sort. This catches all lines that are the same
//      and so becomes our outer hash starting point
// 3. using these hashes, form a HashMap< sortedTokens, HashMap< sortedWords, originalLine > >
// 4. every originalLine present under each sortedTokens entry is an anagram of eachother. Print them.

fn main() {

    let filename = env::args().nth(1).expect("Must provide first argument: filename to find anagrams in");
    let contents = read(&filename);

    let outer_map = into_map(&contents);

    print_results(&outer_map);

}

// read our file to a string, panicking if it can't:
fn read(filename : &str) -> String {
    let mut contents = String::new();
    File::open(&filename)
        .expect("File not found")
        .read_to_string(&mut contents)
        .expect("File could not be read");
    contents
}

// put contents of a file into a hash map:
fn into_map(contents : &String) -> Map {

    let mut outer_map = FnvHashMap::default();
    for line in contents.lines() {

        if line == "" { continue; }

        let (outer_hash, inner_hash) = hashify(&line.to_string());
        let mut inner_map = outer_map
            .entry(outer_hash)
            .or_insert(FnvHashMap::default());

        inner_map.insert(inner_hash, line);

    }
    outer_map

}

// tokenize a string into words:
fn tokenize(line : &String) -> Vec<String> {
    let mut toks = line
        .to_ascii_lowercase()
        .split(|c| c < 'a' || c > 'z' )
        .filter(|&c| c != "" )
        .map(|s| s.to_string())
        .collect::<Vec<String>>();

    toks.sort();
    toks
}

// generate our inner and outer comparison values:
fn hashify(line : &String) -> ( Vec<u8>, Vec<String> ) {

    let tokens = tokenize(line);

    //push all letters from tokens into sorted vec:
    let mut letters = Vec::new();
    for word in &tokens {
        letters.extend( word.as_bytes().clone() );
    }
    letters.sort();

    (letters, tokens)

}

//read output from maps to print, ignoring single entries:
fn print_results(map : &Map) {

    let mut count = 0;
    for inner_map in map.values() {

        let anagrams = inner_map.values().map(|&v| v).collect::<Vec<&str>>();
        if anagrams.len() > 1 {

            count += 1;
            for one in anagrams {
                println!("{}", one);
            }
            println!("");

        }

    }
    println!("{} sets of anagrams found", count);

}


type Map<'a> = FnvHashMap< Vec<u8>, FnvHashMap< Vec<String>, &'a str > >;