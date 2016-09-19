
extern crate fnv;

use std::{env, str};
use std::fs::File;
use std::io::prelude::*;
use fnv::FnvHashMap;

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
fn read(filename : &str) -> Vec<u8> {
    let mut f = File::open(&filename).expect("File not found");
    let file_len = f.metadata().expect("Error getting file metadata").len();
    let mut buffer = Vec::with_capacity(file_len as usize + 1);

    f.read_to_end(&mut buffer).expect("Error loading entire file");

    buffer
}

// put contents of a file into a hash map:
fn into_map(contents : &Vec<u8>) -> Map {

    let mut outer_map = FnvHashMap::default();
    let mut line = Vec::new();
    for &byte in contents {

        // append to line as long as bytes arent 10 (unix newline)
        if byte != 10 {
            line.push(byte);
            continue;
        }

        // if line has a length, hash it!
        if line.len() != 0 {
            let (outer_hash, inner_hash) = hashify(&line);
            let mut inner_map = outer_map
                .entry(outer_hash)
                .or_insert_with(|| FnvHashMap::default());

            inner_map.insert(inner_hash, line);
            line = Vec::new();
        }

    }
    outer_map

}

// tokenize a string into words:
fn tokenize(line : &Vec<u8>) -> Vec<Vec<u8>> {

    let mut tok = Vec::new();
    let mut tokens = Vec::new();

    for &b in line {

        //lowercase:
        let b = if b > 64 && b < 91 { b + 32 } else { b };

        //if not letter, add token to toks. else add letter to tok.
        if b < 97 || b > 122 {
            if tok.len() > 0 {
                tokens.push(tok);
                tok = Vec::new();
            }
        } else {
            tok.push(b);
        }

    }
    if tok.len() > 0 {
        tokens.push(tok)
    }

    tokens.sort();
    tokens
}

// generate our inner and outer comparison values:
fn hashify(line : &Vec<u8>) -> ( Vec<u8>, Vec<Vec<u8>> ) {

    let tokens = tokenize(line);

    //push all letters from tokens into sorted vec:
    let mut letters = Vec::new();
    for word in &tokens {
        letters.extend(word);
    }
    letters.sort();

    (letters, tokens)

}

//read output from maps to print, ignoring single entries:
fn print_results(map : &Map) {

    let mut count = 0;
    for inner_map in map.values() {

        let anagrams = inner_map.values().cloned().collect::<Vec<Vec<u8>>>();
        if anagrams.len() > 1 {

            count += 1;
            for one in anagrams {
                println!("{}", match String::from_utf8(one) {
                    Ok(s) => s,
                    Err(_) => "ERROR!".to_string()
                });
            }
            println!("");

        }

    }
    println!("{} sets of anagrams found", count);

}


type Map = FnvHashMap< Vec<u8>, FnvHashMap< Vec<Vec<u8>>, Vec<u8> >>;