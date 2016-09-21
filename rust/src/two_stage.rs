
extern crate fnv;

use fnv::{FnvHashMap, FnvHashSet};
use std::fs::File;
use std::ascii::AsciiExt;
use std::io::prelude::*;
use std::env;
use std::str;

// 1. read file into lines
// 2. for each line
//    - turn into sorted letters key
//    - push into hash, keyed by this key, appending matching entries to list
// 3. look through lists and dedupe by tokenizing and sorting tokens, ignoring output lists of length 1.
// 4. print output of 3

fn main() {

    let filename = env::args().nth(1).expect("Must provide first argument: filename to find anagrams in");
    let contents = read(&filename);
    let anagrams = into_results( into_map(&contents) );
    print_results(&anagrams);

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
        let line_str = line.to_string();
        let hash = hashify(&line_str);
        let mut inner_vec = outer_map
            .entry(hash)
            .or_insert(Vec::new());

        inner_vec.push(line_str);

    }
    outer_map

}

// take the map and turn into vec of dupes:
fn into_results(m : Map) -> Vec<Vec<String>> {

    let mut out = Vec::new();
    for values in m.values() {

        if values.len() == 1 { continue }

        let mut deduped = FnvHashSet::default();
        let mut filtered = Vec::new();

        for value in values {
            let tokenized = tokenize(value);
            if !deduped.contains(&tokenized) {
                deduped.insert(tokenized);
                filtered.push(value.clone());
            }
        }

        if filtered.len() > 1 {
            out.push(filtered);
        }

    }
    out

}

//sort bytes in line:
fn hashify(line : &String) -> Vec<u8> {
    let mut line_vec = line
        .clone()
        .into_bytes()
        .into_iter()
        .map(|b| if b > 64 && b < 91 { b + 32 } else { b })
        .filter(|&b| b > 96 && b < 123)
        .collect::<Vec<u8>>();

    line_vec.sort();
    line_vec
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

//read output from maps to print, ignoring single entries:
fn print_results(anagrams : &Vec<Vec<String>>) {

    for dupes in anagrams {
        for dupe in dupes {
            println!("{}", dupe);
        }
        println!("");
    }
    println!("{} sets of anagrams found", anagrams.len());

}


type Map<'a> = FnvHashMap< Vec<u8>, Vec<String> >;