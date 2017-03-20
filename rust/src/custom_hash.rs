
extern crate fnv;

use fnv::FnvHashMap;
use std::fs::File;
use std::ascii::AsciiExt;
use std::io::prelude::*;
use std::env;
use std::str;

// 1. read file into lines
// 2. for each line
//    - generate quick hash to roughly sort lines with dupe chars
//    - hash lines by quick hash (into_map)
// 3. iterate over hash, discarding vectors of 1 entry.
// 4. re-hash entries in vectors of more than 1 entry to properly de-dupe
//    (hashing by sorted_bytes and sorted_tokens).
// 5. turn this into actual non-dupe anagram vectors and print out.

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
fn into_map(contents : &str) -> FnvHashMap<QuickKey,Vec<String>> {

    let mut outer_map = FnvHashMap::default();
    for line in contents.lines() {

        if line == "" { continue; }
        let line_str = line.to_string();
        let hash = quick_key(line);
        outer_map
            .entry(hash)
            .or_insert(Vec::new())
            .push(line_str);

    }
    outer_map

}

// take the map and turn into vec of dupes:
fn into_results(m : FnvHashMap<QuickKey,Vec<String>>) -> Vec<Vec<String>> {

    let mut out = Vec::new();
    for values in m.values().cloned() {

        if values.len() == 1 { continue }

        //more than one value. re-hash these
        //using a precise comparison, paying attention to
        //token equality and character equality.
        let mut second = FnvHashMap::default();
        for value in values {
            let (inner_hash, outer_hash) = (sorted_tokens(&value), sorted_bytes(&value));
            second
                .entry(outer_hash)
                .or_insert(FnvHashMap::default())
                .insert(inner_hash, value);
        }

        //collect values from inner hash, which have already
        //been deduped. ignore if only one remains, else push to output.
        for inner in second.values() {
            let values = inner.values().cloned().collect::<Vec<String>>();
            if values.len() == 1 { continue }
            out.push(values);
        }

    }
    out

}

//quick initial key to roughly sort dupes into bins:
fn quick_key(line : &str) -> QuickKey {
    let mut out = 0;
    for &b in line.as_bytes() {
        let b = if b > 64 && b < 91 { b - 65 } else if b > 96 && b < 123 { b - 97 } else { 254 };
        if b > 25 { continue }
        out = out + (1 << (b * 2));
    }
    out
}
type QuickKey = u64;

//sort bytes in line:
fn sorted_bytes(line : &str) -> Vec<u8> {
    let mut line_vec = line
        .bytes()
        .map(|b| if b > 64 && b < 91 { b + 32 } else { b })
        .filter(|&b| b > 96 && b < 123)
        .collect::<Vec<u8>>();

    line_vec.sort();
    line_vec
}

// tokenize a string into words:
fn sorted_tokens(line : &str) -> Vec<String> {
    let mut toks = line
        .to_ascii_lowercase()
        .split(' ')
        .filter(|&c| c != "" )
        .map(|c| c.replace(|c| c < 'a' || c > 'z', ""))
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
