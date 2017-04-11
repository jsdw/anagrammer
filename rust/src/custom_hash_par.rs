
extern crate fnv;
extern crate crossbeam;

use fnv::FnvHashMap;
use std::fs::File;
use std::ascii::AsciiExt;
use std::io::prelude::*;
use std::sync::mpsc::{channel,Sender,Receiver};
use std::env;
use std::str;

// how many threads do we want? The work done on the threads is quite small, and so adding more threads
// doesnt help beyond 2..
const COUNT : usize = 2;

fn main() {

    let filename = env::args().nth(1).expect("Must provide first argument: filename to find anagrams in");
    let contents = read(&filename);

    crossbeam::scope(|scope| {

        // make a channel and thread to handle printing:
        let (tx_print, rx_print) = channel();
        printer(&scope, rx_print);

        // make us some workers to process lines:
        let mut workers = Vec::with_capacity(COUNT);
        for _ in 0..COUNT {
            workers.push( worker(&scope, tx_print.clone()) )
        }

        // read lines, splitting to worker threads to do the lifting.
        for line in contents.lines() {
            if line == "" { continue }
            let hash = very_quick_key(line);
            workers
                .get(hash % COUNT).unwrap()
                .send(line).unwrap();
        }

    });

}

fn worker<'a>(scope: &crossbeam::Scope<'a>, out: Sender<Vec<&'a str>>) -> Sender<&'a str> {
    let (tx, rx) = channel();
    scope.spawn(move || {

        let mut outer_map = FnvHashMap::default();
        for line in rx {
            let hash = quick_key(line);
            outer_map
                .entry(hash)
                .or_insert(Vec::with_capacity(1))
                .push(line);
        }

        into_results(outer_map,out);

    });
    tx
}

fn printer<'a>(scope: &crossbeam::Scope<'a>, rx: Receiver<Vec<&'a str>>) {
    scope.spawn(move || {

        let mut count = 0;
        for dupes in rx {
            count = count + 1;
            for dupe in dupes {
                println!("{}", dupe);
            }
            println!("");
        }
        println!("{} sets of anagrams found", count);

    });
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

// take the map and turn into vec of dupes:
fn into_results<'a>(m : FnvHashMap<QuickKey,Vec<&'a str>>, chan : Sender<Vec<&'a str>>) {

    for values in m.values() {

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
            let out = inner.values().map(|&&v| v).collect::<Vec<&str>>();
            if out.len() > 1 { chan.send(out).unwrap(); }
        }

    }

}

//very quick initial key to roughly sort lines onto threads. Here we just
//count the number of A's in a line to spread the work. Less robust across
//datasets but pretty fast!
fn very_quick_key(line : &str) -> usize {
    let mut out = 0;
    for &b in line.as_bytes() {
        if b == 65 || b == 97 { out = out + 1 }
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