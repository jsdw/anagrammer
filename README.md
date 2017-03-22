# Anagrammer

Code in different languages aimed at completing the anagram challenge with rough benchmarks.

The anagram challenge is this: given an input file, find all groups of lines that contain all of the same letters but not the same words, where a word is defined as a group of tokens separated from other words by spaces and then ignoring any non letter (so for example "t'is" is the word "tis", "good-morrow" is the word "goodmorrow", "hello-there sir." is the words "hellothere" and "sir").

Examples of groups of lines that are valid anagrams according to this:

```
'Tis so, indeed.
Is't so, indeed.

A thing
Hang it!

Good-morrow, cousin.
Good morrow, cousin.

No further, sir.
For his return.
```

Examples of invalid matches (letters are the same but words are as well):

```
T'is morning, sir.
Sir, tis morn-ing

Hello there sir.
Sir, hello there

He played bagpipes.
Bagpipes he played.
```

"shakespeare.txt" contains an example source text, and "result.txt" the output from running the program on this source text.

# Timings

Several implementations exist in an attempt to etch out as much speed as possible. Times taken (average of 10 runs on a 4Ghz Intel i7 iMac, 8GB 1600Mhz DDR3 using `time for i in {0..9}; do program-exe; done`) on the "shakespeare.txt" source text are:

| Implementation | Language | Created By    | Time taken |
|----------------|----------|---------------|------------|
| Basic          | Haskell  | jsdw          | 0.929s     |
| MutableHash    | Haskell  | jsdw          | 1.290s     |
| Bodigrim       | Haskell  | Bodigrim      | 0.182s     |
| Bartavelle     | Haskell  | /r/bartavelle | 0.149s     |
| Kuribas        | Haskell  | /r/kuribas    | 0.111s     |
| Basic          | Rust     | jsdw          | 0.417s     |
| Basic (u8)     | Rust     | jsdw          | 0.416s     |
| Two Stage      | Rust     | jsdw          | 0.135s     |
| Custom Hash    | Rust     | jsdw          | 0.086s     |
| Basic          | Go       | jsdw          | 0.545s     |
| Basic          | Java     | Chris C       | 0.686s     |

# Installation

First, clone the repo and cd into it

```
git clone https://github.com/jsdw/hs-anagrammer
cd hs-anagrammer
```

## Haskell

Use the `stack` tool to install, so the full steps inside the cloned repo will be something like:

```
cd haskell
stack install
```

This will put binaries in `~/.local/bin` or something similar.

## Rust

Examples compiled using Rust 1.16.0

Use the `cargo` tool to install (`--release` applies optimisations).

```
cd rust
cargo install --release
```

Binaries will end up in `~/.cargo/bin` or something similar.

## Go

Examples compiled using Go 1.8

```
cd go
export GOPATH=$(pwd)
go install ...
```

Binaries will be in the `go/bin` folder

## Java

Examples compiled using Java 1.8.0_121

To compile:

```
cd java
javac *.java
```

The compiled `.class` files can then be run like:

```
javac AnagramFinderBasic.class ../shakespeare.txt
```

# License

This is in the public domain; do whatever you like with it!
