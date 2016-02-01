# Anagrammer

A quick tool to find line-anagrams (excluding those where all of the words are the same) in some ascii file, just to have a play with profiling and whatnot. It takes one command line argument - the text file to read - and errors if this is not provided.

Examples of anagrams (all matches are grouped):

```
'Tis so, indeed.
Is't so, indeed.

A thing
Hang it!

No further, sir.
For his return.
```

Examples of invalid matches (only re-arrangement of words rather than of letters):

```
Hello there sir.
Sir, hello there

He played bagpipes.
Bagpipes he played.
```

"shakespeare.txt" contains an example source text, and "result.txt" the output from running the program on this source text.

# Timings

Currently, a Basic and MutableHash implementation exists. Times taken (average of 10 runs) on the above source text are:

| Implementation | Time taken |
-------------------------------
| Basic          | 0.908s     |
| MutableHash    | 1.410s     |

# Installation

Use the `stack` tool to install, so the full steps will be something like:

```
git clone https://github.com/jsdw/hs-anagrammer
cd hs-anagrammer
stack install
```