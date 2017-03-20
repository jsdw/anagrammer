package main

import (
    "bufio"
    "fmt"
    "io"
    "os"
    "sort"
)

func main(){

    if len(os.Args) < 2 {
        fmt.Println("Need filename as first and only arg.")
        os.Exit(1)
    }
    filename := os.Args[1]

    file, err := os.Open(filename)
    if err != nil {
        fmt.Println("Error opening file: "+err.Error())
        os.Exit(2)
    }

    m := intoMap(file)
    outputResults(m)

}

// split a byte slice into tokens, lowercasing and
// ignoring any non alpha.
func tokenize(line []byte) [][]byte {

    toks := [][]byte{}
    tok := []byte{}
    for _,b := range line {

        // lowercase:
        if b > 64 && b < 91 {
            b = b + 32
        }

        // if not letter, add tok to toks, else add letter to tok
        if b == 32 {
            if len(tok) > 0 {
                toks = append(toks, tok)
                tok = []byte{}
            }
        } else if b < 97 || b > 122 {
            //ignore non-letters
        } else {
            tok = append(tok, b)
        }

    }
    if len(tok) > 0 {
        toks = append(toks, tok)
    }

    sort.Sort(TokenSort(toks))
    return toks

}

// return the outer and inner hash to be used.
func hashify(line []byte) (string, string) {

    toks := tokenize(line)

    // Go doesnt support []byte or [][]byte as map key,
    // so mangle them into strings for now. more efficient
    // would be to map[int64][]struct{compare,line} where
    // we hash by int and manually compare for equality.
    innerHash := []byte{}
    outerHash := []byte{}
    for _,tok := range toks {

        //space separated (and sorted) words for innerHash:
        innerHash = append(innerHash, 32)
        innerHash = append(innerHash, tok...)

        //aggregate and sort bytes for outerHash
        outerHash = append(outerHash, tok...)

    }

    sort.Sort(ByteSort(outerHash))
    return string(outerHash),string(innerHash)
}

// read liens from some input and hash them into
// our Map.
func intoMap(r io.Reader) Map {

    m := Map{}
    lines := bufio.NewScanner(r)

    for lines.Scan() {

        line := lines.Bytes()
        if len(line) == 0 {
            continue
        }

        outerHash, innerHash := hashify(line)

        inner, isInner := m[outerHash]
        if !isInner {
            inner = Inner{}
            inner[innerHash] = string(line)
            m[outerHash] = inner
        } else {
            inner[innerHash] = string(line)
        }

    }

    return m

}

// iterate through the map and print out the
// discovered anagrams:
func outputResults(outer Map) {

    count := 0
    for _, inner := range outer {

        outputs := make([]string, 0, 1)
        for _, str := range inner {
            outputs = append(outputs, str)
        }

        if len(outputs) > 1 {
            for _, str := range outputs {
                fmt.Println(str)
            }
            count++
            fmt.Println("")
        }

    }
    fmt.Printf("%d sets of anagrams found\n", count)

}

type Map map[string]Inner
type Inner map[string]string

//
// We need to define our sorts; do that here:
//

// sort byte slices
type TokenSort [][]byte
func (a TokenSort) Len() int           { return len(a) }
func (a TokenSort) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a TokenSort) Less(i, j int) bool {

    fst := a[i]
    snd := a[j]

    //sort based on length to shortcut comparisons:
    l1 := len(fst)
    l2 := len(snd)
    if l1 < l2 {
        return true
    } else if l1 > l2 {
        return false
    }

    //lengths match so compare:
    for i, fsti := range fst {
        sndi := snd[i]
        if fsti < sndi { return true }
        if fsti > sndi { return false }
    }
    return false
}

//sort bytes
type ByteSort []byte
func (a ByteSort) Len() int           { return len(a) }
func (a ByteSort) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a ByteSort) Less(i, j int) bool { return a[i] < a[j] }