# paperfold_ai

Attempt to generate a simulator of paper folding with AI

The project is coded in Scala language.

The paper is divided into cells of the same size and identified with a letter. the top-left cell is identified with A and the following adjacent cell on its right is identified by B and so on. On the end of a line the naming of cells is resume on the line below. So for a paper with 4 columns and 3 lines, the cells are identified like in this schema (where | represents the separation between cells):
A | B | C | D
E | F | G | H
I | J | K | L

It will be represented by an array of array of string

## Folding instructions

columns and lines can be folded.

There is 4 type of folding :

- vertical folding taking a left part over the right part. This folding takes as parameter an index representing the separation that is used to fold. We use instruction L for this folding.
- vertical folding taking a right part over the left part. This folding takes as parameter an index representing the separation that is used to fold. We use instruction R for this folding.
- horizontal folding taking a top part over the bottom part. This folding takes as parameter an index representing the separation that is used to fold. We use instruction T for this folding.
- horizontal folding taking a bottom part over the top part. This folding takes as parameter an index representing the separation that is used to fold. We use instruction B for this folding.

So for a paper with 4 columns and 3 lines, a folding instruction "L 2" will result in folding column 0 over column 3 and column 1 over column 2.

So for a paper with 4 columns and 3 lines, a folding instruction "R 3" will result in folding column 3 over column 2.

## Input file

The input file takes as parameter the folowing information

- length of the paper
- width of the paper
- an ordered list of folding instructions

## Examples

For example for a paper of size 2 by 2 and the following list of instructions [{part: "T", index:1},{part: "L", index:1} ] , the expected result is a an array of array of string (of size 1 by 1) and the cell contains "CABD"

## Output

After applying all instructions , it should result in an array of array of string of size 1 by 1 and thus a single string.

## How to run

Input file format: first line is `rows cols`, then one instruction per line (`L|R|T|B` and the pivot index).

```
sbt test
sbt "run examples/example-2x2.txt"
```

The 2×2 example prints `CABD`.
