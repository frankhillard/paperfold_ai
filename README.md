# paperfold_ai

Attempt to generate a simulator of paper folding with AI

The project is coded in Scala language.

The paper is divided into cells of the same size and identified with a letter.
the list of letter (called alphabet) is
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&()\*+,-./0123456789:;<=>?@[]^\_{|}~

the top-left cell is identified with the first letter of the alphabet (in our case A) and the following adjacent cell on its right is identified by the second letter of the alphabet (B) and so on. On the end of a line the naming of cells is resume on the line below.

So for a paper with 4 columns and 3 lines, the cells are identified like in this schema (where | represents the separation between cells):
A | B | C | D
E | F | G | H
I | J | K | L

So for a paper with 7 columns and 8 lines, the cells are identified like in this schema (where | represents the separation between cells):
A | B | C | D | E | F | G
H | I | J | K | L | M | N
O | P | Q | R | S | T | U
V | W | X | Y | Z | a | b
c | d | e | f | g | h | i
j | k | l | m | n | o | p
q | r | s | t | u | v | w
x | y | z | ! | # | $ | %

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

The first attribute is `mod`, with value `FOLD` or `UNFOLD`.

### FOLD

The file must also contain:

- length of the paper (number of rows)
- width of the paper (number of columns)
- an ordered list of folding instructions

### UNFOLD

The file must contain the folded paper as a string (the 1×1 stack). The program returns an ordered list of folding instructions that produce that stack.

## Examples

For example for a paper of size 2 by 2 and the following list of instructions [{part: "T", index:1},{part: "L", index:1} ] , the expected result is a an array of array of string (of size 1 by 1) and the cell contains "CABD"

FOLD input:

```
FOLD
length 2
width 2
T 1
L 1
```

UNFOLD input:

```
UNFOLD
CABD
```

or `UNFOLD CABD`. The expected instructions are `T 1` then `L 1`.

## Output

In FOLD mode, after applying all instructions, the result is an array of array of string. Folding all the way down yields size 1 by 1 and thus a single string.

In UNFOLD mode, the result is the ordered list of folding instructions.

## How to run

```
sbt test
sbt "run examples/example-2x2.txt"
sbt "run examples/example-unfold-cabd.txt"
```

The 2×2 FOLD example prints `CABD`. The UNFOLD example prints:

```
T 1
L 1
```
