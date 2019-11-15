# HLC

_Henry's Lambda Calculus_.

I envison HLC to inhabit some of the stylistic choices in programming languge design, which also being completely true to the assignment (hw06).

## Grammar

The grammar for HLC is the following:

```

  <program> ::=
  | Module <name> { *<statement> }
  | Executable <name> { *<statement> } Main { <expression> }
  | Main { <expression> }

  <statement> ::=
  | Definition <name> := { <expression> }

  <expression> ::=
  | <primitive>
  | <variable>
  | <name> => <expression>                  # lambda
  | <expression> <expression>               # application
  | <name> := <expression> ; <expression>   # binding
  | ( <expression> )                        # association

  <primitive> ::=
  | unit                                    # unit
  | true | false                            # booleans
  | <natural>                               # naturals 0,1,2,...
  | +<natural> | -<natural>                 # integers

  <variable> ::= <name>

  <name> ::= <string>

```

Note the following conventions:
- `*pattern` is a possibly-empty list of `pattern`

## Running

To run HLC on a program, do

```
  make run source=<source.hlc>
```

If the program has a Main, the Main body will be normalized then output.

For example, to run the program in `examples/sample1.hlc`, do

```
  make run source=examples/sample.hlc
```

To toggle verbosity, do

```
  make run source=<source.hlc> verbosity=<level>
```

where the verbosity levels are

level | description
------|------------
0 | output,
1 | warnings
2 | notes
3 | logs

## Organization



## Building

The behind-the-scenes process for building and running HLC is the following:
1. `ml-build` gathers the sml source files, as specified by `hlc.cm`
2. `ml-build` builds a heap image `main.amd64-darwin`
3. `sml @SMLload main.amd64-darwin <source.hlc>` runs the main function specified in `main.sml`, passing `<source.hlc>` as an argument
