# smvtrcviz

This repository provides a command line utility to transform XML traces generates by [nuXmv](https://nuxmv.fbk.eu) into more readable HTML tables.
You can generate such XML traces for counter examples by running nuXmv in interactive mode and using the `show_traces -p 4` command.

Furthermore, it supports mapping such traces to LaTeX tables in a specific format.
To use this feature, your trace must be in accordance with the model of my [master thesis](https://github.com/felixlinker/ifc-rv-thesis/).
I use this for my master thesis.
If you read this, you will probably never want to use this feature.

## Installation

Clone the repository and make a standalone executable ready with `lein uberjar`.

## Usage

```
$ java -jar smvtrcviz-0.1.0-standalone.jar [args]
```

## Options

* `-i, --input FILE` - Set the input file; if no file is provided stdin is read

## Example

```
$ cat trace.xml | java -jar smvtrcviz-0.1.0-standalone.jar > trace.html
```
