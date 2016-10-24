
This is a selection of scripts and programs that I use daily in my
research. I'm putting them here for (1) backup and easy version
control (2) that I think they might be useful for others as well.

I work on [Pycket](https://github.com/pycket/pycket), therefore on
[Racket](https://github.com/racket/racket), trying to understand what
happens when we try to use Racket bytecode compiler as a frontend for
Pycket. There are a bunch of optimizations in the Racket compiler that
either helps or makes it worse for the Pycket's tracing JIT. I'm
trying to make sense out of it.

I use [PBS](https://en.wikipedia.org/wiki/Portable_Batch_System) on a
cluster and [ReBench](https://github.com/smarr/ReBench) for
benchmarking and [R](https://www.r-project.org/) to analyze the
results.

- `unroll.rkt` -- This defines a useful macro "define/unroll" which
 takes a regular function definition and a number, and unrolls the
 recursive calls (basically self inlines) that many times.

- `lambda-lift.rkt` -- This is a very crappy first take on lambda
  lifting. It works all-right but it's too messy to be useful for
  anyone (including me) right now. It basically defines a
  "define/lift" macro (looks very similar to the regular define) which
  searches for "lift-this" literals inside its body and does really
  hacky stuff to lift those expressions. I suggest to avoid
  hacking/trying-to-understand until it doesn't have the "TODO" stuff
  at the top.

- `gen-scripts.rkt` -- If you're using PBS on a cluster for benchmarking
purposes, you might find this useful. It produces bash-scripts ready
to be "qsub"ed, and a master script to "qsub" all, so all you have to
do is to give it a template for the script (what to run), and just
"./run-all-of-them.sh"

- `timings-to-csv.rkt` -- If you're using ReBench to run your
 benchmarks, you might use this to read a collection of outputs from
 those benchmarks and put all of them into one csv (so that you can
 process it easily or just copy/paste it onto a spreadsheet). (see
 disclaimer)

- `confidence-interval.rkt` -- This computes bootstrap confidence
 intervals for a given list of results. Currently, you give it a
 directory, and a benchmark name, and it writes down the conf
 intervals for numerous settings. (And it may alo assume to be given
 1000 elements, see disclaimer)

- `analyze-conf-intervals.rkt` -- It has a cool name, but all it does is
 to read and organize what *confidence-interval.rkt* has written.

- `analyze-rebench-output.rkt` -- This can do bunch of cool stuff with a
 result (or a collection of results) of *rebench*. (I often use "rst"
 extension for rebench out) It can read a "rst" and turn it into a
 list (believe me it's not as trivial as it sounds), take the average,
 or write those lists somewhere, or even produce an input for our R
 script (see [pycket-bench](https://github.com/pycket/pycket-bench)).

- `simplify-ast.rkt` -- This takes a JSON ast from pycket and turns it
 into sexp, while simplifying it in the process (removes the
 boilerplate code, simplifies some ast nodes). Beware: this is not an
 ast optimizaiton, the resulting code is not to be run in any way. At
 some point I needed to actually see and read the ast to see what's
 going on, so while this increases readability, it also removes code
 sections, rendering the program effectively useless.

- `zo-inspect.rkt` -- This takes a "compiled/<file>_rkt.zo" and
 "compiled/<file>_rkt.dep" and disects through the compilation top,
 (and possibly runs the bytecode expander in
 [Pycket](https://github.com/pycket/pycket)). It's messy, because I'm
 too lazy. If you want to have a general purpose useful tool to
 inspect Racket bytecode, see
 [Zordoz](https://github.com/bennn/zordoz).


Disclaimer: Most of these codes are *not* general purpose, I wrote
them to solve specific tasks on particular settings, so they may not
work for you as conveniently as you might think. But I promise they
don't do anything harmful (#whatcouldgowrong).
