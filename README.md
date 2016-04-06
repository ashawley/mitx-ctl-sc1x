## Supply Chain Fundamentals in Scala

Solutions to the practice problems, not the graded assignments, using
the [Scala](http://scala-lang.org) programming language and a
numerical processing library for Scala, called
[Breeze](http://github.com/scalanlp/breeze).  Answers are validated
using [specs2](http://specs2.org), a test framework for Scala.

* [MITx: CTL.SC1x Supply Chain Fundamentals](https://www.edx.org/course/supply-chain-fundamentals-mitx-ctl-sc1x)

Source code is released under the MIT license (see LICENSE):

Copyright (C) 2016, Aaron S. Hawley

### Running the test suite

Use the Scala simple build tool, [sbt](http://www.scala-sbt.org):

    $ sbt
    > test
    [info] Week1Spec
    [info] 
    [info] + W1PP2P1
    [info] + W1PP2P2
    [info] + W1PP2P3
    [...]
    [info] + W1PP7P3
    [info] + W1PP7P4
    [info] + W1PP7P5
    [info] 
    [info] 
    [info] Total for specification Week1Spec
    [info] Finished in 1 second, 334 ms
    [info] 13 examples, 0 failure, 0 error
    [info] 
    [...]
    [info] 
    [info] Passed: Total 34, Failed 0, Errors 0, Passed 34
    [success] Total time: 7 s

To automatically compile and re-run the tests when a source file
changes, using triggered execution feature of sbt

    > ~test

To run a single specification, use `testOnly`:

    > ~testOnly org.edx.mitx.ctl.sc1x.Week3Spec

To run only tests in a spec that match a regular expression:

    > ~testOnly org.edx.mitx.ctl.sc1x.Week2Spec -- ex W2PP4P3C
    > testOnly org.edx.mitx.ctl.sc1x.Week2Spec -- ex W2PP4P3C
    [info] Week2Spec
    [info] 
    [info] + W2PP4P3C
    [info] 
    [info] Total for specification Week2Spec
    [info] Finished in 2 seconds, 454 ms
    [info] 1 example, 0 failure, 0 error
    [info] 
    [info] Passed: Total 1, Failed 0, Errors 0, Passed 1
    [success] Total time: 4 s
    
### Using the REPL

To run the sbt console, with access to the breeze library:

    > console
    [info] Starting scala interpreter...
    [info] 
    Welcome to Scala 2.11.8 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_40).
    Type in expressions for evaluation. Or try :help.
    
    scala> breeze.linalg.DenseVector(1d,2,3,4,5)
    res0: breeze.linalg.DenseVector[Double] = DenseVector(1.0, 2.0, 3.0, 4.0, 5.0)
    
    scala> breeze.stats.meanAndVariance(res0)
    res1: breeze.stats.MeanAndVariance = MeanAndVariance(3.0,2.5,5)
    
    scala> breeze.stats.stddev(res0)
    res2: Double = 1.5811388300841898
