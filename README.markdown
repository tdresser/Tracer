# About

This is a ray tracer I'm writing to learn Haskell, and to learn more about ray tracing.

It is fully parallelized.

# Building and Running

You need ghc and cabal, and you'll need to grab the Control.Parallel package.

    cabal install parallel

Then you should be able to build with 

    ghc --make -threaded -O2 tracer.hs

and run with

    ./tracer +RTS -N1

To use more than 1 core, use `+RTS -NX` where `X` is the desired number of cores to use.

It will spit out a new test.tga, containing the current scene.

# Details

test.tga is a sample image, 1024x768 with 2x oversampling.

On my machine it rendered in about 8 seconds with two cores, and about 15 seconds with 1.

* Tga.hs provides an interface for writing to .tga files
* TracerTypes.hs defines all the datatypes
* tracer.hs contains the raytracing code
