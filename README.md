# nbody

This is a simple gravitational n-body simulator<sup>[1](#note1)</sup> able to simulate the motion
of the planets in our solar system.  Simulations are initialized from a file
containing Keplerian elements describing the position of each planetary
body. For example,


    (in-package :nbody)
    (setf (gethash 'sun system) (make-body :position '(0 0 0.0) :velocity '(0 0 0) :mass +solar-mass+))
    {p jupiter 5.20336301d0  0.04839266d0 1.30530  100.55615 14.75385  34.40438  0.03769367578941243d0}
    {p saturn  9.53707032d0  0.05415060d0 2.48446  113.71504 92.43194  49.94432  0.01128632555889845d0}
    {p uranus  19.19126393d0 0.04716771d0 0.76986  74.22988  170.96424 313.23218 0.001723724022815736d0}
    {p neptune 30.06896348d0 0.00858587d0 1.76917  131.72169 44.97135  304.88003 0.0020336868242529066d0}


This initializes the sun to be at the center with the four major planets
described as keplerian elements<sup>[2](#note2)</sup>:

    {p name semi-major-axis eccentricity inclination longitude-of-ascending-node longitude-of-perihelion mean-anomaly mass}

Semi-major-axis is in astronomical units. Inclination, longitude-of-perihelion,
longitude-of-perihelion, and mean-anomaly are in degrees. Mass is
relative to the mass of the sun, where the mass of the sun is defined
as 4 * pi * pi.

The simulation itself is performed using an interactive text prompt. Example:

    
    $ nbody
    > ?
    Usage:
    
    plot: display a plot of the output data
    cp: collection period
    step: stepsize (in years) to use in simulation
    years: number of years to simulate
    run: start simulation
    dump: show variables
    output: file to store output data in
    input: read input from file
    q: quit
    > input major.lisp
    > run
    years:             1000
    step:              0.01d0
    collection period: 1
    input file:        major.lisp
    output file:       output.txt
    -0.1692047048433548d0
    -0.1691972137556261d0
    > plot
    
    

The plot function requires a GL-enabled R environment to be installed
(package "r-base-core" and "r-cran-rgl" in Ubuntu), and produces a
fully interactive plot window that allows rotating, zooming, and
panning:

# ![Major planets](https://raw.github.com/jlowder/nbody/master/doc/major.png)

<a name="note1">1</a>: This started as a port from python to common lisp of nbody.py from "The
    Computer Language Benchmarks Game" at <http://shootout.alioth.debian.org>, but
    this URL is no longer valid.

<a name="note2">2</a>: Orbital element sets can be found online. See
    <http://www.met.rdg.ac.uk/~ross/Astronomy/Planets.html> for a good example.
