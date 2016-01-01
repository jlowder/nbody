require(rgl)
ssystem <- function (filename)
{
   ss <- read.csv(filename)
   mercury <- ss[ss$name == "MERCURY",-1]
   venus <- ss[ss$name == "VENUS",-1]
   earth <- ss[ss$name == "EARTH",-1]
   mars <- ss[ss$name == "MARS",-1]
   neptune <- ss[ss$name == "NEPTUNE",-1]
   pluto <- ss[ss$name == "PLUTO",-1]
   eris  <- ss[ss$name == "ERIS",-1]
   makemake <- ss[ss$name == "MAKEMAKE",-1]
   sedna <- ss[ss$name == "SEDNA",-1]
   uranus <- ss[ss$name == "URANUS",-1]
   saturn <- ss[ss$name == "SATURN",-1]
   jupiter <- ss[ss$name == "JUPITER",-1]
   sun <- ss[ss$name == "SUN",-1]
   plot3d(neptune,aspect=FALSE,col="blue")
   plot3d(uranus,col="green",add=TRUE)
   plot3d(saturn,col="pink",add=TRUE)
   plot3d(jupiter,col="purple",add=TRUE)
   #plot3d(sun,col="yellow",add=TRUE)
   plot3d(pluto,col="orange",add=TRUE)
   plot3d(eris,col="green",add=TRUE)
   plot3d(makemake,col="red",add=TRUE)
   plot3d(sedna,col="purple",add=TRUE)
   plot3d(mars,col="red",add=TRUE)
   plot3d(earth,col="blue",add=TRUE)
   plot3d(venus,col="green",add=TRUE)
   plot3d(mercury,col="orange",add=TRUE)
}
ssystem(commandArgs(TRUE)[1])
play3d(function(time) {Sys.sleep(0.01); list()} )
