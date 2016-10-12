# average dart velocity
# this will be lower at longer ranges.  Ideally an air resistance computation would be added
Velocity <- 30 # meters/second
# gravity
g <- 9.81 #meters/second-squared
#radians to degrees
rad2deg <- 360/pi

BDrop <- function(range,V=Velocity) {
  t <- range/V
  return(0.5* g * t^2)
}
Angle <- function(range,V=Velocity) {
  t <- range/V
  BDrop <- 0.5* g * t^2
  return(atan(BDrop/range)*rad2deg)
}

