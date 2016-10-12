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

#angle to hit coordinate x,y
# https://en.wikipedia.org/wiki/Trajectory_of_a_projectile

angleToPoint <- function(x,y,v=Velocity){
  q<-sqrt(v^4-g*(g*x^2+2*y*v^2))
  t1<-atan((v^2+q)/(g*x))
  t2<-atan((v^2-q)/(g*x))
  return(min(t1,t2)*rad2deg)
}
  

trajectory <- function(angle=0,v=Velocity,t=1){
  angle<-angle*1/rad2deg
  x<-v*t*cos(angle)
  y<-v*t*sin(angle)-0.5*g*t^2
  return(c(t,x,y))
}
  
matrix(trajectory(angle=2,t=seq(0,1,0.1)),ncol=3)
