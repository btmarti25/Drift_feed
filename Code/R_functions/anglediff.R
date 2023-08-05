anglediff <- function(ang1, ang2) {
  dA <- ang1 - ang2
  
  while (dA < -pi) {
    dA <- dA + 2*pi
  }
  
  while (dA > pi) {
    dA <- dA - 2*pi
  }
  
  return(dA)
}