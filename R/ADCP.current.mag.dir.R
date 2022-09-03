## Function to calculate the direction and magnitude of ADCP current from u and v components
## Function takes u and v velocity components as input

current.direction <- function(u, v) {
  
  new.current <- numeric()
  temp <- numeric()
  
  for (i in 1:length(u)) {
    temp[i] <- atan(v[i] / u[i])
    new.current[i] <- u[i] / cos(temp[i])
  }
  return(data.frame(Magnitude = new.current,
                    Direction = temp)
  )
}