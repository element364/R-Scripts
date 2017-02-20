library(png)

queue = function(){ 
  e=new.env() 
  q=list() 
  assign("q",q,envir=e) 
  class(e)=c("queue","environment") 
  e 
} 

push.queue=function(e,v){ 
  q=c(v,get("q",envir=e)) 
  assign("q",q,envir=e) 
  v 
} 

pop.queue=function(e){ 
  q=get("q",envir=e) 
  v=q[[length(q)]] 
  if(length(q)==1){ 
    assign("q",list(),e) 
  }else{ 
    assign("q",q[1:(length(q)-1)],e) 
  } 
  return(v) 
} 

check_color <- function (m, i, j, color = 0) {
  if (color == 0) {
    m[i,j,1] == color && m[i,j,2] == color && m[i,j,3] == color  
  } else {
    m[i,j,1] != 0 && m[i,j,2] != 0 && m[i,j,3] != 0 
  }
}

directionsX <- c(1, 0, -1,  0)
directionsY <- c(0, 1,  0, -1)

directionsDiagX <- c(1,  1, -1, -1)
directionsDiagY <- c(1, -1,  1, -1)

print(directionsDiagX)
print(directionsDiagY)

check_cell <- function (m, x, y) {
  for (i in 1:4) {
    nx <- x + directionsX[i]
    ny <- y + directionsY[i]
    
    if (check_color(m, nx, ny, 0)) {
      return (F)
    }
  }
   
  count_diag <- 0
  for (i in 1:4) { 
    nx <- x + directionsDiagX[i]
    ny <- y + directionsDiagY[i]
    
    if (!check_color(m, nx, ny, 1)) {
      count_diag <- count_diag + 1
    }
  }
  
  if (count_diag > 1) {
    return (F)
  }
  
  return (T)
}



removeGrid <- function (m) {
  Nm1 <- nrow(m) - 1
  Mm1 <- ncol(m) - 1
  
  for (x in 2:Nm1) {
    for (y in 2:Mm1) {
        check <- T
        
        if (check_cell(m, x, y)) {
          pp[x,y,1] <<- 1
          pp[x,y,2] <<- 1
          pp[x,y,3] <<- 1
          
          for (i in 1:4) {
            nx <- x + directionsX[i]
            ny <- y + directionsY[i]
            
            pp[nx,ny,1] <<- 1
            pp[nx,ny,2] <<- 1
            pp[nx,ny,3] <<- 1
          }
        } 
    }
    
    if (x %% 10 == 0) {
      print(x)  
    }
  }
  m
}

#source <- 'c:/Users/ELeMeNT/Dropbox/Programming/it open/9/task.png'
#dest <- 'c:/Users/ELeMeNT/Dropbox/Programming/it open/9/task_rr.png'
# source <- 'D:/task.png'
# dest <- "D:/task_r.png"

#p <- readPNG(source = source, native = F)
#pp <- readPNG(source = source, native = F)
#print (p[2,4,1])
#print (p[2,4,2])
#print (p[2,4,3])
#p <- removeGrid(p)
#writePNG(pp, target = dest)

bfs <- function(x, y, l) {
  p[x, y, 1] <<- 0
  p[x, y, 2] <<- 5
  p[x, y, 3] <<- 0
  
  size <<- size + 1
  
  for (i in 1:4) {
    nx <- x + directionsX[i]
    ny <- y + directionsY[i]
    
    if (check_color(p, nx, ny)) {
      bfs(nx, ny, l)
    }
  }
}

find_componetnts <- function () {
  N <- nrow(p)
  M <- ncol(p)
  
  for (x in 1:N) {
    for (i in 1:3) {
      p[x, 1, i] <<- 1
      p[x, M, i] <<- 1      
    }
  }
  for (y in 1:M) {
    for (i in 1:3) {
      p[1, y, i] <<- 1
      p[N, y, i] <<- 1
    }
  }
   
  res <- c()
  
  for (x in 1:N) {
    for (y in 1:M) {
      if (check_color(p, x, y)) {
        size <<- 0
        bfs(x, y, length(res))
        res <- append(res, size)
        print(paste(x, y, size, sep = ' '))
      }
    }
  }
  
  res
}

source <- 'c:/Users/ELeMeNT/Dropbox/Programming/it open/9/task_rr.png'
dest <- 'c:/Users/ELeMeNT/Dropbox/Programming/it open/9/task_rrr.png'

p <- readPNG(source = source, native = F)

size <- 0
sizes <- find_componetnts()

writePNG(p, target = dest)