######################################################################
# Do some code golf! How little code can you write to make something # 
# interesting? Share the sketch and its code together if you can.    #
######################################################################
set.seed(5)

make_square <- function(id) {
  level <- ceiling(id / 12)
  
  theta <- (c(45, 135, 225, 315) + 
              runif(1, -2 * (level - 1), 2 * (level - 1))) * pi/180
  
  movement <- rnorm(2, 0, level / 90)
  
  x <- c(sqrt(.5) * cos(theta) + movement[1] + id %% 12, NA)
  y <- c(sqrt(.5) * sin(theta) + movement[2] - level, NA)
  
  data.frame(x,y)
}

df <- do.call(rbind, lapply(1:288, make_square))

png("JAN_05.png", 
    width = 2400, height = 2400, res = 300)
par(mar = c(5, 2, 4, 2))
plot(range(df$x, na.rm = TRUE), range(df$y, na.rm = TRUE), 
     type = "n", axes = FALSE, ann = FALSE, asp = 1)
polygon(df$x,df$y)
 <- <- 


## Tweet length #set.seed(5)
f=function(i){l=ceiling(i/12);u=2*(l-1);t=(c(1,3,5,7)*45+runif(1,-u,u))*.0174;m=rnorm(2,0,l/90);s=.707;x=c(s*cos(t)+m[1]+i%%12,NA);y=c(s*sin(t)+m[2]-l,NA);data.frame(x,y)}
df=do.call(rbind,lapply(1:288,f));plot(-5:25,-30:0,"n",ax=F,an=F,as=1);polygon(df$x,df$y)
