# generate n random number with significant variance v and to certain sum s
# uses uniform distribution with v variable defining max as how far it can be from average

randtrans <- function  (s, n, v=3)
{

maxn <- s/n*v
r <- numeric(n)
c <-0
i <-1

while (i<n){
        if (s-c<1) {
                r[i]<-0
                i <- i+1
                next
                }
        t <- runif(1,0, maxn)
        if (c+t>s) {next}
        r[i] <- t
        c <- c+ r[i]
        
        i <- i+1
}
# correcting if the sum of generated values less than s
if (c>0){r<-r+(s-c)/n}
# randomizing order of values in resulting vector
r <- sample(r)

return(r)
}

