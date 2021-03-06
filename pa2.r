library(compiler)
mergelists <- function(a,b) {
al <- length(a$data)
bl <- length(b$data)
r <- list(data=numeric(al+bl), number=0)
r$number <- a$number + b$number
ai <- 1
bi <- 1
j <- 1
while((ai<=al) && (bi<=bl)) {
if(a$data[ai]<b$data[bi]) {
r$data[j] <- a$data[ai]
ai <- ai+1
} else {
r$data[j] <- b$data[bi]
bi <- bi+1
}
j <- j+1
}
r$number <- r$number + j - 1
if(ai<=al) r$data[j:(al+bl)] <- a$data[ai:al]
else if(bi<=bl) r$data[j:(al+bl)] <- b$data[bi:bl]
return(r)
}
mergelists <- cmpfun(mergelists)



mergesort <- function(x) {
l <- length(x)
if(l>1) {
p <- ceiling(l/2)
a <- mergesort(x[1:p])
b <- mergesort(x[(p+1):l])
return(mergelists(a,b))
}
return(list(data=x, number=0))
}
mergesort <- cmpfun(mergesort)


MC <- function(n, B=10000){
experiments <- replicate(B,mergesort(sample(n))$number)
erwartungswert <- mean(experiments)
varianz <- var(experiments)
FtStern <- function(a) {ecdf((experiments - erwartungswert)/sqrt(varianz))(a)}
return(list(F=FtStern, 
v=varianz, 
e=erwartungswert))
}
MC <- cmpfun(MC)