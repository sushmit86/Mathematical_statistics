##Chapter 5 The Boostrap

#Exercise

#9 (medians)
##
ne <- 10000 # n even
no <- 10001 # n odd

wwe <- rnorm(ne) # draw random sample of size ne
wwo <- rnorm(no) # draw random sample of size no

B<-10^4
even.boot <-numeric(B) #save space
odd.boot <-numeric(B)
set.seed(10)
for (i in 1:B)
 {
  x.even<-sample(wwe, ne, replace=TRUE)
  x.odd<-sample(wwo, no, replace=TRUE)
  even.boot[i] <-median(x.even)
  odd.boot[i]  <-median(x.odd)
 }

par(mfrow=c(2,1))
hist(even.boot, xlim = c(-1,1))  #set x range to be
hist(odd.boot, xlim = c(-1,1))  #same in both plots
par(mfrow=c(1,1))               #reset to original
