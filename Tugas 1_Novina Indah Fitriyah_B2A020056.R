#Multiplicative Dan Bernauli
#NIM GENAP
#NOVINA INDAH FITRIYAH_B2A020056

multiplicative_RNG<-function(a,z0,m,n) {
  xb<-matrix(NA,n,3)
  colnames(xb)<-c("aZ","Xb","Ub")
  for (b in 1:n)
  {
    xb[b,1]<-(a*z0)
    xb[b,2]<-xb[b,1]%%m
    xb[b,3]<-xb[b,2]/m
    z0<-xb[b,2]
  }
  hist(xb[,3])
  View(xb)
}
multiplicative_RNG(45, 21139, 417, 150)

Bernouli_novina<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-NULL
  for (z in 1:i) ifelse (X[z]<=p,Y[z]<-1,Y[z]<-0)
  (tabel<-table(Y)/length(Y))
}
Bernouli_novina(150, 0.83)