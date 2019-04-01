## Illustration Tests multiples

# Motivation

M1 = matrix(rnorm(100*10000), 100, 10000)
M2 = matrix(rnorm(100*10000), 100, 10000)
M = rbind(M1, M2)
y = rep(1:2, each = 100)

p = apply(M, 2, function(x) t.test(x~y)$p.value)
padj_bonf = p*NCOL(M)
sum(padj_bonf)

which(padj_bonf<0.05)
padj_bonf[padj_bonf>1] = 1

pbonf = p.adjust(p, method = "bonferroni")
pBH = p.adjust(p, method = "BH")

P = data.frame(p = p, pbonf = pbonf, pBH=pBH)
head(P, 30)



load("../data/Golub.Rdata")

# Student test for all the valriables simultaneously

p = apply(X.golub, 2, function(x) t.test(x~y.golub)$p.value)

sum(p<0.05) # 1078 p value < 0.05

# Ilustration Bonferonni

my.pbonf = p*NCOL(X.golub)
pbonf = p.adjust(p, method = "bonferroni")
pBH = p.adjust(p, method = "BH")
P = data.frame(p = p, BONF = pbonf, BH = pBH)
head(P, 30)

# Illustration FDR
nbcol = NCOL(X.golub)
psort <- sort(p)
myfdr <- NULL
for (i in 1:nbcol)
  myfdr <- c(myfdr, p[i] <= match(p[i],psort) * .05/nbcol)

pfdr = p.adjust(p, method = "BH") # or equivalently p.adjust(p, method = "fdr")

FDR = data.frame(mydfr = myfdr, fdr = pfdr)
head(FDR, 30)

