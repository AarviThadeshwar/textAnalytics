setwd("/Users/aarvithadeshwar/Desktop/big data/analysethis.txt")

rtt<-VCorpus(DirSource(".", ignore.case=TRUE, mode="text"))
rtt
inspect(rtt)
str(rtt)

rttdtm <- DocumentTermMatrix(rtt)
rttdtm

rtttdm<- TermDocumentMatrix(rtt)
rtttdm

inspect(rtttdm[1:30,1])

rttlow<- tm_map(rtt, content_transformer(tolower))
rttlow
str(rttlow)

remNumPunc <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
rttclean<- tm_map(rttlow, content_transformer(remNumPunc))
rttclean
str(rttclean)

rttstopwords<- c(stopwords('english'))
rttstopwords

rttstop<-tm_map(rttclean, removeWords, rttstopwords)
inspect(rttstop)

###c
rtttdm2<- TermDocumentMatrix(rttstop, control = list(wordlengths=c(1,Inf)))
rtttdm2

freqTerms<- findFreqTerms(rtttdm2,lowfreq = 4)

termFre<- rowSums(as.matrix(rtttdm2))
termFreSub<- subset(termFre, termFre>=6)
termFDF<- as.data.frame(names(termFre), freq= termFre)
termFDF

d <- dist(rtttdm2, method = "euclidean")
fits<- hclust(d, method = "ward.D2")
plot(fits, cex=0.6)

rect.hclust(fits, k=4, border = 2:5)
par(mfrow=c(3,1))

###dendrogram
hc<-hclust(dist(rtttdm2, method = "euclidean"), method = "ward.D2")
hcd<-as.dendrogram(hc)
plot(hcd, main="Main")
plot(cut(hcd, h=54.7)$upper, 
     main="Upper tree of cut at h=54.7")
plot(cut(hcd, h=54.7)$lower[[2]], 
     main="Second branch of lower tree with cut at h=54.7")

###word cloud
rtttm<-as.matrix(rtttdm2)
word.freq<-sort(rowSums(rtttm), decreasing=T)
word.freq

rttpal<- brewer.pal(9, "BuGn")
rttpal<-rttpal[-(1:4)]
wordcloud(words = names(word.freq), freq = word.freq, min.freq=30, random.order=F, 
          colours=rttpal)
        
####
s<-as.String(rttlow)

###zipfR
bootstrap.confint(rtttm, level=0.95, method = "normal")
my.spc<-read.spc("/Users/aarvithadeshwar/Desktop/big data/analysethis.txt")

rttdtm<-DocumentTermMatrix(rttstop)
View(rttdtm)
freq<-colSums(as.matrix(rttdtm))
rttdtm
length(freq)
ord<-order(freq,decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]

rttdtmr<-DocumentTermMatrix(rttstop, control = list(wordLengths=c(4,20)))
rttdtmr
freqr<-colSums(as.matrix(rttdtmr))
ordr<-order(freqr,decreasing=TRUE)
freqr[head(ordr)]
freqr[tail(ordr)]

View(freqr)

write.table(freqr, file = "freqr.txt", sep = "\n",
            row.names = FALSE)

rtt.tfl<-read.tfl("/Users/aarvithadeshwar/Desktop/freqr.txt")
summary(rtt.tfl)

rtt.spc<- tfl2spc(rtt.tfl)
summary(rtt.spc)

plot(rtt.spc,log="x", m.max=10)

zm<-lnre("zm",rtt.spc)
zm.spc<-lnre.spc(zm,N(rtt.spc))

plot(zm.spc, rtt.spc, legend = c("expected", "observed"), log="x")

zm.spc<-lnre.spc(zm,N(rtt.spc), variances = TRUE)
plot(zm.spc, log = "x")

summary(rtt.emp.vgc)

zm.vgc<- lnre.vgc(zm, (1:100)*70, variances=TRUE)
summary(zm.vgc)
print(zm.vgc)
plot(zm.vgc)

zm.vgc<-vgc.interp(zm.spc,N(zm.vgc), m.max = 1)
stop("cannot interpolate from incomplete frequency spectrum")
stop("cannot interpolate from expected frequency spectrum")
