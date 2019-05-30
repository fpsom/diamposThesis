#we have a column of genes with their ensembl id
#we transform that to goid
library(hugene20sttranscriptcluster.db)
X<-select(hugene20sttranscriptcluster.db, head(genenames),"GO", "ENSEMBL")

#we use these goid to extract from godb some characteristics
library(GO.db)
#keytypes(GO.db)
X<-select(Go.db,X$GO, c("TERM","ONTOLOGY"),"GOID")


X<-select(hugene20sttranscriptcluster.db, head(genenames),"ENTREZID", "ENSEMBL")
library(KEGGREST)
#entrezid for example 1027 for one and only gene and returns a list
keggquery<-KEGGREST::keggGet("hsa:entrezid")
