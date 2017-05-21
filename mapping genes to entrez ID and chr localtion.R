source("https://bioconductor.org/biocLite.R")
biocLite("org.Hs.eg.db")
?biocLite

library(org.Hs.eg.db)

###############
## Example of how to convert gene name to entrez ID:
?org.Hs.eg.db

columns(org.Hs.eg.db)
ls("package:org.Hs.eg.db")
?org.Hs.egCHRLOC



gene=c("PRKAR1A", "PRKCB")
?mget
?org.Hs.egALIAS2EG
org.Hs.egALIAS2EG
unlist(mget(x=gene,envir=org.Hs.egALIAS2EG))

###################
## Load data
acc.cnv<- read.delim("/Users/Matt/Documents/Masters_Bioinformatics/Internships/Data/unzipped data/ACC/all_data_by_genes.txt", stringsAsFactors = FALSE, header = TRUE)
#acc.cnv<- tbl_df(acc.ncv)
acc.cnv
dim(acc.cnv)

##################
##convert gene name to entrez ID

columns(org.Hs.eg.db)

gene.name<- acc.ncv$Gene.Symbol
gene.name[198:240]

##Remove ensembl ENSG name from some gene name entries
gene.name<- gsub("\\|.*","",gene.name)
gene.name[198:240]

##Get new entrez gene IDs from the gene names
new.entrez.id<- select(org.Hs.eg.db, keys = gene.name, columns = c("ENTREZID"), keytype = "SYMBOL")
new.entrez.id
nrow(new.entrez.id)

#############
##Convert original entrez ID to chromosomal location

##
acc.locus.id<- acc.ncv %>% dplyr::select(Locus.ID)
dim(acc.locus.id)

ls("package:org.Hs.eg.db")

columns(org.Hs.eg.db)
help(MAP)
x <- org.Hs.egCHRLOC
head(x)
columns(x)
x
str(x)

k<- acc.locus.id[100:115,]
k<- as.character(k)
k
k<- as.character(c(116983, 140625, 375790, 441869, 55210))
class(k)
k
k<- as.character(c(-16, 387509, 390992, 11332, 54626, 83715, 100422975, 57449, 8718, 79707, 80835, 3104, 9903, 148479, 90326, 55735))

#Confirm using biomart:(http://www.ensembl.org/biomart/martview/355d65b92327cb1eac4b6aef37749a19)

select(org.Hs.eg.db, k, columns = c("CHRLOC", "SYMBOL"), keytype = "ENTREZID")
select(org.Hs.eg.db, k, columns = c("TXSTART"), keytype = "ENTREZID")
??genes

## from: RNA-seq Data Analysis: A Practical Approach. By Eija Korpelainen, Jarno Tuimala, Panu Somervuo, Mikael Huss, Garry Wong
#(https://books.google.co.uk/books?id=u5fNBQAAQBAJ&pg=PA229&lpg=PA229&dq=org.Hs.egCHRLOC+columns&source=bl&ots=RK7s4k5kyl&sig=2ma2_Hun96OYiNEXBzZ8ACPLcL4&hl=en&sa=X&ved=0ahUKEwidvKzwpYHUAhWlBcAKHdbFA8gQ6AEIQTAF#v=onepage&q=org.Hs.egCHRLOC%20columns&f=false)

sig<- c("ENSG00000099622", "ENSG00000114737", "ENSG00000105254", "ENSG00000104879", "ENSG00000237289")
         
keys<- keys(org.Hs.eg.db, keytype = "ENSEMBL")
keys
columns<- c("CHR", "CHRLOC", "CHRLOCEND")
sel<- select(org.Hs.eg.db, keys, columns, keytype = "ENSEMBL")
View(sel)
sel2<- sel[sel$ENSEMBL %in% sig,]
sel2

sel3<- na.omit(sel2[!duplicated(sel2$ENSEMBL), ])
sel3

sel3$strand<- ifelse(sel3$CHRLOC <0, "-", "+")
sel3
sel3$start<- abs(sel3$CHRLOC)
sel3$end<- abs(sel3$CHRLOCEND)
sel3

##Better attempt

keys<- keys(org.Hs.eg.db, keytype = "ENTREZID")
keys
columns<- c("CHR", "CHRLOC", "CHRLOCEND")
sel<- select(org.Hs.eg.db, keys, columns, keytype = "ENTREZID")
View(sel)
dim(sel)

acc.locus.id<- acc.cnv$Locus.ID
length(acc.locus.id)

sel2<- sel[sel$ENTREZID %in% acc.locus.id,]
sel2
dim(sel2)

sel3<- na.omit(sel2[!duplicated(sel2$ENTREZID), ])
sel3
dim(sel3)

sel3$strand<- ifelse(sel3$CHRLOC <0, "-", "+")
sel3
sel3$start<- abs(sel3$CHRLOC)
sel3$end<- abs(sel3$CHRLOCEND)
head(sel3)
dim(sel3)

###########
##convert new entrez ID to chromosomal location

keys<- keys(org.Hs.eg.db, keytype = "ENTREZID")
keys
columns<- c("CHR", "CHRLOC", "CHRLOCEND")
sel<- select(org.Hs.eg.db, keys, columns, keytype = "ENTREZID")
View(sel)
dim(sel)

new.acc.locus.id<- new.entrez.id$ENTREZID
length(new.acc.locus.id)

new.sel2<- sel[sel$ENTREZID %in% new.acc.locus.id,]
new.sel2
dim(new.sel2)

new.sel3<- na.omit(sel2[!duplicated(sel2$ENTREZID), ])
new.sel3
dim(new.sel3)
dim(sel3)

new.sel3$strand<- ifelse(new.sel3$CHRLOC <0, "-", "+")
new.sel3
new.sel3$start<- abs(new.sel3$CHRLOC)
new.sel3$end<- abs(new.sel3$CHRLOCEND)
head(new.sel3)
dim(new.sel3)









############
## Use Biomart to get all human genes (https://github.com/stephenturner/annotables)

library(biomaRt)
library(dplyr)

fix_genes <- . %>% 
  tbl_df %>% 
  distinct %>% 
  rename(ensgene=ensembl_gene_id,
         entrez=entrezgene,
         symbol=external_gene_name,
         chr=chromosome_name,
         start=start_position,
         end=end_position,
         biotype=gene_biotype)

myattributes <- c("ensembl_gene_id",
                  "entrezgene",
                  "external_gene_name",
                  "chromosome_name",
                  "start_position",
                  "end_position",
                  "strand",
                  "gene_biotype",
                  "description")

# Human
grch38 <- useMart("ensembl") %>% 
  useDataset(mart=., dataset="hsapiens_gene_ensembl") %>% 
  getBM(mart=., attributes=myattributes) #%>% 
  #fix_genes
View(grch38)



#############
## Intro to annotation packages (https://www.bioconductor.org/packages/devel/bioc/vignettes/AnnotationDbi/inst/doc/IntroToAnnotationPackages.pdf)

##ChipDb objects and select method

biocLite("hgu95av2.db")
library(hgu95av2.db)

ls("package:hgu95av2.db")

hgu95av2.db

columns(hgu95av2.db)

help("SYMBOL")

keytypes(hgu95av2.db)

head(keys(hgu95av2.db, keytype="SYMBOL"))

k<- head(keys(hgu95av2.db, keytype = "PROBEID"))
k

select(hgu95av2.db, keys = k, columns = c("SYMBOL", "GENENAME"), keytype = "PROBEID")

mapIds(hgu95av2.db, keys = k, column = c("GENENAME"), keytype = "PROBEID")

## OrgDb and select method

library(org.Hs.eg.db)

columns(org.Hs.eg.db)

keytypes(org.Hs.eg.db)

?keys
k<- head(keys(org.Hs.eg.db, "UNIPROT"), 20)
k

help("ENZYME")

?select

select(org.Hs.eg.db, k, columns = c("SYMBOL", "PATH"), keytype = "UNIPROT")

##Use select to annotate results

load(system.file("extdata", "resultTable.Rda", package="AnnotationDbi"))
head(resultTable)

columns(org.Hs.eg.db)
keytypes(org.Hs.eg.db)

dim(resultTable)

annots <- select(org.Hs.eg.db, keys=rownames(resultTable),
                 columns=c("SYMBOL","GENENAME"), keytype="ENTREZID")

annots

resultTable <- merge(resultTable, annots, by.x=0, by.y="ENTREZID")
head(resultTable)
dim(resultTable)

##############
## Using select with GO.db



