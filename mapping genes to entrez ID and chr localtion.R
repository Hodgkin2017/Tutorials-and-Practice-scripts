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

##Remove ENSG name from some gene name entries
gene.name<- gsub("\\|.*","",gene.name)
gene.name[198:240]

##Get new entrez gene IDs from the gene names
new.entrez.id<- select(org.Hs.eg.db, keys = gene.name, columns = c("ENTREZID"), keytype = "SYMBOL")
new.entrez.id
nrow(new.entrez.id)

#############
##Convert original entrez ID to chromosomal location

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


## Having problems using select

##Using Bimap...not sure what to do next?

## Bimap interface:
x <- org.Hs.egCHRLOC
# Get the entrez gene identifiers that are mapped to chromosome locations
mapped_genes <- mappedkeys(x)
mapped_genes
# Convert to a list
xx <- as.list(x[mapped_genes])
xx
if(length(xx) > 0) {
  # Get the CHRLOC for the first five genes
  xx[1:5]
  # Get the first one
  xx[[1]]
}


###########
##convert new entrez ID to chromosaomal location



#.......









############
## Use Biomart to get all human genes

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



