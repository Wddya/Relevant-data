# library(tidyverse)
library(GSVA)
library(clusterProfiler)
library(org.Hs.eg.db)
library(data.table)
library(rtracklayer)

### ssGSEA ######
## table S1 - https://doi.org/10.1016/j.immuni.2013.10.003 
## pdf -> table -> read
immunity <- read.csv("~/immunity-cell-gene.csv", header = T)
#   CellType AffymetrixID Symbol Gene.Symbol ENTREZ_GENE_ID
# 1      aDC    205569_at  LAMP3       LAMP3          27074
# 2      aDC    207533_at   CCL1        CCL1           6346
# 3      aDC    210029_at   INDO        IDO1           3620
# 4      aDC    218400_at   OAS3        OAS3           4940
# 5      aDC    219424_at   EBI3        EBI3          10148
# 6  B cells    204836_at   GLDC        GLDC           2731

idx <- !immunity$CellType %in% c("Blood vessels", "Normal mucosa", "SW480 cancer cells", "Lymph vessels")
immunity <- immunity[idx,]
immunity <- immunity %>%
  split(., .$CellType) %>%
  lapply(., function(x)(x$ENTREZ_GENE_ID))
immunity <- lapply(immunity, unique)

## Ensembl download
anno <- import('~/Homo_sapiens.GRCh38.101.gtf')
anno <- as.data.frame(anno)
anno <- anno[!duplicated(anno$gene_id),]

anno <- merge(anno, gene_symbol, by = "gene_name")
anno <- rbind(anno, data.frame(gene_name = c("KIAA1324", "IGHA1"), 
                               gene_id = c("ENSG00000116299", "ENSG00000211895"),
                               ENTREZID = c("57535", "3492")))
anno <- anno[!duplicated(anno$gene_id),] ### 37417
anno <- anno[, c("gene_id", "ENTREZID")]


data <- fread("~/tpm.txt") %>% 
  rename("gene_id" = "V1") %>% 
  left_join(., anno, by = "gene_id") %>% 
  filter(!is.na(ENTREZID)) %>% 
  select(-gene_id) %>% 
  column_to_rownames("ENTREZID")
data <- log2(data + 1) 

immu_cell <-  as.data.frame(gsva(as.matrix(data), immunity, method = "ssgsea"))


### cor plot 
data <- read.table("~/file.txt", header = T)
#        group       aDC
# 1 1.13092315 0.4709550
# 3 0.55644003 0.1800251
# 4 0.44696904 0.3350859
# 5 0.05474605 0.1191767
# 7 0.61364297 0.1563856
# 8 0.41079217 0.4588979
colnames(data) <- c("x", "y")

ggplot(data, aes(x = x, y = y)) + 
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  theme_bw()