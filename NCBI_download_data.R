####################################################################
# Eduardo Batista                                                  #
# Project MDR                                                      #
# PhD Program in Biology and Ecology of Global Change BEGC         #
# 06-01-2019                                                       #
####################################################################

#Libraries
library(plyr)
library(xlsx)
library(openxlsx)
library(dplyr)
library(XML)
library(rentrez)
library(xml2)
library(textutils)


# API KEY
api_key = "enter_your_api_key"
set_entrez_key("enter_your_api_key")


# Extract data from NCBI info for all BOT species

BOT1 <- read.xlsx('12-05-2020_all_bot_updated.xlsx', sheet = 1, startRow = 1, colNames = FALSE)
colnames(BOT1) <- c("ids")

#Depending on your connection you might want to split BOT1 in several blocks

# Null function 
null.to.other <-
  function(x,y=NA){
    if(is.null(x)){
      return(y)
    }
    else{
      return(x)
    }
  }

# Extract data from NCBI info for all BOT species
#BOT1
test1 = list()
for (i in 1:nrow(BOT1)) {
  fetch2 <- entrez_fetch(db = "nucleotide", id = BOT1$ids[i], 
                         rettype = "gbc", retmode="xml", parsed = TRUE, api_key = "682b572f36aa836509b75761d46ea690af08")  
  xmltop <- xmlRoot(fetch2)
  numseq <- length(xmltop)
  organism <- rep(NA,numseq)
  accession <- rep("",numseq)
  mol_type <- rep(NA,numseq)
  strain <- rep(NA,numseq)
  culture_collection <- rep(NA,numseq)
  host <- rep(NA,numseq) 
  isolation_source <- rep(NA,numseq)
  lat_lon <- rep(NA,numseq)
  country <- rep(NA,numseq)
  collectiondate <- rep(NA,numseq)
  host <- rep(NA,numseq)
  title <- rep(NA,numseq)
  authors <- rep(NA,numseq)
  journal <- rep(NA,numseq)
  pubmed <- rep(NA,numseq)
  definition <- rep(NA,numseq)
  doi <- rep(NA,numseq)
  organism <- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='organism']/INSDQualifier_value"),xmlValue))[1],NA)
  accession <-  unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_primary-accession"),xmlValue))[1]
  mol_type <- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='mol_type']/INSDQualifier_value"),xmlValue))[1],NA)
  strain <- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='strain']/INSDQualifier_value"),xmlValue))[1],NA)
  culture_collection[i] <- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='culture_collection']/INSDQualifier_value"),xmlValue))[1],NA)
  host <- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='host']/INSDQualifier_value"),xmlValue))[1],NA)
  isolation_source <- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='isolation_source']/INSDQualifier_value"),xmlValue))[1],NA)
  lat_lon <- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='lat_lon']/INSDQualifier_value"),xmlValue))[1],NA)
  country <- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='country']/INSDQualifier_value"),xmlValue))[1],NA)
  collectiondate <- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='collection_date']/INSDQualifier_value"),xmlValue))[1],NA)
  definition <-  unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_definition"),xmlValue))[1]
  title <- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_references/INSDReference/INSDReference_title"),xmlValue))[1],NA)
  authors <- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_references/INSDReference/INSDReference_authors"),xmlValue))[1],NA)
  journal <- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_references/INSDReference/INSDReference_journal"),xmlValue))[1],NA)
  pubmed<- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_references/INSDReference/INSDReference_pubmed"),xmlValue))[1],NA)
  doi <- null.to.other(unlist(lapply(getNodeSet(xmltop,"//INSDSeq/INSDSeq_references/INSDReference/INSDReference_xref/INSDXref[INSDXref_dbname='doi']/INSDXref_id"),xmlValue))[1],NA)
  test1[[i]] <- data.frame(organism,accession,mol_type,strain,culture_collection,host,isolation_source,lat_lon,country,collectiondate,definition,title,authors,journal,pubmed,doi)
  BOT1_data = do.call(rbind, test1)
}
Unique_BOT1 <- unique(BOT1_data) # remove duplicates

# Save data
write.xlsx(Unique_BOT1, "12-05-2020-Unique_BOT1.xlsx")
write.xlsx(BOT1_data, "12-05-2020-BOT1_data.xlsx")

#After this step is necessary to clean the data manually 

