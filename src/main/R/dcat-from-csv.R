library(xml2)
library(tidyr)
library(dplyr)
library(jsonlite)
library(data.table)
library(stringr)

setwd('/home/gehau/git/algemeen-catalog-publiek/src/main/R')
artifactory <- "https://repo.omgeving.vlaanderen.be/artifactory/release"



##### FUNCTIES

# functie om dataframe om te zetten naar jsonld
to_jsonld <- function(dataframe) {
  # lees context
  context <- jsonlite::read_json("../resources/source/catalog_context.json")
  # jsonld constructie
  df_in_list <- list('@graph' = dataframe, '@context' = context)
  df_in_json <- toJSON(df_in_list, auto_unbox=TRUE)
  return(df_in_json)
}

### functie om de volgende release versie juist te zetten
prompt_versie <- function(version_next_release) {
  cat(paste("Is de volgende release versie : ",version_next_release,'?\n'))
  cat(" (y/n)  : ")
  yes_or_no <- readLines("stdin",n=1);
  if ( yes_or_no == 'y') {
    return(version_next_release)
  } else if ( yes_or_no == 'n') {
    cat(" Wat is de volgende release versie?  : ")
    version_next_release <- readLines("stdin",n=1)
    prompt_versie(version_next_release)
  } else {
    cat("enter y or n\n")
    prompt_versie(version_next_release)
  }
}

expand_df_on_pipe <- function(df) {
  # verdubbel rijen met pipe separator
  for(col in colnames(df)) {   # for-loop over columns
    df <- df %>%
      separate_rows(col, sep = "\\|")%>%
      distinct()
  }
  return(df)
}


collapse_df_on_pipe <- function(df) {
  # group by
  df3 <- df %>% select(id) %>% distinct()
  for(col in colnames(df)) {   # for-loop over columns
    if ( col != 'id') {
      df4 <- df %>% select(id, col)
      names(df4)[2] <- 'naam' # hack, geef de tweede kolom een vaste naam, summarize werkt niet met variabele namen
      df2 <- df4 %>% group_by(id) %>%
        summarize(naam = paste(sort(unique(naam)),collapse="|"))
      names(df2)[2] <- col # wijzig kolom met naam 'naam' terug naar variabele naam
      df3 <- merge(df3, df2, by = "id")
    }
  }
  return(df3)
}



add_package_as_distribution <- function(df) {
  #ds <- df[df$type == 'spdx:Package', ]['id']
  ds <- subset(df, type == 'spdx:Package')$id
  setDT(df)[type == "dcat:Dataset", distribution := paste(distribution,ds, sep = "|")]
  setDT(df)[type == "spdx:Package", type := paste(type,'dcat:Distribution', sep = "|")]

  return(df)
}

rename_columns <- function(df) {
  # rename columns
  df <- df %>%
    rename(  "@id" = id,
             "@type" = type)

  return(df)
}

# read pom.xml 
x <- read_xml("../../../pom.xml")
xml_ns_strip( x )
my_groupId <- xml_text( xml_find_first(x, "/project/groupId") )
my_artifactId <- xml_text( xml_find_first(x, "/project/artifactId") )
my_version <- xml_text( xml_find_first(x, "/project/version") )
my_packageName_ <- paste(my_groupId, my_artifactId, sep = ".")


class_path  <- gsub("\\.","/", my_groupId)
version_next_release <- sub("-SNAPSHOT", "", my_version)
#version_next_release <- prompt_versie(version_next_release)
my_package_id <- paste("omg_package", my_packageName_, sep = ":")
my_package_id_versioned <- paste(my_package_id, version_next_release, sep = ".")
packageFileName_ <- paste(my_artifactId,'-',version_next_release,'.jar', sep = "")
downloadLocation_ <- paste(artifactory, class_path, my_artifactId, version_next_release, packageFileName_, sep = "/")


dependencies <- xml_find_all(x, "/project/dependencies/dependency")

df <- read.csv(file = "../resources/source/catalog_source.csv", sep=",", na.strings=c("","NA"))
df2 <- data.frame(my_package_id_versioned, 'spdx:Package', my_packageName_, paste("Package", my_artifactId, sep = " "), downloadLocation_, packageFileName_, version_next_release)
names(df2) <- c("id","type", "packageName", "label", "downloadLocation", "packageFileName", "versionInfo")
df <- bind_rows(df, df2)
setDT(df)[source == my_package_id, source := my_package_id_versioned]

for (dependency in dependencies){
  groupId <- xml_text( xml_find_first(dependency, "groupId") )
  artifactId <- xml_text( xml_find_first(dependency, "artifactId") )
  version <- xml_text( xml_find_first(dependency, "version") )
  class_path  <- gsub("\\.","/", groupId) 
  packageFileName_ <- paste(artifactId,'-',version,'.jar', sep = "")
  packageName_ <- paste(groupId, artifactId, sep = ".")
  package_id <- paste("omg_package", packageName_, sep = ":")
  package_id_versioned <- paste(package_id, version, sep = ".")
  downloadLocation_ <- paste(artifactory, class_path, artifactId, version, packageFileName_, sep = "/")
  omg_named_graph_ <- paste("omg_named_graph", artifactId, sep = ":")
  df2 <- data.frame(package_id_versioned, 'spdx:Package', downloadLocation_, packageFileName_, packageName_, version, my_package_id_versioned, paste("Package", artifactId, sep = " "))
  names(df2) <- c("id","type", "downloadLocation", "packageFileName", "packageName",  "versionInfo", "relationshipType_packageOf", "label")
  df <- bind_rows(df, df2)
  setDT(df)[source == package_id, source := package_id_versioned]


}

write.csv(collapse_df_on_pipe(df),"../resources/be/vlaanderen/omgeving/data/id/catalog/algemeen-catalog-publiek/catalog.csv", row.names = FALSE)


df <- df %>%
  mutate_all(list(~ str_c("", .)))


df <-   expand_df_on_pipe(df)%>%
  rename_columns()


df_in_json <- to_jsonld(df)

tmp_file <- tempfile(fileext = ".jsonld")

write(df_in_json, tmp_file)
system(paste("riot --formatted=TURTLE ", tmp_file, "  > ../resources/be/vlaanderen/omgeving/data/id/catalog/algemeen-catalog-publiek/catalog.ttl"))
system("riot --formatted=JSONLD ../resources/be/vlaanderen/omgeving/data/id/catalog/algemeen-catalog-publiek/catalog.ttl > ../resources/be/vlaanderen/omgeving/data/id/catalog/algemeen-catalog-publiek/catalog.jsonld")

