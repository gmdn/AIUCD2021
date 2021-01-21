library(xml2)

#write_xml(my_child, "~/Downloads/my_child.txt")

create_tbx <- function(term) {
  
  tbx <- xml_new_root("tbx",
                      "xmlns:tbx3" = "urn:iso:std:iso:30042:ed-2",
                      "type" = "TBX-TriMED",
                      "style" = "dct",
                      "xml:lang" = "en",
                      "xmlns:min" = "http://www.tbxinfo.net/ns/min",
                      "xmlns:basic" = "http://www.tbxinfo.net/ns/basic", 
                      "xmlns:trimed" = "http://www.trimed.org/ns/sample") %>%
    xml_root()
  
  #write_xml(tbx, "~/Downloads/tbx.txt")
  
  header_desc <- "An instance of the EUGloss termbase consisting of one concept entry. The terminological record is based on the TriMED termbase."
  concept_id <- term$codes[1]
  subject <- "Medicine"
  
  # add header
  xml_add_child(tbx, .value = "tbxHeader") %>%
    xml_add_child("fileDesc") %>%
    xml_add_child("sourceDesc") %>%
    xml_add_child("p", header_desc) %>%
    xml_root()
  
  # add text
  text <- xml_add_child(tbx, .value = "text")
  
  # add body
  body <- xml_add_child(text, .value = "body")
  
  # add concept
  concept_entry <- xml_add_child(body, 
                                 .value = "conceptEntry", "id" = concept_id)
  
  # add transaction group
  xml_add_child(concept_entry, .value = "transacGrp") %>%
    xml_add_child(.value = "basic:transactionType", "origination") %>%
    xml_add_sibling(.value = "date", Sys.Date()) %>%
    xml_add_sibling(.value = "basic:responsibility", "TriMED 2019")
  
  # add subject fields
  xml_add_child(concept_entry, 
                .value = "min:subjectField", subject) %>%
    xml_add_sibling(.value = "note", NULL) %>%
    xml_add_sibling(.value = "trimed:superordinateConcept", NULL) %>%
    xml_add_sibling(.value = "trimed:subrdinateConcept", NULL)
  
  
  # for each language
  langs <- term$language #c("en", "fr", "it")
  
  subdomain <- "subdomain"
  definition <- "definition"
  def_source <- "source definition"
  semic <- "semic analysis"
  pos <- "pos"
  gender <- "gender"
  crossref <- "cross reference"
  number <- "number"
  derivative <- "derivative"
  number <- "number"
  pronounce <- "pronounce"
  etymology <- "etymology"
  variant <- "variant"
  abbreviation <- "abbrev"
  full_form <- "full"
  acronym <- "acronym"
  vernacular <- "vernacular"
  register <- "register"
  hyponym <- "hypo"
  hypernym <- "hyper"
  phraseology <- "phrase"
  collocation <- "collo"
  colligation <- "colli"
  context <- "context"
  con_source <- "source"
  
  for (lang in langs) { #lang <- langs[3]
    
    term_ <- term$technical[term$language == lang]
    common <- term$popular[term$language == lang]
    scientific <- term$technical[term$language == lang]
    
    # add language 
    language_entry <- xml_add_child(concept_entry,
                                    .value = "langSec", "xml:lang" = lang)
    
    # add description group
    xml_add_child(language_entry, .value = "descGroup") %>%
      xml_add_child(.value = "basic:definition", definition) %>%
      xml_add_sibling(.value = "basic:source", def_source) %>%
      xml_add_sibling(.value = "semicAnalysis", semic)
    
    # add term section
    term_entry <- xml_add_child(language_entry, .value = "termSec") %>%
      xml_add_child(.value = "term", term_) %>%
      xml_add_sibling(.value = "min:partOfSpeech", pos) %>%
      xml_add_sibling(.value = "basic:grammaticalGender", gender) %>%
      xml_add_sibling(.value = "basic:crossreference", crossref) %>%
      xml_add_sibling(.value = "trimed:grammaticalNumber", number) %>%
      xml_add_sibling(.value = "trimed:derivative", derivative) %>%
      xml_add_sibling(.value = "trimed:grammaticalNumber", number) %>%
      xml_add_sibling(.value = "trimed:pronounciation", pronounce) %>%
      xml_add_sibling(.value = "trimed:etymology", etymology) %>%
      xml_add_sibling(.value = "trimed:variant", variant)  %>%
      xml_add_sibling(.value = "trimed:abbreviation", abbreviation) %>%
      xml_add_sibling(.value = "trimed:fullForm", full_form) %>%
      xml_add_sibling(.value = "trimed:acronym", acronym) %>%
      xml_add_sibling(.value = "trimed:synonymVernacular", vernacular) %>%
      xml_add_sibling(.value = "trimed:register", register) %>%
      xml_add_sibling(.value = "trimed:commonName", common) %>%
      xml_add_sibling(.value = "trimed:scientificName", scientific) %>%
      xml_add_sibling(.value = "trimed:hyponym", hyponym) %>%
      xml_add_sibling(.value = "trimed:hypernym", hypernym) %>%
      xml_add_sibling(.value = "trimed:phraseologicalUnit", phraseology) %>%
      xml_add_sibling(.value = "trimed:collocation", collocation) %>%
      xml_add_sibling(.value = "trimed:colligation", colligation) %>%
      xml_parent()
    
    # add (term) description group
    xml_add_child(term_entry, .value = "descGroup") %>%
      xml_add_child(.value = "basic:context", context) %>%
      xml_add_sibling(.value = "basic:source", con_source) %>%
      xml_add_sibling(.value = "trimed:subDomain", subdomain)
    
  }
  
  
  #write_xml(tbx, "~/Downloads/tbx.txt")
  return(tbx)
  
}