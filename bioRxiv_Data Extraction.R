
# Key ---------------------------------------------------------------------

# REF.ARTICLES  - index of all preprints on bioRxiv
# REF.DETAILS   - all metrics held by bioRxiv about each preprint
# REF.INFO      - all information about prepring versions on bioRxiv
# REF.PUB       - all preprints that have been published
# REF.ALTMETRIC - data held by Altmetric on each preprint
# REF.POSTPUB   - data held by Altmetric on the published version of a preprint


# Scrape search -------------------------------------------------------------

# Load packages
library(XML)
library(rvest)
library(pbapply)
library(magrittr)

# URL terms
search.root   <- "http://biorxiv.org/search/"
search.terms  <- ""
search.size   <- "numresults%3A500%20"  # 7760 stands for the total results
search.sort   <- "sort%3Apublication-date%20direction%3Aascending%20"
search.format <- "format_result%3Astandard"

# Much quicker to Parse as many results as possible each time
# 100, 100, 100, 100 vs 400 was 42 vs 16 seconds
# 500 seems to be the maximum results per page accepted by bioRxiv

# Use for specific dates
# search.terms <- "limit_from%3A2016-01-01%20limit_to%3A2016-02-29%20"

# Build initial URL
search.URL <- paste(search.root, search.terms, 
                    search.size, search.sort, 
                    search.format, sep = "")

# Identify number of last page
html.website.scout <- read_html(search.URL)
last.page <- html.website.scout %>% html_nodes('.pager-last a') %>% html_text()
last.page <- as.numeric(last.page)

# Build all URLs
search.URL.list <- list()
# Ascending order of results
for (i in 1:last.page){
    search.page <- paste("?page=", i - 1, sep = "")
    search.URL.page <- paste(search.URL, search.page, sep = "")
    search.URL.list[[i]] <- search.URL.page
    
    # bioRxiv limits itself to only creating up to 4000 results
    # I have sorted ascending and then descending to circumvent that
    URL.descend <- gsub("ascending", "descending", search.URL.page)
    search.URL.list[[i + last.page]] <- URL.descend
}

# Parse results website
html.website <- pblapply(search.URL.list, htmlParse)

# Create list
html.list <- pblapply(html.website, readHTMLList)

# Access articles within list
html.articles <- list()
for (i in 1:length(html.website)){
    html.articles[[i]] <- html.list[[i]][[3]]
}

# View list to make sure everything got extracted appropriately
# str(html.articles)

# Extract titles
extractTitle <- function(x) {gsub("(^.*?)\n.*","\\1", x)}
trimTrailing <- function(x) {sub("\\s+$", "", x)}
article.titles <- pblapply(html.articles, extractTitle)
article.titles <- pblapply(article.titles, trimTrailing)
article.titles <- unlist(article.titles)

# Extract authors
extractAuthor <- function(x) {gsub("^.*?\\s+\n\\s+\n\\s+(.*?)\n.*","\\1", x)}
article.authors <- pblapply(html.articles, extractAuthor)
article.authors <- unlist(article.authors)

# Extract DOI
extractDoi <- function(x) {gsub("^.*https://doi.org/(.*?)\\s+\n.*", "\\1", x)}
article.dois <- pblapply(html.articles, extractDoi)
article.dois <- unlist(article.dois)

# Build URLs
xpath.URL <- "//a[@class=\"highwire-cite-linked-title\"]/@href" # This query says, "select the "href" attribute (@) of an "a" tag that appears anywhere (//), but only where (the bracketed phrase) the class contents of the "a" tag is equal to the one wanted
article.URL <- pblapply(html.website, function(x){xpathSApply(x, xpath.URL)})
article.URL <- pblapply(article.URL, as.vector)
article.URL <- unlist(article.URL)
article.URL <- paste("http://biorxiv.org", article.URL, sep = "")

# Build data frame
article.all  <- c(article.titles, article.authors, article.dois, article.URL) %>% 
                unlist()
REF.ARTICLES <- data.frame(matrix(article.all, ncol = 4), stringsAsFactors=FALSE)
colnames(REF.ARTICLES) <- c("Title", "Authors", "DOI", "URL")

# Remove duplicates
REF.ARTICLES <- REF.ARTICLES[!duplicated(REF.ARTICLES), ]

# Alternative (find links using package: rvest)
# html.website <- read_html(search.URL)
# html.website %>% html_nodes("a") %>% html_attr("href")



# Batch download ----------------------------------------------------------

# Load packages
library(XML)
library(rvest)
library(RSelenium)
library(RCurl)
library(pbapply)

# Declare variables
html.article <- c()
html.info <- c()
html.metrics <- c()

# Parse in stages
for (i in 1:77) {
    
    # Offset
    lo.off <- 1 + (100 * (i - 1))
    hi.off <- lo.off + 99
    
    # Scrape details
    html.article.t <- pblapply(REF.ARTICLES$URL[lo.off:hi.off], read_html)
    html.article <- c(html.article, html.article.t)
    
    # Scrape info
    URL.info <- paste(REF.ARTICLES$URL, ".article-info", sep = "")
    html.info.t <- pblapply(URL.info[lo.off:hi.off], read_html)
    html.info <- c(html.info, html.info.t)
    
    # Scrape metrics
    URL.usage <- paste(REF.ARTICLES$URL, ".article-metrics#details", sep = "")
    html.metrics.t <- pblapply(URL.usage[lo.off:hi.off], htmlParse)
    html.metrics <- c(html.metrics, html.metrics.t)

    # Scrape Altmetric
    # altmetric.API <- "https://api.altmetric.com/v1/doi/"
    # URL.altmetric <- paste(altmetric.API, REF.ARTICLES$DOI, sep = "")
    # altmetric.json <- pblapply(URL.altmetric, getURL)
}

# Initiate Docker and establish connection to RSelenium server in Terminal
# Instantiate remote driver to connect to Selenium Server
remDr <- remoteDriver(browserName = "firefox", port = 4445) 
# Open web browser
remDr$open(silent = T) 

# Declare variables
html.pub <- list()

## Scrape publication
for (i in 1:77) {
    # Offset
    lo.off <- 1 + (100 * (i - 1))
    hi.off <- lo.off + 99

    # Extract source
    pbar <- txtProgressBar(min = 0, max = 100, style = 3)
    kpval <- 0
    for (j in lo.off:hi.off){
        # Navigate to URL
        remDr$navigate(REF.ARTICLES$URL[j])
        # Give R 5 seconds to run the Javascripts (essential step!)
        Sys.sleep(5)
        # Extract source
        html.pub[[j]] <- unlist(remDr$getPageSource())
        # Parse source
        html.pub[[j]] <- read_html(html.pub[[j]])
    }
    kpval  <-  kpval+1
    setTxtProgressBar(pbar, kpval)
}



# Scrape details ----------------------------------------------------------

# Load packages
library(rvest)

# Parse website
html.article <- pblapply(REF.ARTICLES$URL, read_html)

# Extract article type
css.type <- ".biorxiv-article-type"
articleType <- function(x){x %>% html_node(css.type) %>% html_text()}
article.type <- pblapply(html.article, articleType)
article.type <- gsub("^[\n]\\s+(.*?)\\s+$", "\\1", article.type)

# Extract abstract
css.abstract <- "#p-2"
articleAbstract  <- function(x){x %>% html_node(css.abstract) %>% html_text()}
article.abstract <- pblapply(html.article, articleAbstract)

# Extract collection
css.collect <- ".highlight"
articleCollection <- function(x){x %>% html_node(css.collect) %>% html_text()}
article.collection <- pblapply(html.article, articleCollection)
article.collection <- gsub("[\n]", "", article.collection)

# Alternative (using rvest package)
# part.one <- "//*[contains(concat( \" \", @class, \" \" )"
# part.two <- "concat( \" \", \"highlight\", \" \" ))]"
# xpath.code <- paste(part.one, part.two, sep = ", ")
# html.article %>% html_node(xpath = ) %>% html_text()

# Alternative (using XML package)
# part.one <- "//*[contains(concat( \" \", @class, \" \" )"
# part.two <- "concat( \" \", \"highlight\", \" \" ))]"
# xpath.code <- paste(part.one, part.two, sep = ", ")
# xpathSApply(html.article, xpath.code, xmlValue)

# Namespaces
#XML Namespaces provide a method to avoid element name conflicts. In XML, 
#element names are defined by the developer. This often results in a conflict 
#when trying to mix XML documents from different XML applications.

# Synthesise data
details.all <- unlist(c(article.type, article.abstract, article.collection))
REF.DETAILS <- data.frame(matrix(details.all, ncol = 3), stringsAsFactors = F)
colnames(REF.DETAILS) <- c("Type", "Abstract", "Collection")



# Scrape info -------------------------------------------------------------

# Load packages
library(rvest)
library(pbapply)

# Create URL
URL.info <- paste(REF.ARTICLES$URL, ".article-info", sep = "")

# Parse URL
html.info <- pblapply(URL.info, read_html)

# Date of all previous submissions
css.previous <- ".hw-version-previous-link"
articlePrevious  <- function(x){x %>% html_nodes(css.previous) %>% html_text()}
article.previous <- pblapply(html.info, articlePrevious)
extractPrevious  <- function(x){gsub(".*?\\((.+?)\\s+-.*$", "\\1", x)}
article.previous <- pblapply(article.previous, extractPrevious)
collatePrevious  <- function(x) {paste(x, collapse = "; ")}
article.previous <- unlist(lapply(article.previous, collatePrevious))

# Date of current submission
css.current <- ".published"
articleCurrent <- function(x){x %>% html_node(css.current) %>% html_text()}
article.current <- pblapply(html.info, articleCurrent)
article.current <- gsub("\\s*(.+?)\\.$", "\\1", article.current)

# Date of first submission 
article.first <- paste(article.previous, article.current, sep = "; ")
article.first <- gsub("^;\\s(.*?$)", "\\1", article.first)
article.first <- gsub("^(.*?);.*$", "\\1", article.first)

# All involved institutions
css.inst.all <- "address"
articleInst  <- function(x){x %>% html_nodes(css.inst.all) %>% html_text()}
article.inst   <- pblapply(html.info, articleInst)
articleInstReg <- function(x){gsub("^.+\t[[:digit:]]*\\s(.+?)\n.*?$", "\\1",x)}
article.inst   <- pblapply(article.inst, articleInstReg)
inst.list <- pblapply(article.inst, function(x) {paste(x, collapse = " ")})
inst.list <- unlist(inst.list)

# Affiliation of first and last authors
css.affiliations <- ".xref-aff"
articleAff <- function(x){x %>% html_nodes(css.affiliations) %>% html_text()}
article.aff <- pblapply(html.info, articleAff)

index <- pblapply(article.aff, as.numeric)
end.authors <- c()
for (i in 1:length(index)){
    if (is.na(index[[i]][1])){
        end.authors.temp <- article.inst[[i]][1]
    } else {
        first.aff <- article.inst[[i]][index[[i]][1]]
        last.aff <- article.inst[[i]][index[[i]][length(index[[i]])]]
        end.authors.temp <- unique(c(first.aff, last.aff))
        end.authors.temp <- paste(end.authors.temp, collapse = " ")
    }
    end.authors <- c(end.authors, end.authors.temp)
    end.authors <- gsub(";$", "", end.authors)
}

# Synthesise data
info.all <- unlist(c(article.first, article.previous, article.current, 
                     inst.list, end.authors))
REF.INFO <- data.frame(matrix(info.all, ncol = 5), stringsAsFactors = FALSE)
colnames(REF.INFO) <- c("First_Pub"
                        , "Previous_Pub"
                        , "Current_Pub"
                        , "Institutes_All"
                        , "Institutes_End")




# Scrape metrics ----------------------------------------------------------

# Create URL
URL.usage <- paste(REF.ARTICLES$URL, ".article-metrics#details", sep = "")

# Parse URL and create Table
html.metrics <- pblapply(URL.usage, htmlParse)

# Create table
usage.table <- pblapply(html.metrics, readHTMLTable)

# Declare variables
article.usage <- list()
detailed.pdf  <- c()
detailed.abstract <- c()
sum.pdf <- c()
sum.abstract <- c()
error.index  <- c()

# Extract metrics
pbar  <- txtProgressBar(min = 0, max = length(usage.table), style = 3)
kpval <- 0
for (i in 1:length(usage.table)){
    
    if (is.null(usage.table[[i]]$'NULL')){
        error.index <- c(error.index, i)
        article.usage[[i]] <- usage.table[[i]]
    } else {
        article.usage[[i]] <- usage.table[[i]]$'NULL'
    }
    
    if (length(article.usage[[i]]) == 0){
        error.index <- c(error.index, i)
        detailed.pdf.temp <- NA
        detailed.abstract.temp <- NA
        sum.pdf.temp <- NA
        sum.abstract.temp <- NA
    } else {
        if (dim(article.usage[[i]])[2] < 3 | (length(article.usage[[i]]) == 0)) {
            error.index <- c(error.index, i)
            detailed.pdf.temp <- NA
            detailed.abstract.temp <- NA
            sum.pdf.temp <- NA
            sum.abstract.temp <- NA
        } else {
            detailed.pdf.temp <- paste(article.usage[[i]][, 3], collapse = ", ")
            detailed.abstract.temp <- paste(article.usage[[i]][, 2], collapse = ", ")
            sum.pdf.temp <- sum(as.numeric(as.matrix(article.usage[[i]][, 3])))
            sum.abstract.temp <- sum(as.numeric(as.matrix(article.usage[[i]][, 2])))
        }
    }
    detailed.pdf <- c(detailed.pdf, detailed.pdf.temp)
    detailed.abstract <- c(detailed.abstract, detailed.abstract.temp)
    sum.abstract <- c(sum.abstract, sum.abstract.temp)
    
    sum.pdf <- c(sum.pdf, sum.pdf.temp)
    kpval  <-  kpval+1
    setTxtProgressBar(pbar, kpval)
}

# Correction - works by giving htmlParse more time
html.metrics.correct <- pblapply(URL.usage[unique(error.index)], htmlParse)
usage.table.correct  <- pblapply(html.metrics.correct, readHTMLTable)

# Create index of usage.table.correct
k <- 1
for (i in unique(error.index)){
    
    # Feed correct list into usage list
    article.usage[[i]] <- usage.table.correct[[k]]$'NULL'
    
    detailed.pdf[i] <- paste(article.usage[[i]][, 3], collapse = ", ")
    detailed.abstract[i] <- paste(article.usage[[i]][, 2], collapse = ", ")
    sum.abstract[i] <- sum(as.numeric(as.matrix(article.usage[[i]][, 3])))
    sum.pdf[i] <- sum(as.numeric(as.matrix(article.usage[[i]][, 2])))
    
    # Increase index of correct list
    k <- k + 1
    
    kpval  <-  kpval + 1
    setTxtProgressBar(pbar, kpval)
}

# Synthesise data
metrics.all <- unlist(c(detailed.pdf, sum.pdf, detailed.abstract, sum.abstract))
REF.METRICS <- data.frame(matrix(metrics.all, ncol = 4), stringsAsFactors = F)
colnames(REF.METRICS) <- c("PDF_monthly", "PDF_all", 
                           "Abstract_monthly", "Abstract_all")



# Javascript scrape -------------------------------------------------------

# Load packages
library("RSelenium")
library("rvest")
library("XML")
library("RCurl")
library("httr")

## Initiate Selenium

#1. Open 'Dockers'
#2. Open 'Terminal' and type: docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
#3. Proceed

# Instantiate remote driver to connect to Selenium Server
remDr <- remoteDriver(browserName = "firefox", port = 4445) 

# Open web browser
remDr$open(silent = T) 

# Query the status of the remote Server
# remDr$getStatus()

# Declare variables
html.pub <- list()

# Extract source
pbar  <- txtProgressBar(min = 0, max = 290, style = 3)
kpval <- 0
for (i in 7489:nrow(REF.ARTICLES)){
    # Navigate to URL
    remDr$navigate(REF.ARTICLES$URL[i])
    # Give R 5 seconds to run the Javascripts (essential step!)
    Sys.sleep(5)
    # Extract source
    html.pub[[i]] <- unlist(remDr$getPageSource())
    # Parse source
    html.pub[[i]] <- read_html(html.pub[[i]])
    
    kpval  <-  kpval + 1
    setTxtProgressBar(pbar, kpval)
}

# Extract details
pubDetails  <- function(x){x %>% html_nodes(".pub_jnl") %>% html_text()}
pub.details <- pblapply(html.pub[7488:7760], pubDetails)

# Only maintain the first record from each level of the list
pub.details <- pblapply(pub.details, `[`, 1)

# Extract journal
pub.jou <- gsub("^.+?published\\sin\\s(.+?)\\s+doi.+?$", "\\1", pub.details)
pub.jou <- gsub("^This.+?$", NA, pub.jou)

# Extract doi
pub.doi <- gsub("^.+?doi:\\s(.+?)$", "\\1", pub.details)
pub.doi <- gsub("^This.+?$", NA, pub.doi)

# Build matrix
pub.all <- c(pub.jou, pub.doi)
REF.PUB <- data.frame(matrix(pub.all, ncol = 2), stringsAsFactors=FALSE)
colnames(REF.PUB) <- c("Journal_Pub", "DOI_Pub")


## I could also extract the code of interest to me directly
## (I did not because RSelenium gave me random errors every 5-6 websites)
# pub.journal.all <- c()
# pub.doi.all <- c()
# Find the publication details
# webElem <- remDr$findElement('css', ".pub_jnl")
# 
# # Extract the HTML
# elemTxt <- webElem$getElementAttribute("outerHTML")
# elemTxt <- unlist(elemTxt)
# 
# if (length(grep("doi", elemTxt)) > 0){
#     # Extract journal
#     pub.journal <- gsub("^.+<i>(.+?)</i>.+?$", "\\1", elemTxt)
#     # Extract doi
#     pub.doi <- gsub("^.+?org/(.+?)\\\"\\st.+?$", "\\1", elemTxt)
# } else {
#     pub.journal <- NA
#     pub.doi <- NA
# }
# pub.journal.all <- c(pub.journal.all, pub.journal)
# pub.doi.all <- c(pub.doi.all, pub.doi)



# Altmetric --------------------------------------------------------------

# Load required packages
library(pbapply)
library(tidyjson)
library(jsonlite)
library(RCurl)
library(dplyr)

# Dictionary to Altmetric API
# http://api.altmetric.com/docs/call_citations.html

# Create URL
altmetric.API <- "https://api.altmetric.com/v1/doi/"
URL.altmetric <- paste(altmetric.API, REF.ARTICLES$DOI, sep = "")

# Initialize variables
altmetric.json <- list()

# Download JSON file - use this because of restrictions of Altmetric API
pbar  <- txtProgressBar(min = 0, max = nrow(URL.altmetric), style = 3)
kpval <- 0
for (i in 1:length(URL.altmetric)){
    altmetric.json[i] <- getURL(URL.altmetric[i])
    kpval  <-  kpval+1
    setTxtProgressBar(pbar, kpval)
}

# This did not work, probably due to restrictions by Altemtric
# altmetric.json <- pblapply(URL.altmetric, getURL)

# Alternative (with package: rvest)
#altmetric.json <- read_html("https://api.altmetric.com/v1/doi/10.1101/036707") 
#altmetric.json <- html_text(altmetric.json)

# Declare variables
REF.ALTMETRIC <- data.frame()

pbar  <- txtProgressBar(min = 0, max = nrow(altmetric.json), style = 3)
kpval <- 0
for (i in 1:length(altmetric.json)){
    if (altmetric.json[[i]] == "Not Found"){
        altmetric.data <- data.frame(Not_Found = altmetric.json[[i]])
    } else {
        # Create temporary data frame
        altmetric.data <- fromJSON(altmetric.json[[i]], simplifyDataFrame = TRUE)
        altmetric.data <- unlist(altmetric.data)
        altmetric.data <- as.data.frame(t(altmetric.data), stringsAsFactors = F)
    }
    # Create final data frame
    REF.ALTMETRIC <- bind_rows(REF.ALTMETRIC, altmetric.data)
    kpval  <-  kpval+1
    setTxtProgressBar(pbar, kpval)
}
# Extract Altmetric of published articles
# Contains PubMed ID as a bonus



# Altmetric post-pub ------------------------------------------------------

# Load required packages
library(pbapply)
library(tidyjson)
library(jsonlite)
library(RCurl)
library(dplyr)

# Create URL
doipub        <- na.omit(REF.PUB$DOI_Pub)


# Correct wrong entry in doipub
doipub[1508] <- "10.1007/s10021-016-9987-9"
# Previously: "10.1098/<U+200B>rsif.2013.1095"
doipub[177] <- "10.1098/rsif.2013.1095"


# Create URL
altkey        <- "?key=d8cdb7af57fd685d417482b48b15425a"
altmetric.API <- "https://api.altmetric.com/v1/doi/"
URL.altmetric <- paste(altmetric.API, doipub, sep = "")
# (with the key)
# URL.altmetric <- paste(altmetric.API, doipub, altkey, sep = "")

# Manually correct faulty URLs
URL.altmetric[1508] <- "https://api.altmetric.com/v1/doi/10.1007/s10021-016-9987-9"

# Download JSON file
pbar    <- txtProgressBar(min = 0, max = length(doipub), style = 3)
kpval   <- 0
pub.jsn <- character(length(doipub))
for (i in 1:1014){
  
  pub.jsn[i] <- getURL(URL.altmetric[i])
  kpval      <- kpval+1
  
  setTxtProgressBar(pbar, kpval)
}

# The following did not work, most probably due to restrictions by Altemtric
# pub.jsn <- pblapply(URL.altmetric, getURL)

# Declare variables
REF.POSTPUB <- data.frame()

pbar  <- txtProgressBar(min = 0, max = length(pub.jsn), style = 3)
kpval <- 0
for (i in 1:length(pub.jsn)){
  if (pub.jsn[i] == "Not Found"){
    pub.data <- data.frame(Not_Found = pub.jsn[i])
  } else {
    # Create temporary data frame
    pub.data <- fromJSON(pub.jsn[i], simplifyDataFrame = TRUE)
    pub.data <- unlist(pub.data)
    pub.data <- as.data.frame(t(pub.data), stringsAsFactors = F)
  }
  # Create final data frame
  REF.POSTPUB <- bind_rows(REF.POSTPUB, pub.data)
  
  kpval <- kpval+1
  setTxtProgressBar(pbar, kpval)
}

# Ascertain that missing doi is a subset of missing PMID
ind.na.doi <- grep("TRUE", is.na(REF.POSTPUB$doi))
ind.na.pub <- grep("TRUE", is.na(REF.POSTPUB$pmid))
match.list <- lapply(ind.na.doi, match, ind.na.pub)
any(is.na(match.list))
#[1] FALSE

# So DOI is a subset of missing PMID.

# (empty rows refer to papers that do not have an Altmetric score)
# How many records did we not have an Altmetric entry for?
sum(is.na(REF.POSTPUB$doi))
# 155

# Number of papers not indexed in Altmetric + no. in Altmetric but not in PubMed
sum(is.na(REF.POSTPUB$pmid))
# [1] 214


## Retrieve the PMID of articles not available on Altmetric

# (I am not using apply below because it induces a timeout in curl)

# Initialize object
doi.to.pmid <- numeric(length(doipub[is.na(REF.POSTPUB$doi)]))

# Progress bar
pbar  <- txtProgressBar(min = 0, max = length(doipub[is.na(REF.POSTPUB$doi)]), style = 3)
kpval <- 0

for(i in 1:length(doipub[is.na(REF.POSTPUB$doi)])){
  doi.to.pmid[i] <- doiToPmid(doipub[is.na(REF.POSTPUB$doi)][i])
  
  # Progress bar
  kpval <- kpval + 1
  setTxtProgressBar(pbar, kpval)
}

# Manually indicate "Not on PubMed" for record 96 (1508)
doi.to.pmid[96] <- "Not on PubMed"

# How many papers were not published in PubMed?
length(grep("Not on PubMed", doi.to.pmid))
# [1] 89

# So, out of 2631 published articles, 155 were not listed in Altmetric. Out of those listed in Altmetric, 214 - 155 = 59 were not listed in PubMed. Given that 89 papers out of those not listed in Altmetric were not on PubMed, the total number of papers listed on PubMed is 2631 - 59 - 89 = 2483.

# Add PMID from CrossRef to REF.POSTPUB
REF.POSTPUB$pmid[is.na(REF.POSTPUB$doi)] <- doi.to.pmid


# Add published article DOI from bioRxiv (as an ID column)
REF.POSTPUB$biorxiv_doi <- doipub


### INVESTIGATE WHY THE doipub DOI DOES NOT MATCH THE ALTMETRCI DOI.

# Identify which DOIs in REF.POSTPUB are not in doipub
doi.alt <- setdiff(REF.POSTPUB$doi, doipub)

# Take one of these DOIs: "10.7554/elife.00829"

# Find the title: An unmet actin requirement explains the mitotic inhibition of clathrin-mediated endocytosis

# It turns out it does exist in doipub, but it's: "10.7554/eLife.00829"

# Capitalize all letters and look again for DOIs in REF.POSTPUB but not in doipub
length(setdiff(toupper(REF.POSTPUB$doi), toupper(doipub)))
# [1] 1

# Which ones DOI is on REF.POSTPUB, but not doipub?
setdiff(toupper(REF.POSTPUB$doi), toupper(doipub))
# [1] NA

# So mystery solved

# Scrape PubMed -----------------------------------------------------------

# Load packages
library(reutils)
library(pbapply)
library(RISmed)
library(magrittr)
library(XML) 
library(readxl)

# Define functions

# Function to fetch true DOI
trueDoi <- function(x){
  
  # Only replace for wrong DOIs
  if(length(grep("^S", x$DOI)) > 0){
    
    # Extract DOI for each article and replace
    x$DOI <- efetch(x$PMID, db = "pubmed", retmode = "xml")$content %>%
      xmlParse() %>%
      getNodeSet("//PubmedArticle") %>%
      lapply(xpathSApply, ".//ELocationID[@EIdType = \"doi\"]", xmlValue)
  }
  return(x)
}

doiToPmid <- function(x){
  
  # Extract PMID from CrossRef
  a <- id_converter(x, type = "doi")$records
  
  # Print PMID, if one exists
  if (is.null(a$pmid)){
    "Not on PubMed"
  } else {
    a$pmid
  }
}

# Fetch articles using the RISmed package
# (I tend to get NAs for things that should not have been NAs)
# (this is b/c the XML truly does not contain info, not due to RISmed)
fetchArticles <- function(searchTerms){
  
  # Create the search summary
  res <- EUtilsSummary(searchTerms, 
                       type     = "esearch", 
                       db       = "pubmed", 
                       datetype = 'pdat') # can take more useful options
  
  # Get the results
  fetch <- EUtilsGet(res)
  
  if (length(attributes(fetch)$PMID) > 0){
    
    # Status
    print("Extracting authors")
    
    # Collect authors
    authors <- Author(fetch) %>%
      lapply(`[`, , 1:2) %>%
      lapply(function(x) paste(x[, 1], x[, 2])) %>%
      lapply(paste, collapse = ", ") %>%
      unlist()
    
    ## Collect publication type and funding
    
    # Status
    print("Extracting publication type")
    
    # Fetch data on publication and funding
    pub.term <- PublicationType(fetch) %>% lapply(as.vector)
    
    # Identify research support information
    support <- lapply(pub.term, function(x) grep("Research Support", x))
    
    # Initialize objects
    funder    <- list()
    pub.type  <- list()
    
    # Separate publication type from funding agency
    for (i in 1:length(pub.term)){
      if (length(support[[i]]) > 0) {
        funder[[i]]   <- pub.term[[i]][  support[[i]]]
        pub.type[[i]] <- pub.term[[i]][- support[[i]]] %>% sort()
      } else {
        funder[[i]]   <- ""
        pub.type[[i]] <- pub.term[[i]] %>% sort()
      }
    }
    
    # Store publication type and funder
    pub.type <- lapply(pub.type, paste, collapse = "; ") %>% unlist()
    funder   <- lapply(funder, paste, collapse = "; ") %>% unlist()
    
    
    ## Collect MeSH terms (neater methods did not work)
    
    # Status
    print("Extracting MeSH")
    
    # Initialize objects
    MeSH <- list()
    term <- Mesh(fetch)
    
    # Extract MeSH
    for(i in 1:length(term)){
      if (any(is.na(term[[i]]))){
        MeSH[[i]] <- NA
      } else {
        MeSH[[i]] <- term[[i]][,"Heading"]
      }
    }
    
    # Store MeSH terms
    MeSH <- unlist(lapply(MeSH, paste, collapse = "; "))
    
    ## Build date
    
    # Status
    print("Building date")
    
    # Date of registration on PubMed
    # (I prefer date of publication, but specs vary so much between journals!)
    # (some only report month and year)
    day     <- formatC(DayPubmed(fetch), width = 2, format = "d", flag = "0")
    month   <- formatC(MonthPubmed(fetch), width = 2, format = "d", flag = "0")
    year    <- YearPubmed(fetch)
    numdate <- as.Date(paste(year, month, day, sep = "-"))
    
    # Status
    print("Building dataframe")
    
    # Create frame of papers
    articles <- data.frame(ID       = 1:length(authors),
                           Title    = ArticleTitle(fetch), 
                           Author   = authors, 
                           Year     = year,
                           Date     = numdate,
                           Journal  = ISOAbbreviation(fetch), 
                           ISSN     = ISSN(fetch), # unique journal identifier
                           Pub_type = pub.type, 
                           Funder   = funder,
                           Issue       = Issue(fetch),
                           Volume      = Volume(fetch),
                           Abstract    = AbstractText(fetch), 
                           Affiliation = Affiliation(fetch),
                           Country     = Country(fetch), 
                           Language    = Language(fetch),
                           MeSH        = MeSH,
                           PMID        = PMID(fetch), 
                           DOI         = ELocationID(fetch), # not always DOI!
                           stringsAsFactors = F
    )
    
    # Add search date
    articles$Search_date <- date()
    
    # Correct the DOIs
    articles <- trueDoi(articles)
    
    # Print status
    print("Retrieving missing DOIs")
    
    # Retrieve DOIs that my function failed to retrieve
    ind <- articles$DOI %>%
      lapply(function(k) grep("/", k)) %>%
      lapply(length) %>%
      is_in(0)
    
    # Retrieve DOIs
    if (sum(ind) > 0){ # do this b/c I may not have missing DOIs
      
      # Retrieve DOIs
      dois <- articles$PMID[ind] %>%
        pblapply(function(k) id_converter(k, type = "pmid")$records$doi)
      
      # Replace missing DOIs
      articles$DOI[ind] <- dois
    }
  } else {
    print("Search identified no records")
  }
  # Return object
  return(articles)
}

# Code taken from: https://github.com/christopherBelter/pubmedXML

# Identify PMIDs of published preprints available on PubMed
# (disregard warning message)
pmid <- na.omit(as.numeric(REF.POSTPUB$pmid))

# I NEED TO CHANGE HOW I FETCH ARTICLES BECAUSE I AM LOSING 334 AT THE MOMENT
# BECAUSE I DO NOT HAVE THEIR MONTH OF PUBLICATION. I PRESUME THIS IS NON-DIFFERENTIAL
# MEASUREMENT ERROR BUT STILL I THIN THAT I COULD CIRCUMVENT THIS PROBLEM BY EXTRACTING
# ARTICLES DIFFERENTLY.

# Initialize
articles <- data.frame()

# Progress bar
pbar  <- txtProgressBar(min = 0, max = ceiling(length(pmid) / 400), style = 3)
kpval <- 0

# Fetch using PMID
for (i in 1:ceiling(length(pmid) / 400)){
  
  # Construct search terms for this sample
  lower.bound <- 1 + 400 * (i - 1)
  upper.bound <- min(400 * i, length(pmid))
  searchTerms <- paste(REF.POSTPUB[lower.bound:upper.bound], collapse = " [uid] OR ") %>% 
                 paste0(" [uid]")
  
  # Fetch articles
  articles <- rbind(articles, fetchArticles(searchTerms))
  
  # Progress bar
  kpval <- kpval + 1
  setTxtProgressBar(pbar, kpval)
}

# Number of articles identified
dim(articles)
# [1] 2474   19

# Identify articles in pmid but not in articles
setdiff(pmid, articles$PMID)
# [1] 27651397 27694225 27920425 27530254 27924030 27980112 27742694 27797948

# Identify articles in articles but not in pmid
setdiff(articles$PMID, pmid)
# No falsely identified articles

# I HAVE SEARCHED MANUALLY FOR THE MISSING PMIDs, AND THEY DO NOT EXIST ON PUBMED, WHICH MEANS THAT THEY WERE FALSELY REPORTED BY ALTMETRIC OR THAT THEY HAVE BEEN REMOVED FROM PUBMED. THE PMIDs WERE CORRECTLY EXTRACTED FROM ALTMETRIC.

# This is less than 2483 - which ones do I not have?
searchTerms <- paste(pmid[!(pmid %in% articles$PMID)], collapse = " [uid] OR ") %>% 
  paste0(" [uid]")

a <- pmid[!(pmid %in% articles$PMID)]
b <- paste(a, collapse = "|")
c <- grep(b, REF.POSTPUB$pmid)
d <- REF.POSTPUB$doi[c]
doiToPmid("10.1523/jneurosci.1265-16.2016")

# [1] 27651397 27694225 27920425 27530254 27924030 27980112 27742694 27797948

# ALL OF THESE BELONG TO DATA FROM ALTMETRIC (using setdiff()). It seems that the PMID is wrong. For example, "10.1093/bioinformatics/btw632" does not have the PMID "27694225". Instead, it has PMID, "28172542". I CAN SOLVE THIS PROBLEM BY SEARCHING BY DOI, BUT THE PROBLEM IS THAT I MIGHT HAVE TO FILTER THESE RESULTS USING PMIDs - TRY IT ANYWAY TO SEE IF I WILL NEED TO. I CONFIRMED THAT CROSSREF IS CORRECT AND ALTMETRIC IS TRULY WRONG IN THESE INSTANCES. I HAVE NOT CHECKED WITHER ALTMETRIC WAS CORRECT IN ALL OTHER INSTANCES - IT IS CORRECT IN ALL OTHER INSTANCES I HAVE CHECKED.

# CREATE NEW SEARCH ONLY WITH DOIS AND USE SYNTAX: 10.1016/j.ejca.2014.08.019[LID] 





# Initialize
articles.doi <- data.frame()

# Progress bar
pbar  <- txtProgressBar(min = 0, max = ceiling(length(doipub) / 200), style = 3)
kpval <- 0

# Fetch using DOI
for (i in 1:ceiling(length(doipub) / 200)){
  
  # Construct search terms for this sample
  lower.bound <- 1 + 200 * (i - 1)
  upper.bound <- min(200 * i, length(doipub))
  searchTerms <- paste(doipub[lower.bound:upper.bound], collapse = " [LID] OR ") %>% 
    paste0(" [LID]")
  
  # Fetch articles
  articles.doi <- rbind(articles.doi, fetchArticles(searchTerms))
  
  # Progress bar
  kpval <- kpval + 1
  setTxtProgressBar(pbar, kpval)
}

# Number of articles identified
dim(articles.doi)
# [1] 2554   19

# Number wrongly identified
length(setdiff(articles.doi$DOI, doipub))
# [1] 8

# Number not identified
length(setdiff(doipub, unlist(articles.doi$DOI)))
# 130

# THE ABOVE LIST IS NOT ACCURATE BECAUSE FOR SOME PAPERS I FOUND THE PAPER FROM DOIPUB, BUT THE DOI IS NULL. THE ONLY WAY I COULD POSSIBLY FIX THE DOI IS MANUALLY I THINK... I COULD POSSIBLY FIX THIS BY USING THEIR PMID TO PAIR THEM WITH THE DOIs EXTRACTED FROM ALTMETRIC.

# How many have no DOI recorded?
sum(articles.doi$DOI == "NULL")
# [1] 44

# Find missing DOIs
null.doi <- articles.doi$PMID[articles.doi$DOI == "NULL"]
find.doi <- lapply(null.doi, function(x) doipub[grep(x, REF.POSTPUB$pmid)])

# I CHECKED 3 OF THE ONES IDENTIFIED AUTOMATICALLY AND THEY WERE CORRECT

# Replace missing DOIs
articles.doi$DOI[articles.doi$DOI == "NULL"] <- lapply(find.doi, function(x) ifelse(length(x) > 0, x, "NULL")) 

# Find manually remaining missing DOIs
remain.pmid <- articles.doi$PMID[articles.doi$DOI == "NULL"]
remain.ind  <- lapply(remain.pmid, function(x) grep(x, articles.doi$PMID))
matrix(c(remain.pmid, remain.ind), ncol = 2)

# Manual
articles.doi$DOI[168]  <- "10.1103/PhysRevLett.112.068101"
articles.doi$DOI[411]  <- "10.1103/PhysRevE.92.022711"
articles.doi$DOI[451]  <- "10.1103/PhysRevE.91.052711"
articles.doi$DOI[473]  <- "10.1103/PhysRevLett.114.168102"
articles.doi$DOI[920]  <- "10.1103/PhysRevE.92.012719"
articles.doi$DOI[1324] <- "10.1109/TCBB.2015.2505278"

# Number not identified
setdiff(doipub, articles.doi$DOI)
length(setdiff(doipub, articles.doi$DOI))
# 86

# Some of the DOIs indicate that it should have been on PubMed. Why is it not? Are the DOIs wrong? Let's find out:
grep("10.7554/eLife.01856.001", doipub)
# [1] 62

# Look for it in POSTPUB
REF.POSTPUB[62,]
# not in postpub

# Look for the original preprint on REF.PUB
grep("10.7554/eLife.01856.001", pub.doi)
# [1] 106
REF.ARTICLES[106, ]
# So the DOI that I extracted was correct, but the DOI on bioRxiv was wrong. Let me use the title of the article to see if I can find it.

# I found it and its DOI is: 10.7554/eLife.01856

# Let's do this for PNAS
grep("10.1073/pnas.151650311", pub.doi)
# [1] 1953
REF.ARTICLES[1953, ]
# Again I correctly extracted the DOI. By searching for the article by title, the correct DOI is: 10.1073/pnas.1516503113

# I'll do this for a final paper
# "10.7554/eLife.01917.001" on biorxiv, but correct is: "10.7554/eLife.01917" 

# Some of them are correct but are not on bioRxiv: 10.1109/ACC.2014.6859452. This was not found on Altmetric either.

# I will make a last attempt to find them using their title. I've done 4 searches and the title seems to be a surprisingly good way to search for them.

# Collect titles
again.missing <- setdiff(doipub, unlist(articles.doi$DOI))
the.titles <- lapply(again.missing, function(x) REF.ARTICLES$Title[grep(x, pub.doi)])

# Initialize
articles.title <- data.frame()

# Progress bar
pbar  <- txtProgressBar(min = 0, max = ceiling(length(the.titles) / 10), style = 3)
kpval <- 0

# Fetch using DOI
for (i in 1:length(the.titles)){
  
  # Construct search terms for this sample
  searchTerms <- gsub("[[:space:]]", "[Title] AND ", the.titles[i])
  searchTerms <- paste0(searchTerms, "[Title]")
  #lower.bound <- 1 + 10 * (i - 1)
  #upper.bound <- min(10 * i, length(the.titles))
  #searchTerms <- paste0("\"", paste(the.titles[lower.bound:upper.bound], collapse = "\" OR \""))
  #searchTerms <- paste0(searchTerms, "\"")
  
  # Fetch articles
  articles.title <- rbind(articles.title, fetchArticles(searchTerms))
  
  # Progress bar
  kpval <- kpval + 1
  setTxtProgressBar(pbar, kpval)
}

# Remove articles of which the title does not match the ones I have exactly


# HOW MANY OF THE ARTICLES ON BIORXIV ARE REVIEW ARTICLES??? I THINK SOME OF THE MAY BE EITHER REVIEWS OR COMMENTS!

# ANNOYINGLY PUBMED DOES NOT ACCEPT CONNECTIVE WORDS, WHICH MEANS I NEED TO REMOVE THEM TO SPECIFY TITLES EXACTLY. IF I DO NOT SPECIFY TITLES EXACTLY, I CAN GET RESULTS WITH HUNDREDS OF RECORDS. EVEN WHEN SPECIFYING THAT A PHRASE IS A TITLE, PUBMED REMOVES THE PRONOUNS FROM THE TITLE WHEN SEARCHING. IT ONLY ACCEPTS 'AND' IN A TITLE, NOT AND, ETC. TO SEARCH IN A TITLE PLACE [Title] NEXT TO EVERY WORD AND USE AND TO CONNECT THE WORDS. PUBMED ALSO DOES NOT RECOGNISE "THE GENE" AS A PHRASE AND WITH "THE GENE"[Title] IT WILL DEMAND THE QUOTATIONS AS WELL.

# I CAN SEARCH GOOGLE WITH THE TITLE - IDENTIFY THE TRUE DOI AND THEN USE THAT DOI TO SEARCH PUBMED. THAT WOULD HAVE ACTUALLY BEEN THE FASTEST.

REF.MISSING <- data.frame(DOI = again.missing, Title = unlist(the.titles))

require(XLConnect)

## Load the workbook of interest and if it does not exist create it
wb <- loadWorkbook("~/Desktop/Projects/Stanford/Research/bioRxiv/Analysis/Missing.xlsx", create = TRUE) 

## Name the sheet of the workbook
createSheet(wb, name = "Missing")

## Print the dataframe 'REF.MISSING' into the workbook
## (use rownames = "Row" only when I want to print the names of each row)
writeWorksheet(wb, REF.MISSING, sheet = "Missing", startRow = 1, startCol = 1, rownames = "Row")

## Save the workbook
saveWorkbook(wb)

# PROCESS: Use bioRxiv-provided DOI. If it does not work, use "doi + Title" in google. Find DOI in the results.

# SOME HAVE THE WRONG DOI ON PUBMED, THIS IS WHY THEY WERE NOT FOUND, E.G.: 10.1186/s13059-014-0524-x NORMALLY BUT ON PUBMED: 10.1186/PREACCEPT-2573980311437212. I CAN SOLVE THIS PROBLEM BY GOING FROM ALTMETRIC TO PMID TO PUBMED

# IT SEEMS THAT THERE IS SELECTIVE BIAS - THE ONES THAT I COULD NOT FIND ARE MOSTLY ECOLOGY ARTICLES

# I NEED TO SEARCH AGAIN USING PMIDs FROM ALTMETRIC BECAUSE SOME CORRECT DOIS ARE ON PUBMED WITH EITHER ANOTHER DOI OR NO DOI AT ALL!

# Import manually extracted PMIDs
manual <- read_excel("Missing.xlsx")
manual <- data.frame(manual)


# Isolate manually extracted PMIDs
man.pmid <- manual$PMID[manual$PMID != "NA"]

# How many PMIDs did I discover manually?
length(man.pmid)
# 32

# Construct search terms
searchTerms <- paste(man.pmid, collapse = "[UID] OR ") %>% paste0("[UID]")

# Initialize
articles.man <- fetchArticles(searchTerms)
  
# Validate dimensions
dim(articles.man)

# Order data frame as in imported dataset
articles.man <- articles.man[match(man.pmid, articles.man$PMID), ]

# Replace PubMed-based DOI by bioRxiv-based DOI
articles.man$DOI <- manual$DOI[manual$PMID != "NA"]

# Combine the two data frames of PubMed data
all.articles <- rbind(articles.doi, articles.man)

# Log dimensions
dim(all.articles)
# [1] 2586   19

# How many papers could I not find on PubMed?
nrow(REF.POSTPUB) - nrow(all.articles)
# [1] 45

# Identify the DOIs of the articles I could not find on PubMed
length(setdiff(doipub, all.articles$DOI))
# [1] 55

# SO ONLY 55 OF 2631 were actually not on PUBMED!

# Identify DOIs in all.articles that are not in doipub
setdiff(toupper(all.articles$DOI), toupper(doipub))
# [1] "10.12688/F1000RESEARCH.7082.1" "10.1098/<U+200B>RSIF.2013.1095"

# Test for "10.12688/F1000RESEARCH.7082.1" whether I have a different version
grep("10.12688/F1000RESEARCH.7082", toupper(doipub))
# [1] 1177

# View
doipub[1177]
# "10.12688/f1000research.7082.3"

# Find this record in all.articles
grep("10.12688/F1000RESEARCH.7082", toupper(all.articles$DOI))
# [1] 1142 2571

# View
all.articles$DOI[1142]
# [1] "10.12688/f1000research.7082.1"
all.articles$DOI[2571]
# [1] "10.12688/f1000research.7082.3"

# Remove 1142 from all.articles
all.articles <- all.articles[-1142, ]

# Find "10.1098/<U+200B>RSIF.2013.1095"
grep("RSIF.2013.1095", toupper(all.articles$DOI))
# [1] 2557

# Correct it
all.articles$DOI[2557] <- "10.1098/rsif.2013.1095"

# Ascertain that no further discrepancies
length(setdiff(toupper(all.articles$DOI), toupper(doipub)))
# [1] 0

# I had 2631 DOIs - I did not find 54, so I should have 2577 records. However, all.articles has 2585 records, which means that I have 8 duplicates, unless doipub has duplicates.

# Look for duplicates in doipub
doipub[duplicated(doipub)]
# [1] "10.1098/rsob.160009"        "10.1016/j.ajhg.2015.03.004" "10.1111/jeb.12972"  

# Investigate why this happened, one by one
grep("10.1098/rsob.160009", doipub)
# [1]  975 1195

# Pull the record from REF.PUB
grep("10.1098/rsob.160009", REF.PUB$DOI_Pub)
# [1] 1706 2086

# Pull the record from REF.ARTICLES
REF.ARTICLES[c(1706, 2086), ]

# Structure and evolutionary history of a large family of NLR proteins in the zebrafish

# So comparing the two records it does seem that the two are identical, but have separate bioRxiv DOIs. The two records refer to the same paper because they have the same published DOI associated to them.

# Investigate why this happened, one by one
grep("10.1016/j.ajhg.2015.03.004", doipub)
# [1]  388 1921

# Pull the record from REF.PUB
grep("10.1016/j.ajhg.2015.03.004", REF.PUB$DOI_Pub)
# [1] 655 3630

# Pull the record from REF.ARTICLES
REF.ARTICLES[c(655, 3630), ]

# Mixed Model with Correction for Case-Control Ascertainment Increases Association Power

# Same as for the previous

# Investigate why this happened, one by one
grep("10.1111/jeb.12972", doipub)
# [1]  1882 2239

# Pull the record from REF.PUB
grep("10.1111/jeb.12972", REF.PUB$DOI_Pub)
# [1] 3505 6391

# Pull the record from REF.ARTICLES
REF.ARTICLES[c(3505, 6391), ]

# Escherichia coli populations adapt to complex, unpredictable fluctuations without any trade-offs across environments

# Same as for the previous. bioRxiv does indeed have two identical records with separate bioRxiv DOIs for each.

# I should have remove these early on. I did not, so I will do so just before uniting the databases.

# So it seems that I have 7757 distinct records and doipub has 2628 distinct records. There is a possibility that some of them refer to the same article, but with very similar publication DOIs. The all.articles has 2585, but 2585 + 54 = 2639, so there are 11 duplicates.

# Look for potential duplicate records in all.articles
repeats <- all.articles$DOI[duplicated(all.articles$Title)]

# How many repeating records?
length(repeats)
# [1] 12

sum(duplicated(all.articles$DOI))

# Identify titles
all.articles$Title[duplicated(all.articles$Title)]
# [1] "Structure and evolutionary history of a large family of NLR proteins in the zebrafish."                                                                               
# [2] "Mixed model with correction for case-control ascertainment increases association power."                                                                              
# [3] "Functional analysis of a biosynthetic cluster essential for production of 4-formylaminooxyvinylglycine, a germination-arrest factor from Pseudomonas fluorescens WH6."

# [4] "Escherichia coli populations adapt to complex, unpredictable fluctuations by minimizing trade-offs across environments."                                              
# [5] NA                                                                                                                                                                     
# [6] "Revisiting demand rules for gene regulation."                                                                                                                         
# [7] "Predicting genetic interactions from Boolean models of biological networks."                                                                                          
# [8] "A synthetic gene circuit for measuring autoregulatory feedback control."                                                                                              
# [9] "Analysis of the optimality of the standard genetic code."                                                                                                             
# [10] "Estimation of the lag time in a subsequent monomer addition model for fibril elongation."                                                                             
# [11] "Multiple binding modes of ibuprofen in human serum albumin identified by absolute binding free energy calculations."                                                  
# [12] "Autonomous microfluidic capillaric circuits replicated from 3D-printed molds." 

# So the first 2 and the 4th I know are duplicates due to bioRxiv. Let's investigate the 3rd:
grep("Functional analysis of a biosynthetic cluster essential", all.articles$Title)
# [1] 1930 1978

# What are their DOIs?
all.articles$DOI[c(1930, 1978)]
# "10.1099/mic.0.000418"

# They have the same DOI. So I need to remove one of them. Let's go to next.

# What record gave me an NA for title?
all.articles[is.na(all.articles$Title), ]

# Two records gave me an NA for Title. Their PMIDs are: 27501063 and 27980682. I will manually replace their NA with the proper titles. Their row numbers are: 1847 and 2351.
all.articles$Title[1847] <- "\"Is voice a marker for Autism spectrum disorder? A systematic review and meta-analysis\"."
all.articles$Title[2351] <- "\"Gap hunting\" to characterize clustered probe signals in Illumina methylation array data."

# Let's investigate 6: "Revisiting demand rules for gene regulation.".
grep("Revisiting demand rules for gene regulation.", all.articles$Title)
# [1]  615 2566

# What's their DOI?
all.articles$DOI[c(615, 2566)]
# [[1]]
# [1] "10.1039/c5mb00693g"
# 
# [[2]]
# [1] "10.1039/C5MB00693G"

# So one was in small and the other in capital letters. Let's see if this was the same for the rest:
length(all.articles$DOI[duplicated(toupper(all.articles$DOI))])
# [1] 11

# Yes it was. Let's see if this was also the case for bioRxiv
sum(duplicated(toupper(doipub)))
# [1] 3

# No, it wasn't. Now I have 11 duplicates, which makes sense, given that 2639 - 2628 = 11. Let's remove those duplicates
undup.all <- all.articles[!duplicated(toupper(all.articles$DOI)), ]

# Confirm undup has the expected dimensions: 2628 - 54 = 2575
dim(undup.all)
# [1] 2574   19

# Confirm that number of unmatched records remained the same
length(setdiff(toupper(doipub), toupper(undup.all$DOI)))
# [1] 54

# Confirm no missing dates
sum(is.na(undup.all$Date))
# [1] 0

# Format dates
undup.all$Date <- as.Date(undup.all$Date)
manual$Publication_Date <- as.Date(manual$Publication_Date, "%d-%m-%Y")

# Create data frame of missing dates with respective DOIs
add.dates <- data.frame(DOI  = manual$DOI[manual$PMID == "NA"],
                        Date = manual$Publication_Date[manual$PMID == "NA"])

# Convert add.dates$DOI to character
add.dates$DOI <- as.character(add.dates$DOI)
undup.all$DOI <- unlist(undup.all$DOI)

# Create unified data frame with all dates
combi.pub <- bind_rows(add.dates, undup.all)

# Confirm combi.pub dimensions
dim(combi.pub)
# [1] 2628   19





































# YES IT DOES WORK, SO I NEED TO SWITCH.

# Fetch all data from PubMed
postpub <- list()
pbar    <- txtProgressBar(min = 0, max = length(pmid), style = 3)
kpval   <- 0
for (i in 1:length(pmid)){
  data       <- efetch(pmid[i], db = "pubmed", retmode = "xml")
  postpub[i] <- data$content
  
  kpval      <- kpval + 1
  setTxtProgressBar(pbar, kpval)
}

# Get the date
newData <- pblapply(postpub, xmlParse)

# Build data frame
PUB <- data.frame(pmid = c(), doi = c(), Year = c(), Month = c(), Day = c(),
                  stringsAsFactors = FALSE)

pbar    <- txtProgressBar(min = 0, max = length(postpub), style = 3)
kpval   <- 0
for (i in 1:length(postpub)){
  
  records <- getNodeSet(newData[[i]], "//PubmedArticle")
  pmid    <- xpathSApply(newData[[i]],"//MedlineCitation/PMID", xmlValue)
  doi     <- lapply(records, xpathSApply, 
                    ".//ELocationID[@EIdType = \"doi\"]", xmlValue)
  doi[sapply(doi, is.list)] <- NA
  doi  <- unlist(doi)
  year <- lapply(records, xpathSApply, ".//PubDate/Year", xmlValue) 
  year[sapply(year, is.list)] <- NA
  year  <- unlist(year)
  month <- lapply(records, xpathSApply, ".//PubDate/Month", xmlValue) 
  month[sapply(month, is.list)] <- NA
  month <- unlist(month)
  day   <- lapply(records, xpathSApply, ".//PubDate/Day", xmlValue) 
  day[sapply(day, is.list)] <- NA
  day <- unlist(day)
  
  PUB.temp <- data.frame(pmid, doi, year, month, day)
  PUB      <- rbind(PUB, PUB.temp)
  
  kpval <- kpval + 1
  setTxtProgressBar(pbar, kpval)
}

# Build date
PUB$Date <- paste(PUB$year, PUB$month, sep = "-")

# Remove values with NAs in date
# (I will disregard the day and assume all published first day of month)
ind <- grep("NA", PUB$Date)

# PUB2
PUB2 <- PUB[-ind,]
PUB2$Date <- gsub("-01", "-Jan", PUB2$Date)
PUB2$Date <- gsub("-02", "-Feb", PUB2$Date)
PUB2$Date <- gsub("-03", "-Mar", PUB2$Date)
PUB2$Date <- gsub("-04", "-Apr", PUB2$Date)
PUB2$Date <- gsub("-05", "-May", PUB2$Date)
PUB2$Date <- gsub("-06", "-Jun", PUB2$Date)
PUB2$Date <- gsub("-07", "-Jul", PUB2$Date)
PUB2$Date <- gsub("-08", "-Aug", PUB2$Date)
PUB2$Date <- gsub("-09", "-Sep", PUB2$Date)
PUB2$Date <- gsub("-10", "-Oct", PUB2$Date)
PUB2$Date <- gsub("-11", "-Nov", PUB2$Date)
PUB2$Date <- gsub("-12", "-Dec", PUB2$Date)
PUB2$Date <- paste(PUB2$Date, "-01", sep = "")
PUB2$Date <- as.Date(PUB2$Date, format = "%Y-%b-%d")

colnames(PUB2)[2] <- "DOI_Pub"

# Merge with REF.POSTPUB by DOI
colnames(PUB2)[2] <- "DOI_Pub"
REF.COMP1 <- merge(REF.POSTPUB, PUB2)
colnames(PUB2)[1] <- "pmid_pub"
REF.COMP2 <- merge(REF.ALL, PUB2, by = "DOI_Pub")

# Use example code from here to run further queries
# https://github.com/christopherBelter/pubmedXML/blob/master/pubmedXML.R

extract_xml <- function(theFile) {
  library(XML)
  newData <- xmlParse(theFile)
  records <- getNodeSet(newData, "//PubmedArticle")
  pmid <- xpathSApply(newData,"//MedlineCitation/PMID", xmlValue)
  doi <- lapply(records, xpathSApply, ".//ELocationID[@EIdType = \"doi\"]", xmlValue)
  doi[sapply(doi, is.list)] <- NA
  doi <- unlist(doi)
  authLast <- lapply(records, xpathSApply, ".//Author/LastName", xmlValue)
  authLast[sapply(authLast, is.list)] <- NA
  authInit <- lapply(records, xpathSApply, ".//Author/Initials", xmlValue)
  authInit[sapply(authInit, is.list)] <- NA
  authors <- mapply(paste, authLast, authInit, collapse = "|")
  ## affiliations <- lapply(records, xpathSApply, ".//Author/AffiliationInfo/Affiliation", xmlValue)
  ## affiliations[sapply(affiliations, is.list)] <- NA
  ## affiliations <- sapply(affiliations, paste, collapse = "|")
  year <- lapply(records, xpathSApply, ".//PubDate/Year", xmlValue) 
  year[sapply(year, is.list)] <- NA
  year <- unlist(year)
  articletitle <- lapply(records, xpathSApply, ".//ArticleTitle", xmlValue) 
  articletitle[sapply(articletitle, is.list)] <- NA
  articletitle <- unlist(articletitle)
  journal <- lapply(records, xpathSApply, ".//ISOAbbreviation", xmlValue) 
  journal[sapply(journal, is.list)] <- NA
  journal <- unlist(journal)
  volume <- lapply(records, xpathSApply, ".//JournalIssue/Volume", xmlValue)
  volume[sapply(volume, is.list)] <- NA
  volume <- unlist(volume)
  issue <- lapply(records, xpathSApply, ".//JournalIssue/Issue", xmlValue)
  issue[sapply(issue, is.list)] <- NA
  issue <- unlist(issue)
  pages <- lapply(records, xpathSApply, ".//MedlinePgn", xmlValue)
  pages[sapply(pages, is.list)] <- NA
  pages <- unlist(pages)
  abstract <- lapply(records, xpathSApply, ".//Abstract/AbstractText", xmlValue)
  abstract[sapply(abstract, is.list)] <- NA
  abstract <- sapply(abstract, paste, collapse = "|")
  meshHeadings <- lapply(records, xpathSApply, ".//DescriptorName", xmlValue)
  meshHeadings[sapply(meshHeadings, is.list)] <- NA
  meshHeadings <- sapply(meshHeadings, paste, collapse = "|")
  grantAgency <- lapply(records, xpathSApply, ".//Grant/Agency", xmlValue)
  grantAgency[sapply(grantAgency, is.list)] <- NA
  grantAgency <- sapply(grantAgency, paste, collapse = "|")
  grantAgency <- sapply(strsplit(grantAgency, "|", fixed = TRUE), unique)
  grantAgency <- sapply(grantAgency, paste, collapse = "|")
  grantNumber <- lapply(records, xpathSApply, ".//Grant/GrantID", xmlValue)
  grantNumber[sapply(grantNumber, is.list)] <- NA
  grantNumber <- sapply(grantNumber, paste, collapse = "|")
  grantCountry <- lapply(records, xpathSApply, ".//Grant/Country", xmlValue)
  grantCountry[sapply(grantCountry, is.list)] <- NA
  grantCountry <- sapply(grantCountry, paste, collapse = "|")
  grantCountry <- sapply(strsplit(grantCountry, "|", fixed = TRUE), unique)
  grantCountry <- sapply(grantCountry, paste, collapse = "|")
  ptype <- lapply(records, xpathSApply, ".//PublicationType", xmlValue)
  ptype[sapply(ptype, is.list)] <- NA
  ptype <- sapply(ptype, paste, collapse = "|")
  theDF <- data.frame(pmid, doi, authors, year, articletitle, journal, volume, issue, pages, abstract, meshHeadings, grantAgency, grantNumber, grantCountry, ptype, stringsAsFactors = FALSE)
  return(theDF)
}

extract_xml(data$content)

# Consider the following code
xmlToList(theData)

# Or this code
# https://www.r-bloggers.com/parse-and-process-xml-and-html-with-xml2/
library(xml2) # supposed to be better than XML memory-wise
ddat <- read_xml(data$content)
recs <- xml_find_all(ddat, "//PubmedArticle")
# extract and clean all the columns
vals <- trimws(xml_text(ddat))
# extract and clean (if needed) the area names
# labs <- trimws(xml_attr(recs, "label"))
# cols <- xml_attr(xml_find_all(pg, "//data/variables/*[self::categoricalvariable or self::realvariable]"), "name")
# Turn into a list
d.list <- as_list(ddat)
frame  <- data.frame(d.list)

# e <- getURL("https://www.ncbi.nlm.nih.gov/pubmed/25806692?report=xml&format=text")
# ee <- xmlParse(e)
# eee <- read_xml(e)
# 
# download.file("https://www.ncbi.nlm.nih.gov/pubmed/25806692?report=xml&format=text", destfile=tf <- tempfile(fileext=".xml"))
# doc <- xmlParse(tf)
# xpathSApply(doc, "/Year", xmlValue)
# 
# fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
# download.file(fileURL, destfile=tf <- tempfile(fileext=".xml"))
# doc <- xmlParse(tf)
# 
# capture.output(a$children$html, file = NULL)

# Look at these websites by PubMed
# "https://www.ncbi.nlm.nih.gov/pmc/tools/cites-citedby/"
# "https://www.ncbi.nlm.nih.gov/books/NBK25500/#chapter1.Downloading_Full_Records"



# Synthesise and Export ---------------------------------------------------

# Create database of all data about preprints
preprint <- cbind(REF.ARTICLES, REF.DETAILS, REF.INFO, 
                  REF.PUB, REF.METRICS, REF.ALTMETRIC)

# Create database of all data about published paper
x <- combi.pub
x$DOI <- toupper(x$DOI)

y <- REF.POSTPUB[!duplicated(REF.POSTPUB$biorxiv_doi), ]
y$biorxiv_doi <- toupper(y$biorxiv_doi)

pstprint <- merge(x, y, by.x = "DOI", by.y = "biorxiv_doi")

# Import bioRxiv DOI as a common ID column
pstprint$biorxiv_doi <- preprint$doi[match(pstprint$DOI, preprint$doi)]

# Export
setwd("/Users/Stelios/Desktop")
write.csv(REF.ALL, "bioRxiv.csv")