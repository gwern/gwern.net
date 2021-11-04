#!/usr/bin/env Rscript

# LinkAbstracter
# Author: gwern
# Date: 2019-08-29
# When:  Time-stamp: "2021-11-03 22:24:15 gwern"
# License: CC-0
#
# Read a PLOS or PMCID URL, and return the parsed fulltext as newline-delimited Title/Author/Date/DOI/Abstract.
#
# Pubmed Central example:
#
# $ Rscript linkAbstract.R "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2793346/"
# The physiological effects of Shinrin-yoku (taking in the forest atmosphere or forest bathing): evidence from field experiments in 24 forests across Japan
# Bum Jin Park, Yuko Tsunetsugu, Tamami Kasetani, Takahide Kagawa, Yoshifumi Miyazaki
# 2000-12-01
# 10.1007/s12199-009-0086-9
# This paper reviews previous research on the physiological effects of Shinrin-yoku (taking in the forest atmosphere or forest bathing), and presents new results from field experiments conducted in 24 forests across Japan. The term Shinrin-yoku was coined by the Japanese Ministry of Agriculture, Forestry, and Fisheries in 1982, and can be defined as making contact with and taking in the atmosphere of the forest. In order to clarify the physiological effects of Shinrin-yoku, we conducted field experiments in 24 forests across Japan. In each experiment, 12 subjects (280 total; ages 21.7 ± 1.5 year) walked in and viewed a forest or city area. On the first day, six subjects were sent to a forest area, and the others to a city area. On the second day, each group was sent to the other area as a cross-check. Salivary cortisol, blood pressure, pulse rate, and heart rate variability were used as indices. These indices were measured in the morning at the accommodation facility before breakfast and also both before and after the walking (for 16 ± 5 min) and viewing (for 14 ± 2 min). The R–R interval was also measured during the walking and viewing periods. The results show that forest environments promote lower concentrations of cortisol, lower pulse rate, lower blood pressure, greater parasympathetic nerve activity, and lower sympathetic nerve activity than do city environments. These results will contribute to the development of a research field dedicated to forest medicine, which may be used as a strategy for preventive medicine.
#
# NOTE: Pubmed rate-limits the public API; to reduce the risk of being blacklisted, get an API key and set it in R with something lik `Sys.setenv(ENTREZ_KEY="fe7ca39604a5de176be4c2a1cbd2b2902108")`
#
# PLOS example (NOTE: ' ' thin spaces are used in the formatting):
#
# $ Rscript  linkAbstract.R 'http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0100248'
# Genetic Variation Associated with Differential Educational Attainment in Adults Has Anticipated Associations with School Performance in Children
# Mary E. Ward, George McMahon, Beate St Pourcain, David M. Evans, Cornelius A. Rietveld, Daniel J. Benjamin, Philipp D. Koellinger, David Cesarini, NA NA, George Davey Smith, Nicholas J. Timpson
# 2014-05-22
# 10.1371/journal.pone.0100248
# <abstract>
#   <p>Genome-wide association study results have yielded evidence for the association of common genetic variants with crude measures of completed educational attainment in adults. Whilst informative, these results do not inform as to the mechanism of these effects or their presence at earlier ages and where educational performance is more routinely and more precisely assessed. Single nucleotide polymorphisms exhibiting genome-wide significant associations with adult educational attainment were combined to derive an unweighted allele score in 5,979 and 6,145 young participants from the Avon Longitudinal Study of Parents and Children with key stage 3 national curriculum test results (SATS results) available at age 13 to 14 years in English and mathematics respectively. Standardised (z-scored) results for English and mathematics showed an expected relationship with sex, with girls exhibiting an advantage over boys in English (0.433 SD (95%CI 0.395, 0.470), p&lt;10<sup>−10</sup>) with more similar results (though in the opposite direction) in mathematics (0.042 SD (95%CI 0.004, 0.080), p = 0.030). Each additional adult educational attainment increasing allele was associated with 0.041 SD (95%CI 0.020, 0.063), p = 1.79×10<sup>−04</sup> and 0.028 SD (95%CI 0.007, 0.050), p = 0.01 increases in standardised SATS score for English and mathematics respectively. Educational attainment is a complex multifactorial behavioural trait which has not had heritable contributions to it fully characterised. We were able to apply the results from a large study of adult educational attainment to a study of child exam performance marking events in the process of learning rather than realised adult end product. Our results support evidence for common, small genetic contributions to educational attainment, but also emphasise the likely lifecourse nature of this genetic effect. Results here also, by an alternative route, suggest that existing methods for child examination are able to recognise early life variation likely to be related to ultimate educational attainment.</p>

args = commandArgs(trailingOnly=TRUE)[1]

if (grepl("plos",args)) {
   # assume easy PLOS case:

    # handle cases like 'http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0100248' where the DOI is URL-encoded
    url <- URLdecode(args[1])

    # extract DOI, handling any anchors or query parameters:
    # "http://www.plosone.org/article/info:doi/10.1371/journal.pone.0100248#close" → [1] "10.1371/journal.pone.0100248"
    # "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0100248#close" → [1] "10.1371/journal.pone.0100248"

    doi <- gsub(".*info:doi/(.*/.*[[:digit:]]).*", "\\1",
                 gsub(".*id=(.*/.*[[:digit:]]).*", "\\1", url))

    # look up in PLOS:
    library(fulltext)
    # NOTE: a PLOS DOI might not have fulltext, even if it's a valid DOI: PLOS assigns individual DOIs to *parts of articles* like figures or tables or supplements. See `ft_get-warnings` documentation.
    fulltext <-  tryCatch(ft_get(doi, from="entrez"), warning=function(w) { print(w); cat(doi); cat(args[1]); quit(status=1) })

    library(pubchunks)
    y <- (fulltext %>% ft_collect("author", "title") %>% pub_chunks())$entrez[[1]]
    # NOTE: we preserve XML/HTML formatting in the abstract (but not other fields), such as headers or subscripts, by using 'as.character' option
    abstract <- gsub("\\*", "×", (fulltext %>% ft_collect("abstract") %>% pub_chunks(extract="as.character"))$entrez[[1]]$abstract)

    title <- gsub("\n *", " ", y$title)
    author <- paste(sapply(y$authors, function(a) { paste(a$given_names, a$surname)}), collapse=", ")
    # doi
    date <- y$history$accepted

    # DOIs are optional since so many fulltext PMC papers are still missing them
    if (any(c(is.list(title), is.list(author), is.list(date), is.list(abstract)))) {

        # fallback to the other fulltext/rentrez path:
        # if the backup query fails, fail out:
        if (any(c(is.list(title), is.list(author), is.list(date), is.list(abstract), (nchar(abstract) < 50)))) {
          cat(paste("Some medata missing, exiting with error:", args[1], title, author, date, doi, abstract, sep="\n"), "\n")
          quit(status=1)  }
    }

    cat(c(title, "\n"))
    cat(c(author, "\n"))
    cat(c(as.character(date), "\n"))
    if (is.list(doi)) { cat("\n"); } else { cat(c(doi, "\n")) }
    cat(c(abstract, "\n"))

} else {
  # assume PMC
    pmcid <- sub("/", "", sub("/pdf/.*", "", sub("https?://www.ncbi.nlm.nih.gov/pmc/articles/PMC", "", args[1])))

    library(fulltext)
    library(rentrez)
    library(pubchunks)

    pmcidSearch = paste0("PMC", pmcid, "[pmcid]")

    paper    <- entrez_search(db="pubmed", term=pmcidSearch)
    rawXML   <- entrez_fetch(db="pubmed", id=paper$id, rettype="xml")
    fulltext <- parse_pubmed_xml(rawXML)

    # handle section labels, like 'Methods and Materials', 'Conclusion', etc: https://github.com/ropensci/rentrez/issues/170
    # PMC, turns out, encodes those descriptions into all-uppercase XML 'labels' which are not payload/text, and rentrez by default strips them like it would any other bit of XML metadata. The different sections are then by default collapsed into a single run-on paragraph.
    # to deal with this, we parse out the special labels, convert them to mixed-case (because `toTitlecase` preserves all-uppercase, we force to lowercase first), surround them with <strong></strong> per gwern.net house style for inline-headings, and combine the section header label and section text pairwise, and intersperse double-newlines to force separate paragraphs when parsed by LinkMetadata.hs as Markdown.
    # And if there are no section headers, then we just use the abstract as is.
    library(XML)
    library(tools)
    parsed_XML <- entrez_fetch(db="pubmed", id=paper$id, rettype="xml", parsed=TRUE)
    labels     <- sapply(parsed_XML["//Abstract/AbstractText"], xmlGetAttr, "Label")
    labelsFormatted <- sapply(tolower(labels), function(s) { paste0("<strong>", toTitleCase(s), "</strong>"); })
    text       <- sapply(parsed_XML["//Abstract/AbstractText"], xmlValue)
    combined        <- paste0(paste0(labelsFormatted, rep(": ", length(labelsFormatted)), text), collapse="\n\n")

    title    <- fulltext$title
    # convert `[1] "Kosuri, Sriram"   "Church, George M"` into "Sriram Kosuri, George M Church"
    author   <- paste(unlist(
          Map(function(name) { paste(rev(unlist(strsplit(name, ", "))), collapse=" ") }, fulltext$author)),
      collapse=", ")
    date     <- fulltext$year
    doi      <- fulltext$doi
    abstract <- { if (length(labels) > 1) { combined; } else { fulltext$abstract; } }

    # DOIs are optional since so many fulltext PMC papers are still missing them
    if (any(c(is.list(title), is.list(author), is.list(date), is.list(abstract)))) {
        # fallback to the other fulltext/rentrez path:

        pmid <- entrez_search(db = "pubmed", term = pmcidSearch)$ids
        ids <- entrez_summary(db="pubmed", id=pmid)$articleids
        doi <- ids[ids$idtype=="doi",]$value
        f <- ft_get(doi, from="entrez", callopts=list(http_version = 0L))
        f2 <- f %>% ft_collect()
        fulltext <- (f %>% ft_collect() %>% pub_chunks(c("title","authors","front")))$entrez[[1]]
        fulltextXML <- (f %>% ft_collect() %>% pub_chunks(c("abstract"), extract="as.character"))$entrez[[1]]
        title    <- fulltext$title
        author   <- paste(sapply(fulltext$authors, function(a) { paste(a$given_names, a$surname)}), collapse=", ")
        date     <- as.character(as.data.frame(fulltext$front)$pub.date[1])
        date     <- if (is.list(date)) { "" } else { date }
        doi      <- fulltext$doi
        doi      <- if (is.list(doi)) { "" } else { doi }
        abstract <- fulltextXML$abstract

        # if the backup query fails, fail out:
        if (any(c(is.list(title), is.list(author), is.list(date), is.list(abstract)))) {
          cat(paste("Some medata missing, exiting with error:", args[1], pmcid, title, author, date, doi, abstract, sep="\n"), "\n")
          quit(status=1)  }
    }

    cat(c(title, "\n"))
    cat(c(author, "\n"))
    cat(c(as.character(date), "\n"))
    if (is.list(doi)) { cat("\n"); } else { cat(c(doi, "\n")) }
    cat(paste0(abstract, sep="\n\n"))
}
