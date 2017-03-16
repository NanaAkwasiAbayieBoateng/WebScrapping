#==============
# LOAD PACKAGES
#==============

library(tidyverse)
library(stringr)
library(forcats)
library(ggmap)
library(rvest)
library(tm)
library(SnowballC)
library(dplyr)
library(calibrate)
library(stringi)
library(ggplot2)
library(maps)
#install.packages("SnowballC")
#==========================
#SCRAPE DATA FROM WIKIPEDIA
#==========================

# SCRAPE
html.world_ports <- read_html("https://en.wikipedia.org/wiki/List_of_busiest_container_ports")

df.world_ports <- html_table(html_nodes(html.world_ports, "table")[[2]], fill = TRUE)


# INSPECT
glimpse(df.world_ports)



%>%
  html_text()
#-----------
# LOWER CASE
#-----------
colnames(df.world_ports) <- colnames(df.world_ports) %>% tolower()

# INSPECT
colnames(df.world_ports)



#===================================================
# GEOCODE
# - here, we're picking up lat/long from Google Maps
#   using ggmaps::geocode()
#===================================================


#--------------------------
# get data from google maps
#--------------------------
geocodes.world_ports <- geocode(df.world_ports$port)


#--------------------------------------------------------
# COMBINE:
# - bind the new lat/long data to df.world_ports data frame
#--------------------------------------------------------
df.world_ports <- cbind(df.world_ports, geocodes.world_ports)

head(df.world_ports)

#=========================================================================================
# RECODE lon and lat
# - There are 4 lon/lat values that weren't found with geocode()
# - We'll just hand code them
# - The values can be obtained at http://www.latlong.net/convert-address-to-lat-long.html
##
# Tanjung Pelepas, Johor Bahru: lon = 103.551035, lat = 1.362374
# Yingkou:..................... lon = 122.108231, lat = 40.266062
# Valencia, Spain:............. lon = -0.3762881, lat = 39.46991
# Malta Freeport:.............. lon = 14.537637 , lat = 35.816287
#
#=========================================================================================
df.world_ports <- df.world_ports %>%
  mutate( lon = case_when(.$port == "Tanjung Pelepas" ~ 103.551035
                          ,.$port == "Yingkou" ~ 122.108231
                          ,.$port == "Valencia" ~ -0.3762881
                          ,.$port == "Malta Freeport" ~ 14.537637
                          ,TRUE ~ .$lon
  )
  ,lat = case_when(.$port == "Tanjung Pelepas" ~ 1.362374
                   ,.$port == "Yingkou" ~ 40.266062
                   ,.$port == "Valencia" ~ 39.46991
                   ,.$port == "Malta Freeport" ~ 35.816287
                   ,TRUE ~ .$lat
  )
  )
# CHECK
df.world_ports %>% filter(port == "Tanjung Pelepas") %>% select(lat,lon)
df.world_ports %>% filter(port == "Yingkou") %>% select(lat,lon)
df.world_ports %>% filter(port == "Valencia") %>% select(lat,lon)
df.world_ports %>% filter(port == "Malta Freeport") %>% select(lat,lon)

#=============================
# convert variables to factors
#=============================
df.world_ports <- mutate(df.world_ports
                         , economy = as.factor(str_trim(economy))
                         , port = as.factor(port)
)







# install.packages("devtools")
#devtools::install_github("hadley/rvest")
#install.packages("rvest")

library(rvest)
lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")
#lego_movie=html_table(html_nodes(lego_movie, "table")[[2]], fill = TRUE)
rating <- lego_movie %>% 
  html_nodes("strong span") %>%
  html_text() %>%
  as.numeric()
rating
#> [1] 7.8

cast <- lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast
#>  [1] "Will Arnett"     "Elizabeth Banks" "Craig Berry"    
#>  [4] "Alison Brie"     "David Burrows"   "Anthony Daniels"
#>  [7] "Charlie Day"     "Amanda Farinos"  "Keith Ferguson" 
#> [10] "Will Ferrell"    "Will Forte"      "Dave Franco"    
#> [13] "Morgan Freeman"  "Todd Hansen"     "Jonah Hill"

poster <- lego_movie %>%
  html_nodes(".poster img") %>%
  html_attr("src")
poster
#> [1] "http://ia.media-imdb.com/images/M/MV5BMTg4MDk1ODExN15BMl5BanBnXkFtZTgwNzIyNjg3MDE@._V1_UX182_CR0,0,182,268_AL_.jpg"



#The most important functions in rvest are:
  
#  Create an html document from a url, a file on disk or a string containing html with 
#read_html().

#Select parts of a document using css selectors: html_nodes(doc, "table td") (or if you've 
#a glutton for punishment, use xpath selectors with html_nodes(doc, xpath = "//table//td")). 
#If you haven't heard of selectorgadget, make sure to read vignette("selectorgadget") to 
#learn about it.
                                                                             
# Extract components with html_tag() (the name of the tag), html_text() (all text inside 
#the tag), html_attr() (contents of a single attribute) and html_attrs() (all attributes).
                                                                             
#(You can also use rvest with XML files: parse with xml(), then extract components using 
#xml_node(), xml_attr(), xml_attrs(), xml_text() and xml_tag().)
                                                                             
# Parse tables into data frames with html_table().
# Extract, modify and submit forms with html_form(), set_values() and submit_form().
                                                                             
#Detect and repair encoding problems with guess_encoding() and repair_encoding().
                                                                             
#Navigate around a website as if you're in a browser with html_session(), jump_to(), 
#follow_link(), back(), forward(), submit_form() and so on. (This is still a work in 
#progress, so I'd love your feedback.)

#To see examples of these function in use, check out the demos.



# SCRAPE
html.world_ports <- read_html("https://en.wikipedia.org/wiki/List_of_National_Basketball_Association_single-game_scoring_leaders")

df.world_ports <- html_table(html_nodes(html.world_ports, "table")[[2]], fill = TRUE)


# INSPECT
glimpse(df.world_ports)

head(df.world_ports)


page <- read_html("http://journals.ametsoc.org/doi/full/10.1175/JCLI-D-13-00693.1")
paper=page%>% html_nodes(css = ".NLM_sec_level_1")%>%html_text()
paper[1] # Let's see the first one

length(paper)

paper=str_replace_all(paper, "[^[:alnum:]]", " ")
paper=gsub("[^A-Za-z0-9 ]", "-", paper)
paper[2]


# Create corpus
corpus = Corpus(VectorSource(paper[2]))

# Convert to lower-case
corpus = tm_map(corpus, tolower)
# convert corpus to a Plain Text Document
corpus = tm_map(corpus, PlainTextDocument)
# Remove punctuation
corpus = tm_map(corpus, removePunctuation)
# Remove stopwords
corpus = tm_map(corpus, removeWords, stopwords("english"))

# Create matrix
frequencies = DocumentTermMatrix(corpus)
findFreqTerms(frequencies, lowfreq=25)


wiki_kirk <- read_html("https://en.wikipedia.org/wiki/Kirk_Ferentz")
wiki_kirk_extract <- wiki_kirk %>%
  html_nodes(".vcard td , .vcard th")
head(wiki_kirk_extract)

glimpse(wiki_kirk_extract)


wiki_kirk_extract <- wiki_kirk %>%
  html_nodes(".vcard td , .vcard th") %>%
  html_text()
head(wiki_kirk_extract)


#==========================
#SCRAPE DATA FROM WIKIPEDIA
#==========================

# SCRAPE
wiki <- read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population")

wiki <- html_table(html_nodes(html.world_ports, "table")[[2]], fill = TRUE)

glimpse(wiki)

wiki1 <- html_table(html_nodes(html.world_ports, "table")[[1]], fill = TRUE)

glimpse(wiki1)


wiki= read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population")
states=wiki %>%
  html_nodes("table") %>%
  .[[1]]%>%
  html_table(fill=T)
head(states)



states[,1]=stri_sub(states[,1],22)
states[,2]=stri_sub(states[,2],22)
states[,7]=stri_sub(states[,7],22)
states[,3]=stri_sub(states[,3],3)
states[,12]=stri_sub(states[,12],3)

lego_movie <- html("http://www.imdb.com/title/tt1490017/")

vignette("selectorgadget")

lego_movie %>%
  html_node("strong span") %>%
  html_text() %>%
  as.numeric()





#===================================================
# rvest  
#  selector gadget installed in google chrome
#  selects the css h2 , .character
#===================================================

b1 <- read_html("http://www.imdb.com/title/tt1490017/")
b2 <- wiki_kirk %>%
  html_nodes("h2 , .character") %>%
  html_text()

#===================================================
#The curl() and curl_download() functions provide highly 
#configurable drop-in replacements for base url() and download.file() 
#with better performance, support for encryption (https, ftps), gzip compression,
#authentication, and other 'libcurl' goodies
#===================================================
#install.packages("curl")
library(curl)



read.table("https://drive.google.com/file/d/0ByPBn4rtMQ5HaVFITnBObXdtVUU/view")

library(data.table)
mydat <- fread('https://drive.google.com/file/d/0ByPBn4rtMQ5HaVFITnBObXdtVUU/view')


library(RCurl)
myfile <- getURL('https://drive.google.com/file/d/0ByPBn4rtMQ5HaVFITnBObXdtVUU/view', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

class(myfile)


urll='https://drive.google.com/file/d/0ByPBn4rtMQ5HaVFITnBObXdtVUU/view'

read.table.url(url,skip=4,header=T)

read.csv(url)

myfile=readLines(url)

read.table(textConnection(myfile), header=T)

mydat <- read.csv(textConnection(myfile), header=T)

read.url(url)

head(mydat)

mydat <- read.table(textConnection(myfile), header=T)
head(mydat)

library(RCurl)
myfile <- getURL('https://sakai.unc.edu/access/content/group/3d1eb92e-7848-4f55-90c3-7c72a54e7e43/public/data/bycatch.csv', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
mydat <- read.csv(textConnection(myfile), header=T)
head(mydat)
#===================================================
#
#  Read Google Sheets
#===================================================

#install.packages('gsheet')
library(gsheet)
x1=gsheet2tbl('docs.google.com/spreadsheets/d/1I9mJsS5QnXF2TNNntTy-HrcdHmIF9wJ8ONYvEJTXSNo')


head(x1)

gsheet2tbl(urll)

readGoogleSheet <- function(url, na.string="", header=TRUE){
  stopifnot(require(XML))
  # Suppress warnings because Google docs seems to have incomplete final line
  suppressWarnings({
    doc <- paste(readLines(url), collapse=" ")
  })
  if(nchar(doc) == 0) stop("No content found")
  htmlTable <- gsub("^.*?(<table.*</table).*$", "\\1>", doc)
  ret <- readHTMLTable(htmlTable, header=header, stringsAsFactors=FALSE, as.data.frame=TRUE)
  lapply(ret, function(x){ x[ x == na.string] <- NA; x})
}

readGoogleSheet(urll)


devtools::install_github("jennybc/googlesheets")
gs_gap() %>% 
  gs_copy(to = "Gapminder")
## or, if you don't use pipes
gs_copy(gs_gap(), to = "Gapminder")
#===================================================
#
#  Get Todays Date
#===================================================
Date <- Sys.Date()
datestring <- format(Date, '%Y/%m/%d')