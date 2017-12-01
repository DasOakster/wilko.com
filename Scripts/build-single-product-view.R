#--This script creates three functions: import.web.data; tidy.web.data; merge.web.data
#--5 files are imported:  Google Product Feed, Google Merchant Centre, SAP extract, PSA Owner and Web Category to Attribute mapping
#--These are imported, tidied up and then merged into a single file 'web.product.data'


import.web.data <- function() {

      setwd("./Import Data")

      
# Read in the Google Product Feed
      
      google.product.feed <<- read.csv("Google Product Feed.csv",sep = ",",stringsAsFactors = FALSE,na.strings = c("","NA"))
      google.header <- c("Article","Product Name","Publishing Status","Web Category","Type","Brand","Sub Brand","Size","Height","Width","Depth","Colour","Colour Family",
                         "Pack Qty","Material","Assembly","Washable","Power","Capacity","Coverage","Diameter","Age","Model Number","Supplier Direct")
      colnames(google.product.feed) <<- google.header

# Read in the Google Merchant Centre extract
      
      google.merchant.centre <<- read.csv("Google Merchant Centre.csv",sep = ",",stringsAsFactors = FALSE)
      google.merchant.header <- c("Article","Country","Language","Product Title","Issue","Feed Value","Website Value","Sample Time","Disapproval Time","Channel","Source","Additional Information")
      colnames(google.merchant.centre) <<- google.merchant.header

# Read in the SAP extract
      
      sap.product.extract <<- read.csv("SAP Web Product Data.csv",sep = ",",stringsAsFactors = FALSE)
      sap.header <- c("Article","Article Description","Hierarchy Node","Web Description 1","Web Description 2","Status","Old Article Number","Supplier","Current Supplier")
      colnames(sap.product.extract) <<- sap.header
      
      # Split SAP Hierarchy Node into PSA1 and PSA2 fields
      
      sap.product.extract$PSA_1 <<- substr(sap.product.extract$`Hierarchy Node`,1,3)
      sap.product.extract$PSA_2 <<- substr(sap.product.extract$`Hierarchy Node`,4,6)
      

# Read in the PSA Owner file and create a match by joining the PSA1 and PSA2 together
      
      psa.owner <<- read.csv("PSA Responsibility.csv",sep = ",",stringsAsFactors = FALSE, header = TRUE)
      psa.owner$'PSA Key' <<- paste(psa.owner$PSA_1,psa.owner$PSA_2,sep = "")
      

# Read in the PSA Responsibility file      
      
      web.attribute <<- read.csv("Web Category Attributes.csv",sep = ",",stringsAsFactors = FALSE, header = TRUE)
      
}

#------------------------------------------------------------------------------------------------------------------------------------------------

tidy.web.data <- function() {
      
# Remove illegal characters from the 'Supplier Name' in SAP
      
      sap.product.extract$Supplier <<- gsub("\\*","",sap.product.extract$Supplier)
      sap.product.extract$Supplier <<- gsub("\\/","",sap.product.extract$Supplier)
      
# Remove illegal characters and trailing spaces from the Web Category lable in Google
      
      google.product.feed$`Web Category` <<- gsub("\\/","-",google.product.feed$`Web Category`)
      google.product.feed$`Web Category` <<- gsub(" $","",google.product.feed$`Web Category`)

# Default any missing Web Categories to 'Missing'
      
      google.product.feed$`Web Category`[is.na(google.product.feed$`Web Category`)] <<- "Missing"
      google.product.feed$`Web Category`[google.product.feed$`Web Category` == "0"] <<- "Missing"

# Subset the Google Product feed to just 'Published' articles, drop off last column and remove '?' from brand name      
      
      published.article <<- subset(google.product.feed,grepl("Published",google.product.feed$`Publishing Status`) == TRUE)
      published.article <<- subset(published.article,select = c(1:24))
      published.article$Brand <<- gsub("\\?","",published.article$Brand)       
      
# Subset the Google Merchant Centre to Articles with a GTIN issue and trim leading zeroes from the Article number
      
      gtin.issue <<- subset(google.merchant.centre,(google.merchant.centre$Issue %in% c("Item requires a GTIN","GTIN is not associated with this brand","Invalid GTIN value","Reserved range GTIN")))
      gtin.issue$Article <<- substr(gtin.issue$Article,regexpr("[^0]",gtin.issue$Article),nchar(gtin.issue$Article))
      
# Convert vectors holding article numbers to Data Frames for the Inner Join ensuring they are all character fields for matching
      
      # Create a data frame to hold the article numbers of the published web products
      
      web.article <<- data.frame(published.article$Article,stringsAsFactors = FALSE)
      colnames(web.article) <<- "Article"

      # Create a data frame to hold the article numbers of the SAP products
    
      sap.article <<- data.frame(sap.product.extract$Article,stringsAsFactors = FALSE)
      colnames(sap.article) <<- "Article"
      sap.article$Article <<- sapply(sap.article$Article, as.character)
      
      # Create a data frame to hold the article numbers of the old AS400 products
      
      as400.article <<- data.frame(subset(sap.product.extract$`Old Article Number`,sap.product.extract$`Old Article Number`!=""))      
      colnames(as400.article) <<- "Article"
      as400.article$Article <<- sapply(as400.article$Article,tolower)

}

#------------------------------------------------------------------------------------------------------------------------------------------------

merge.web.data <- function() {
      
# Match the web and SAP data on article number and create vectors of matches and non matches
# Redo the match on the AS400 articles and the unmatched to check these
      
      primary.match <<- inner_join(web.article,sap.article)
      unmatched <<- anti_join(web.article,sap.article)
      
      secondary.match <<- inner_join(unmatched,as400.article)
      unmatched <<- anti_join(unmatched,as400.article)
      
# Create the web.product.data combining the Google Feed and the SAP extract
      
      # Subset the SAP data based on the primary and secondary matching
      
      sap.primary.match <<- subset(sap.product.extract,sap.product.extract$Article %in% primary.match$Article)
      sap.secondary.match <<- subset(sap.product.extract,tolower(sap.product.extract$`Old Article Number`) %in% secondary.match$Article)
      sap.secondary.match$Article <<- tolower(sap.secondary.match$`Old Article Number`)
      
      sap.matched <<- rbind(sap.primary.match,sap.secondary.match)
      
      # Subset the Google data based on the primary and secondary matching
      
      google.primary.match <<- subset(published.article,published.article$Article %in% primary.match$Article)
      google.secondary.match <<- subset(published.article,published.article$Article %in% secondary.match$Article)
      
      google.matched <<- rbind(google.primary.match,google.secondary.match)
      
      # Combine the Google and SAP data together into a single file
      
      web.product.data <<- merge(sap.matched,google.matched,by = "Article" )
      
# Add in PSA owners, PDT and Web Trading from the PSA Owner look up table
      
      web.product.data$'Asst Buyer' <<- ""
      web.product.data$'PDT' <<- ""
      web.product.data$'Web Trading' <<- ""
      
      # Create a PSA key to match between the web.product.data and psa.owner files
      
      web.product.data$'PSA Key' <<- paste(web.product.data$PSA_1,web.product.data$PSA_2,sep = "")
      web.product.data$`Asst Buyer`<<-psa.owner[match(web.product.data$'PSA Key',psa.owner$'PSA Key'),3]
      web.product.data$`PDT`<<- psa.owner[match(web.product.data$'PSA Key',psa.owner$'PSA Key'),4]
      web.product.data$`Web Trading`<<- psa.owner[match(web.product.data$'PSA Key',psa.owner$'PSA Key'),5]
      
      # Create a single Web Description field
      
      web.product.data$'Web Description' <- paste(web.product.data[,4],web.product.data[,5], sep = "")

# Check for undefined web categories i.e. no attribute template
      
      defined.web.category <<- unique(web.attribute$Category)
      current.google.web.category <<- unique(web.product.data$`Web Category`)
      
      undefined.web.category <<- setdiff(current.google.web.category,defined.web.category)
      
# Reorder columns and tidy up
      
      web.product.data <<- web.product.data[,c(1,2,12,4,5,39,10,11,14,6,7,13,8,34,35,36,37,16,17,15,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33)]

}
