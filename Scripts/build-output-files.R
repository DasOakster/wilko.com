#  Outputs each Asst Buyer's web articles

output.buyer.list <- function() {

all.buyers <- unique(web.product.data$`Asst Buyer`)

for (i in 1:length(all.buyers)) {
      
      buyer <- all.buyers[i]
      buyer.list.name <- paste(buyer, " Web Products",".csv",sep = "")
      
      attribute.list <- c("Brand","Size","Colour","Pack Qty","Material","Assembly","Washable","Power",
                          "Capacity","Coverage","Age","Model Number")
      
      buyer.list <- subset(web.product.data,web.product.data$`Asst Buyer`== buyer)
      buyer.list <- buyer.list[,c("PSA_1","PSA_2","Article","Web Description",attribute.list,"DQ Score")]
      buyer.list <- buyer.list[order(buyer.list[,1],buyer.list[,2],buyer.list[,5]),]
      
      #--Remove any columns that are all NA
      buyer.list <- buyer.list[,colSums(is.na(buyer.list)) < nrow(buyer.list)]
      
      write.csv(buyer.list,buyer.list.name,row.names = FALSE)
      
      }

}

#  Outputs each Supplier's web articles

output.supplier.list <- function() {
      
      all.suppliers <- unique(web.product.data$Supplier)
      
      for (i in 1:length(all.suppliers)) {
            
            supplier <- all.suppliers[i]
            supplier.list.name <- paste(supplier, " Web Products",".csv",sep = "")
            
            attribute.list <- c("Brand","Size","Colour","Pack Qty","Material","Assembly","Washable","Power",
                                "Capacity","Coverage","Age","Model Number")
            
            supplier.list <- subset(web.product.data,web.product.data$Supplier == supplier)
            supplier.list <- supplier.list[,c("PSA_1","PSA_2","Article","Web Description",attribute.list,"DQ Score")]
            supplier.list <- supplier.list[order(supplier.list[,1],supplier.list[,2],supplier.list[,5]),]
            
            #--Remove any columns that are all NA
            supplier.list <- supplier.list[,colSums(is.na(supplier.list)) < nrow(supplier.list)]
            
            write.csv(supplier.list,supplier.list.name,row.names = FALSE)
            
      }
      
}


#  Outputs each Brand's articles

output.brand.list <- function() {
      
      all.brands <- unique(web.product.data$Brand)
      
      for (i in 1:length(all.brands)) {
            
            brand <- all.brands[i]
            brand.list.name <- paste(brand, " Web Products",".csv",sep = "")
            
            attribute.list <- c("Brand","Size","Colour","Pack Qty","Material","Assembly","Washable","Power",
                                "Capacity","Coverage","Age","Model Number")
            
            brand.list <- subset(web.product.data,web.product.data$Brand == brand)
            brand.list <- brand.list[,c("PSA_1","PSA_2","Article","Web Description",attribute.list,"DQ Score")]
            brand.list <- brand.list[order(brand.list[,1],brand.list[,2],brand.list[,5]),]
            
            #--Remove any columns that are all NA
            brand.list <- brand.list[,colSums(is.na(brand.list)) < nrow(brand.list)]
            
            write.csv(brand.list,brand.list.name,row.names = FALSE)
            
      }
      
}

# ---------------------------------------------------------------------------------------------------------------------------------------

# Create two sets of data for cleansing
# Missing Web Descriptions by PSA1 for PDT
# Web Category Cleanse for Digital Trading

#  Outputs articles with missing web descriptions by PSA1 

output.web.description.cleanse <- function() {
      
      all.psa1 <- unique(web.product.data$PSA_1)
      
      for (i in 1:length(all.psa1)) {
            
            psa1 <- all.psa1[i]
            
            column.list <- c(1,2,4:9,12)
            
            web.description.list <- subset(web.product.data,web.product.data$'PSA_1' == psa1 & web.product.data$`Web Description 1`=="" )
            web.description.list <- web.description.list[,c("PSA_1","PSA_2","Article","Article Description","Web Description 1","Web Description 2","DQ Score")]
            web.description.list <- web.description.list[order(web.description.list[,1],web.description.list[,2],web.description.list[,4]),]
            web.description.list <- web.description.list[order(web.description.list[1]),]
            
            cases <- NROW(web.description.list)
            web.description.list.name <- paste(psa1, " Missing 100 Character Description", " - (",cases," Articles)",".csv",sep = "")
            
            write.csv(web.description.list,web.description.list.name,row.names = FALSE)
            
      }
      
}

output.web.category.cleanse <- function() {
      
      all.categories <- unique(web.product.data$`Web Category`)
      attribute.list <- c("Brand","Size","Colour","Pack Qty","Material","Assembly","Washable","Power","Capacity","Coverage","Age","Model Number")
      web.product.data <- web.product.data[order(web.product.data$'Brand'),]
      
      for (i in 1:length(all.categories)) {
            
            
            category <- all.categories[i]
            dir.create(category,showWarnings = FALSE)
            setwd(category)
            
#  Create Output file for products in category that currently score 100%
            
            web.complete <- subset(web.product.data,web.product.data$`DQ Score` == 1 & web.product.data$`Web Category` == category)
            column.list <- c("Article","Web Description",attribute.list,"DQ Score")
            web.complete <- web.complete[column.list]
            
            # Remove any columns that are all NA
            web.complete <- web.complete[,colSums(is.na(web.complete)) < nrow(web.complete)]
            
            # Create the output file
            
            cases <- NROW(web.complete)
            if(NROW(web.complete) >0) write.csv(web.complete,paste(category, " Complete"," (",cases," Articles)",".csv",sep = ""),row.names = FALSE)
            
#  Create Output file for products in category that are missing a Web Description
            
            web.missing.description <- subset(web.product.data, web.product.data$`DQ Web Description Score` == 0 & web.product.data$`Web Category` == category)
            column.list <- c("Article","Article Description","Web Description 1", "Web Description 2","DQ Score")
            web.missing.description <- web.missing.description[column.list]
            
            # Remove any columns that are all NA and sort by Brand
            cases <- NROW(web.missing.description)
            web.missing.description <- web.missing.description[,colSums(is.na(web.missing.description)) < nrow(web.missing.description)]
            if(NROW(web.missing.description) > 0) write.csv(web.missing.description,paste(category, " Missing Web Description"," (",cases," Articles)",".csv",sep = ""), row.names = FALSE)

            
#  Create Output file for products in category that are missing Product Attribute values
             
            web.missing.attribute <- subset(web.product.data, web.product.data$`DQ Attribute Score` < 0.35 & web.product.data$`Web Category` == category)
            attribute.list <- subset(web.attribute$Attribute,web.attribute$Category == category)
            column.list <- c("Article","Web Description",attribute.list,"DQ Score")
            web.missing.attribute <- web.missing.attribute[column.list]

            # Remove any columns that are all NA and sort by Brand
            cases <- NROW(web.missing.attribute)
            #web.missing.attribute <- web.missing.attribute[,colSums(is.na(web.missing.attribute)) < nrow(web.missing.attribute)]

            if(NROW(web.missing.attribute) > 0) write.csv(web.missing.attribute,paste(category, " Missing Attribute"," (",cases," Articles)",".csv",sep = ""), row.names = FALSE)

#  Create Output file for products in category that have inconsistent branding
            
            web.inconsistent.brand <- subset(web.product.data, web.product.data$`DQ Brand Consistency Score` == - 0.05 & !is.na(web.product.data$`DQ Web Description Score`) & web.product.data$`Web Category` == category)
            web.inconsistent.brand$'Web Description' <- paste(web.inconsistent.brand[,4],web.inconsistent.brand[,5], sep = "")
            column.list <- c("Article","Web Description","Brand","DQ Score")
            web.inconsistent.brand <- web.inconsistent.brand[column.list]
            
            # Remove any columns that are all NA and sort by Brand
            cases <- NROW(web.inconsistent.brand)
            #web.inconsistent.brand <- web.inconsistent.brand[,colSums(is.na(web.inconsistent.brand)) < nrow(web.inconsistent.brand)]
            
            if(NROW(web.inconsistent.brand) > 0) write.csv(web.inconsistent.brand,paste(category, " Inconsistent Brand"," (",cases," Articles)",".csv",sep = ""), row.names = FALSE)         
            
            
#  Create Output file for products in category that have a GTIN issue
            
            web.gtin.issue <- subset(web.product.data, web.product.data$`DQ GTIN Score` == - 0.05 & web.product.data$`Web Category` == category)
            column.list <- c("Article","Web Description","DQ Score")
            web.gtin.issue <- web.gtin.issue[column.list]
            
            # Remove any columns that are all NA and sort by Brand
            cases <- NROW(web.gtin.issue)
            #web.gtin.issue <- web.gtin.issue[,colSums(is.na(web.gtin.issue)) < nrow(web.gtin.issue)]
            
            if(NROW(web.gtin.issue) > 0) write.csv(web.gtin.issue,paste(category, " GTIN Issue"," (",cases," Articles)",".csv",sep = ""), row.names = FALSE)         
            
            setwd("..")

                 
      }
      
}

# ---------------------------------------------------------------------------------------------------------------------------------------

# Create summary files of DQ Score
# Grouping: PSA1, PSA2, Assistant Buyer, Web Category

dq.summarise.psa2 <- function() {
      
      psa2.score <- aggregate(web.product.data$`DQ Score`,list(web.product.data$PSA_2),mean)
      psa2.score <- cbind(psa2.score,aggregate(web.product.data$`DQ Score`,list(web.product.data$PSA_2),length))
      psa2.score <- psa2.score[,c(1,2,4)]
      colnames(psa2.score) <- c("PSA 2","Mean DQ Score", "Num Articles")
      psa2.score$'Mean DQ Score' <- signif(psa2.score$'Mean DQ Score',2) 
      psa2.score <<- psa2.score[order(-psa2.score$`Mean DQ Score`),]
      
}

#------------------------------------------------------------------------------------------------------------------

dq.summarise.psa1 <- function() {
      
      psa1.score <- aggregate(web.product.data$`DQ Score`,list(web.product.data$PSA_1),mean)
      psa1.score <- cbind(psa1.score,aggregate(web.product.data$`DQ Score`,list(web.product.data$PSA_1),length))
      psa1.score <- psa1.score[,c(1,2,4)]
      colnames(psa1.score) <- c("PSA 1","Mean DQ Score", "Num Articles")
      psa1.score$'Mean DQ Score' <- signif(psa1.score$'Mean DQ Score',2) 
      psa1.score <<- psa1.score[order(-psa1.score$`Mean DQ Score`),]
}

#------------------------------------------------------------------------------------------------------------------

dq.summarise.buyer <- function() {
      
      buyer.score <- aggregate(web.product.data$`DQ Score`,list(web.product.data$`Asst Buyer`),mean)
      buyer.score <- cbind(buyer.score,aggregate(web.product.data$`DQ Score`,list(web.product.data$`Asst Buyer`),length))
      buyer.score <- buyer.score[,c(1,2,4)]
      colnames(buyer.score) <- c("Assistant Buyer","Mean DQ Score", "Num Articles")
      buyer.score$'Mean DQ Score' <- signif(buyer.score$'Mean DQ Score',2) 
      buyer.score <<- buyer.score[order(-buyer.score$`Mean DQ Score`),]
}
#------------------------------------------------------------------------------------------------------------------

dq.summarise.web.category <- function() {
      
      web.category.score <- aggregate(web.product.data$`DQ Score`,list(web.product.data$`Web Category`),mean)
      web.category.score <- cbind(web.category.score,aggregate(web.product.data$`DQ Score`,list(web.product.data$`Web Category`),length))
      web.category.score <- web.category.score[,c(1,2,4)]
      colnames(web.category.score) <- c("Web Category","Mean DQ Score", "Num Articles")
      web.category.score$'Mean DQ Score' <- signif(web.category.score$'Mean DQ Score',2) 
      web.category.score <<- web.category.score[order(-web.category.score$`Mean DQ Score`),]
}
#------------------------------------------------------------------------------------------------------------------

dq.summarise.results <- function() {
      
      dq.summarise.psa1()
      dq.summarise.psa2()
      dq.summarise.buyer()
      dq.summarise.web.category()
      
}

report.web.product.data <- function(){
      
      report.columns <- c("Article","Article Description","Web Description","PSA_1", "PSA_2","Web Category",
                          "Asst Buyer","PDT","Web Trading","Status","Supplier","Brand","Type",
                          "DQ Brand","DQ Type","DQ Colour","DQ Size","DQ Assembly","DQ Material","DQ Pack Qty",
                          "DQ Power","DQ Age","DQ Washable","DQ Coverage","DQ Model Number","DQ Capacity",
                          "DQ Web Description Score","DQ Brand Consistency Score","DQ Attribute Score","DQ GTIN Score","DQ Score")
      
      report.data <<- web.product.data[,report.columns]
      
      # Create reporting category for Web Description
      
      report.data$`Web Missing` <<- ifelse(report.data$`DQ Web Description Score` == 0, TRUE, FALSE)
      report.data$`GTIN Issue` <<- ifelse(report.data$`DQ GTIN Score` != 0, TRUE, FALSE)
      report.data$'Attribute Completion %' <<- report.data$`DQ Attribute Score` / 0.35
      
      write.csv(report.data,"Web Product Report Data.csv", row.names = FALSE)
      
}

report.web.product.data.stamped <- function(){
      
      write.csv(report.data,paste("Web Product Report Data_",Sys.Date(),".csv",sep = ""), row.names = FALSE)
}
