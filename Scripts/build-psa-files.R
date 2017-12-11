output.psa2.cleanse <- function() {
      
      all.psa1 <- unique(web.product.data$`PSA_1`)
      attribute.list <- c("Brand","Size","Colour","Pack Qty","Material","Assembly","Washable","Power","Capacity","Coverage","Age","Model Number")
      web.product.data <- web.product.data[order(web.product.data$'Brand'),]
      
      for (i in 1:length(all.psa1)) {
            
            
            psa1 <- all.psa1[i]
            dir.create(psa1,showWarnings = FALSE)
            setwd(psa1)
            
            all.psa2 <- unique(web.product.data$PSA_2[web.product.data$PSA_1 == psa1])
            
            for (j in 1:length(all.psa2)) {
                  
                        psa2 <- all.psa2[j]
                        dir.create(psa2,showWarnings = FALSE)
                        setwd(psa2)
            
                        #  Create Output file for products in psa2 that currently score 100%
                        
                        web.complete <- subset(web.product.data,web.product.data$`DQ Score` == 1 & web.product.data$`PSA_2` == psa2)
                        column.list <- c("Article","Web Description",attribute.list,"DQ Score")
                        web.complete <- web.complete[column.list]
                        
                        # Remove any columns that are all NA
                        web.complete <- web.complete[,colSums(is.na(web.complete)) < nrow(web.complete)]
                        
                        # Create the output file
                        
                        cases <- NROW(web.complete)
                        if(NROW(web.complete) >0) write.csv(web.complete,paste(psa2, " Complete"," (",cases," Articles)",".csv",sep = ""),row.names = FALSE)
                        
                        #  Create Output file for products in psa2 that are missing a Web Description
                        
                        web.missing.description <- subset(web.product.data, web.product.data$`DQ Web Description Score` == 0 & web.product.data$`PSA_2` == psa2)
                        column.list <- c("Article","Article Description","Web Description 1", "Web Description 2","DQ Score")
                        web.missing.description <- web.missing.description[column.list]
                        
                        # Remove any columns that are all NA and sort by Brand
                        cases <- NROW(web.missing.description)
                        web.missing.description <- web.missing.description[,colSums(is.na(web.missing.description)) < nrow(web.missing.description)]
                        if(NROW(web.missing.description) > 0) write.csv(web.missing.description,paste(psa2, " Missing Description"," (",cases," Articles)",".csv",sep = ""), row.names = FALSE)
                        
                        
                        #  Create Output file for products in psa2 that are missing Product Attribute values
                        
                        web.missing.attribute <- subset(web.product.data, web.product.data$`DQ Attribute Score` < 0.35 & web.product.data$`PSA_2` == psa2)
                        attribute.list <- unique(subset(web.attribute$Attribute,web.attribute$'PSA_2' == psa2))
                        column.list <- c("Article","Web Description",attribute.list,"DQ Score")
                        web.missing.attribute <- web.missing.attribute[column.list]
                        
                        # Remove any columns that are all NA and sort by Brand
                        cases <- NROW(web.missing.attribute)
                        #web.missing.attribute <- web.missing.attribute[,colSums(is.na(web.missing.attribute)) < nrow(web.missing.attribute)]
                        
                        if(NROW(web.missing.attribute) > 0) write.csv(web.missing.attribute,paste(psa2, " Missing Attribute"," (",cases," Articles)",".csv",sep = ""), row.names = FALSE)
                        
                        #  Create Output file for products in psa2 that have inconsistent branding
                        
                        web.inconsistent.brand <- subset(web.product.data, web.product.data$`DQ Brand Consistency Score` == - 0.05 & !is.na(web.product.data$`DQ Web Description Score`) & web.product.data$`PSA_2` == psa2)
                        web.inconsistent.brand$'Web Description' <- paste(web.inconsistent.brand[,4],web.inconsistent.brand[,5], sep = "")
                        column.list <- c("Article","Web Description","Brand","DQ Score")
                        web.inconsistent.brand <- web.inconsistent.brand[column.list]
                        
                        # Remove any columns that are all NA and sort by Brand
                        cases <- NROW(web.inconsistent.brand)
                        #web.inconsistent.brand <- web.inconsistent.brand[,colSums(is.na(web.inconsistent.brand)) < nrow(web.inconsistent.brand)]
                        
                        if(NROW(web.inconsistent.brand) > 0) write.csv(web.inconsistent.brand,paste(psa2, " Inconsistent Brand"," (",cases," Articles)",".csv",sep = ""), row.names = FALSE)         
                        
                        
                        #  Create Output file for products in psa2 that have a GTIN issue
                        
                        web.gtin.issue <- subset(web.product.data, web.product.data$`DQ GTIN Score` == - 0.05 & web.product.data$`PSA_2` == psa2)
                        column.list <- c("Article","Web Description","DQ Score")
                        web.gtin.issue <- web.gtin.issue[column.list]
                        
                        # Remove any columns that are all NA and sort by Brand
                        cases <- NROW(web.gtin.issue)
                        #web.gtin.issue <- web.gtin.issue[,colSums(is.na(web.gtin.issue)) < nrow(web.gtin.issue)]
                        
                        if(NROW(web.gtin.issue) > 0) write.csv(web.gtin.issue,paste(psa2, " GTIN Issue"," (",cases," Articles)",".csv",sep = ""), row.names = FALSE)         
                        
                        setwd("..")
            
                  }
      
                        setwd("..")
            }
      
}




output.psa1.cleanse <- function() {
      
      all.psa1 <- unique(web.product.data$`PSA_1`)
      full.attribute.list <- c("Brand","Height","Width","Depth","Type","Size","Colour","Pack Qty","Material","Assembly","Washable","Power","Capacity","Coverage","Age","Model Number")
      web.product.data <- web.product.data[order(web.product.data$'Brand'),]
      
      for (i in 1:length(all.psa1)) {
            
            
            psa1 <- all.psa1[i]
            #psawd <- paste('All ',psa1)
            dir.create(psa1,showWarnings = FALSE)
            setwd(psa1)
            
            
            #  Create Output file for all products in psa1
            
            web.all <- subset(web.product.data,web.product.data$`PSA_1` == psa1)
            column.list <- c("PSA_1","PSA_2","Article","Web Description","Supplier",full.attribute.list,"DQ Score")
            web.all <- web.all[column.list]
            
            # Create the output file
            
            cases <- NROW(web.all)
            if(NROW(web.all) >0) write.csv(web.all[order(web.all$PSA_2),],paste(psa1," All "," (",cases," Articles)",".csv",sep = ""),row.names = FALSE)           
            
            
            #  Create Output file for products in psa1 that currently score 100%
            
            web.complete <- subset(web.product.data,web.product.data$`DQ Score` == 1 & web.product.data$`PSA_1` == psa1)
            attribute.list <- unique(subset(web.attribute$Attribute,web.attribute$'PSA_1' == psa1))
            olumn.list <- c("PSA_1","PSA_2","Article","Web Description",attribute.list,"DQ Score")
            web.complete <- web.complete[column.list]
            
            # Remove any columns that are all NA
            web.complete <- web.complete[,colSums(is.na(web.complete)) < nrow(web.complete)]
            
            # Create the output file
            
            cases <- NROW(web.complete)
            if(NROW(web.complete) >0) write.csv(web.complete[order(web.complete$PSA_2),],paste(psa1," Completed Attributes"," (",cases," Articles)",".csv",sep = ""),row.names = FALSE)
            
            #  Create Output file for products in psa1 that are missing a Web Description
            
            web.missing.description <- subset(web.product.data, web.product.data$`DQ Web Description Score` == 0 & web.product.data$`PSA_1` == psa1)
            column.list <- c("PSA_1","PSA_2","Article","Article Description","Web Description 1", "Web Description 2","DQ Score")
            web.missing.description <- web.missing.description[column.list]
            
            # Remove any columns that are all NA and sort by Brand
            cases <- NROW(web.missing.description)
            web.missing.description <- web.missing.description[,colSums(is.na(web.missing.description)) < nrow(web.missing.description)]
            if(NROW(web.missing.description) > 0) write.csv(web.missing.description[order(web.missing.description$PSA_2),],paste(psa1," Missing Description"," (",cases," Articles)",".csv",sep = ""), row.names = FALSE)
            
            
            #  Create Output file for products in psa1 that are missing Product Attribute values
            
            web.missing.attribute <- subset(web.product.data, web.product.data$`DQ Attribute Score` < 0.35 & web.product.data$`PSA_1` == psa1)
            attribute.list <- unique(subset(web.attribute$Attribute,web.attribute$'PSA_1' == psa1))
            column.list <- c("PSA_1","PSA_2","Article","Web Description",attribute.list,"DQ Score")
            web.missing.attribute <- web.missing.attribute[column.list]
            
            # Remove any columns that are all NA and sort by Brand
            cases <- NROW(web.missing.attribute)
            #web.missing.attribute <- web.missing.attribute[,colSums(is.na(web.missing.attribute)) < nrow(web.missing.attribute)]
            
            if(NROW(web.missing.attribute) > 0) write.csv(web.missing.attribute[order(web.missing.attribute$PSA_2),],paste(psa1, " Missing Attribute"," (",cases," Articles)",".csv",sep = ""), row.names = FALSE)
            
            #  Create Output file for products in psa1 that have inconsistent branding
            
            web.inconsistent.brand <- subset(web.product.data, web.product.data$`DQ Brand Consistency Score` == - 0.05 & !is.na(web.product.data$`DQ Web Description Score`) & web.product.data$`PSA_1` == psa1)
            web.inconsistent.brand$'Web Description' <- paste(web.inconsistent.brand[,4],web.inconsistent.brand[,5], sep = "")
            column.list <- c("PSA_1","PSA_2","Article","Web Description","Brand","DQ Score")
            web.inconsistent.brand <- web.inconsistent.brand[column.list]
            
            # Remove any columns that are all NA and sort by Brand
            cases <- NROW(web.inconsistent.brand)
            #web.inconsistent.brand <- web.inconsistent.brand[,colSums(is.na(web.inconsistent.brand)) < nrow(web.inconsistent.brand)]
            
            if(NROW(web.inconsistent.brand) > 0) write.csv(web.inconsistent.brand[order(web.inconsistent.brand$PSA_2),],paste(psa1, " Inconsistent Brand"," (",cases," Articles)",".csv",sep = ""), row.names = FALSE)         
            
            
            #  Create Output file for products in psa1 that have a GTIN issue
            
            web.gtin.issue <- subset(web.product.data, web.product.data$`DQ GTIN Score` == - 0.05 & web.product.data$`PSA_1` == psa1)
            column.list <- c("PSA_1","PSA_2","Article","Web Description","DQ Score")
            web.gtin.issue <- web.gtin.issue[column.list]
            
            # Remove any columns that are all NA and sort by Brand
            cases <- NROW(web.gtin.issue)
            #web.gtin.issue <- web.gtin.issue[,colSums(is.na(web.gtin.issue)) < nrow(web.gtin.issue)]
            
            if(NROW(web.gtin.issue) > 0) write.csv(web.gtin.issue[order(web.gtin.issue$PSA_2),],paste(psa1, " GTIN Issue"," (",cases," Articles)",".csv",sep = ""), row.names = FALSE)         
            
            setwd("..")
            
      }
      
}