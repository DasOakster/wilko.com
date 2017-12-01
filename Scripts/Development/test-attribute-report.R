psa1.all <- unique(web.product.data$PSA_1)

psa2.attribute.summary <- data.frame("PSA_1" = character(),"PSA_2" = character(),"Attribute" = character(),"Complete" = double())

attributes <- 37:49

for(i in 1:length(psa1.all)) {
      
      psa1 <- psa1.all[i]
      psa.data <- subset(web.product.data,web.product.data$PSA_1 == psa1)
      
      psa2.all <- unique(psa.data$PSA_2)
            
            
            for(j in 1:length(psa2.all)) {
                  
                  psa2 <- psa2.all[j]
                  
                  
                  for(k in 1:length(attributes)) {
                        
                        attribute <- attributes[k]
                        attribute_name <- names(web.product.data[attribute])
                        
                        dq.data <- psa.data[psa.data$PSA_2 == psa2,c(1,2,7,8,9,attribute)]
                        dq.data <- na.omit(dq.data)
                        
                        total <- NROW(dq.data)
                        completed <- sum(dq.data[,6]==1)
                        complete.ratio <- completed / total
                        
                        df <- data.frame(psa1,psa2,attribute_name,complete.ratio)
                        psa2.attribute.summary <- rbind(psa2.attribute.summary,df)
                        
                  }
                  
            }
      
      psa2.attribute.summary <- na.omit(psa2.attribute.summary)
      
}

write.csv(psa2.attribute.summary,paste("Web Product Attribute Completion PSA 2_",Sys.Date(),".csv",sep = ""), row.names = FALSE)

