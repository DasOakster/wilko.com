score.web.attribute <- function(attribute.weight) {
      
      # Work out how many attributes are required for the Article
      # The DQ Attribute fields have already been set to either 1: Populated, 0: Empty, NA: Not required
      # Divides the attribute weight by the number of attributes required to calculate a score for each individual attribute
      
      dq.attribute.fields <- c(37:49)
      
      attribute.required <- rowSums(!is.na(web.product.data[dq.attribute.fields]))
      
      attribute.actual <- rowSums(web.product.data[,dq.attribute.fields],na.rm = TRUE)
      
      attribute.value <- (attribute.weight / attribute.required) 
      attribute.score <- attribute.actual * attribute.value
      
      web.product.data$'DQ Attribute Score' <<- attribute.score
      
}

score.web.description <- function(description.weight) {
      
      # Check 100 Character Field is populated
      
      web.product.data$'DQ Web Description Score' <<- 0
      web.product.data$'DQ Web Description Score'[web.product.data$`Web Description 1`!=""] <<- description.weight      
      
}

score.brand.consistency <- function(brand.weight){
      
# Checks if the brand value in the attribute field appears in the product title
      
      # Create column
      
      web.product.data$`DQ Brand Consistency Score` <<- 0
      
      # Loop through web.product.data
      
      for(i in 1:NROW(web.product.data)) {
            
            # Check if brand is NA or wb description is missing
            
            if(is.na(web.product.data$`Brand`[i]) | web.product.data$`Web Description 1`[i] == "") {
                  
                  web.product.data$`DQ Brand Consistency Score`[i] <<- 0
            }
            
            # Convert both attribute and title to uppercase (test is case sensitive) and compare
            
            else {
                  
                  if(grepl(toupper(web.product.data$Brand[i]),toupper(web.product.data$`Web Description 1`[i])) == FALSE) {
                        
                        web.product.data$`DQ Brand Consistency Score`[i] <<- brand.weight
                  }
                  
            }      
            
      }
}

score.gtin.issue <- function(gtin.weight) {
      
      web.product.data$'DQ GTIN Score' <<- 0
      web.product.data$'DQ GTIN Score'[(web.product.data$Article %in% gtin.issue$Article)] <<- gtin.weight
      
}

#-----------------------------------------------------------------------------------------------------------------------------------------

score.data.quality <- function() {

# Run the Score functions
      
      score.web.description(0.65)
      score.web.attribute(0.35)
      score.brand.consistency(-0.05)
      score.gtin.issue(-0.05)
      
# Set Score field to Zero
      
      web.product.data$'DQ Score' <- 0

# Add the two components of the score together and round to 1 decimal place
      
      dq.score.columns <- c("DQ Web Description Score","DQ Attribute Score","DQ Brand Consistency Score","DQ GTIN Score")
      
# Sum the rows, round to 2 dp and multiply by 10 to get a score out of 100
      
      web.product.data$`DQ Score` <<- rowSums(web.product.data[,dq.score.columns],na.rm = TRUE)
      #web.product.data$`DQ Score` <- signif(web.product.data$`DQ Score`,2)
      #web.product.data$`DQ Score` <<- web.product.data$`DQ Score` * 10
}



