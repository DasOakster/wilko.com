build.attribute.examples <- function() { 


web.category <<- unique(web.product.data$`Web Category`)      

for(i in 1:NROW(web.category)) {

attribute.examples <<- subset(web.product.data,web.product.data$`DQ Score` == 1 & web.product.data$`Web Category` == web.category[i])

if(nrow(attribute.examples) == 3) {

      attribute.examples <<- attribute.examples[sample(nrow(attribute.examples),3),]
      attribute.list <<- c("Brand","Size","Colour","Pack Qty","Material","Assembly","Washable","Power",
                    "Capacity","Coverage","Age","Model Number")
      attribute.examples <<- attribute.examples[,attribute.list]
      attribute.examples <<- attribute.examples[,colSums(is.na(attribute.examples)) < nrow(attribute.examples)]
      attribute.examples <<- t(attribute.examples)

      colnames(attribute.examples) <<- c("Example 1", "Example 2","Example 3")
      
      path <<- "C:/Users/oakleya/Desktop/Product Guide Development/Source Data/Examples/"
      example.file <<- paste(path,web.category[i],".csv",sep = "")
      
      write.csv(attribute.examples,example.file)

}
}
}


build.attribute.semantics <- function() {
      
      source.file <- "C:/Users/oakleya/Desktop/Product Guide Development/Source Data/Web Category Attributes.csv"
      attribute.semantics <- read.csv(source.file,header = TRUE,sep = ",")
      web.category <- unique(attribute.semantics$Category)
      
      path <- "C:/Users/oakleya/Desktop/Product Guide Development/Source Data/Attribute Semantics/"
      
      for(i in 1:NROW(web.category)) {
            
            web.category.attribute <- subset(attribute.semantics,attribute.semantics$Category == web.category[i])
            category.file.name <- paste(path,web.category[i],".csv",sep = "")
            write.csv(web.category.attribute,category.file.name,row.names = FALSE)
      }

      
}

