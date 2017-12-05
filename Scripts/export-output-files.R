# This function exports the data quality management reports, data files for cleansing and report files
      
export.product.files <- function() {

      # Move to the Output Files Directory
      setwd("..")
      setwd("./Output Data Files")
      
# Output the lists of unmatched web skus and web skus with missing web descriptions
      
      message("Outputting Web Product File")
      setwd("./Data Processing")
      write.csv(web.product.data,"Web Product File.csv",row.names = FALSE)
      write.csv(unmatched,"Unmatched Web Articles.csv",row.names = FALSE)
      write.csv(undefined.web.category,"Undefined Web Category.csv",row.names = FALSE)
      setwd("..")
      
#---------------------------------------------------------------------------------------------------------------------------------------

# Output the product attribute lists at Asst Buyer, Supplier and Brand levels
      
      message("Outputting Buyer Lists")
      unlink("Assistant Buyer",recursive = TRUE)
      dir.create("Assistant Buyer",showWarnings = FALSE)
      setwd("./Assistant Buyer")
      output.buyer.list()
      setwd("..")
      
      message("Outputting Supplier Lists")
      unlink("Supplier",recursive = TRUE)
      dir.create("Supplier",showWarnings = FALSE)
      setwd("./Supplier")
      output.supplier.list()
      setwd("..")
      
      message("Outputting Brand Lists")
      unlink("Brand",recursive = TRUE)
      dir.create("Brand",showWarnings = FALSE)
      setwd("./Brand")
      output.brand.list()
      setwd("..")
 
#---------------------------------------------------------------------------------------------------------------------------------------

# Create data for cleansing
            
      # Output the Web Data Quality Lists for Cleansing
      
      message("Outputting Web Category Lists")
      unlink("Web Category",recursive = TRUE)
      dir.create("Web Category",showWarnings = FALSE)
      setwd("./Web Category")
      output.web.category.cleanse()
      setwd("..")

      # Output the Missing Web Descriptions at PSA 1 Level
      
      message("Outputting Missing Web Descriptions")
      unlink("Missing Web Description",recursive = TRUE)
      dir.create("Missing Web Description",showWarnings = FALSE)
      setwd("./Missing Web Description")
      output.web.description.cleanse()
      setwd("..")
      
      # Output the PSA 2 Level Files
      
      message("Outputting PSA2 Lists")
      unlink("PSA 2",recursive = TRUE)
      dir.create("PSA 2",showWarnings = FALSE)
      setwd("./PSA 2")
      output.psa2.cleanse()
      setwd("..")
      
      # Output the PSA 1 Level Files
      
      message("Outputting PSA1 Lists")
      unlink("PSA 1",recursive = TRUE)
      dir.create("PSA 1",showWarnings = FALSE)
      setwd("./PSA 1")
      output.psa1.cleanse()
      setwd("..")
#---------------------------------------------------------------------------------------------------------------------------------------
      
      # Output the Report Data File
      
      message("Outputting Report Data")
      setwd("./Report Data")
      report.web.product.data()
      setwd("./Date Stamp Files")
      report.web.product.data.stamped()
      setwd("..")
      setwd("./Attribute Completion")
      build.psa1.attribute.summary()
      build.psa2.attribute.summary()
      
# Output PSA 1 and PSA 2 files to R Drive folder
      
      setwd("R:/Data Quality Reports/Web Data Quality Reports")
      
      # PSA 1 Level Files
      
      message("Outputting PSA1 Lists to R Drive")
      unlink("PSA 1",recursive = TRUE)
      dir.create("PSA 1",showWarnings = FALSE)
      setwd("./PSA 1")
      output.psa1.cleanse()
      setwd("..")   
      
      # PSA 2 Level Files
      
      message("Outputting PSA2 Lists to R Drive")
      unlink("PSA 2",recursive = TRUE)
      dir.create("PSA 2",showWarnings = FALSE)
      setwd("./PSA 2")
      output.psa2.cleanse()
      setwd("..")
      
}