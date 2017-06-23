RevCon <- function(directory) {
        
        # Import and merge file.csv from directory
        files_list <- list.files(directory, full.names=TRUE)
        no_of_files <- length(files_list)
        Data.df <- data.frame()
        for(h in 1:no_of_files) {
                Data.df <- rbind(Data.df, read.csv(files_list[h]))
        }
        
        # Change create_date to date format
        Data.df[,2] <- as.Date(Data.df$create_date,format='%d/%m/%Y')
        
        # Aggregate sales by invntories
        Contri.df <- aggregate(price ~ main_ingredient, Data.df, sum)
        
        # Calculate contribution in percentage
        RevCon.df <- data.frame(Contri.df,
                                round(100*(Contri.df$price/
                                        sum(Contri.df$price)),digits = 3))
        colnames(RevCon.df)[1:3] <- c("Inventory","Total_Sales","Contribution")
        
        # Sort contribution and calculate cumulative
        RevCon.df <- RevCon.df[with(RevCon.df, order(-Contribution)), ]
        RevCon.df <- data.frame(RevCon.df, cumsum(RevCon.df[,3]))
        colnames(RevCon.df)[4] <- "Cumulative"
        
        # Print results
        RevCon.df
}

## Test
RevCon("BK")