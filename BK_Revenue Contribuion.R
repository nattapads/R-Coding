RevCon <- function(file.csv) {
        Data.df <- read.csv(file.csv)
        Data.df[,2] <- as.Date(Data.df$create_date,format='%d/%m/%Y')
        
        Contri.df <- aggregate(price ~ main_ingredient, Data.df, sum)
        RevCon.df <- data.frame(Contri.df,
                                round(100*(Contri.df$price/
                                                   sum
                                           (Contri.df$price)),digits = 3))
        colnames(RevCon.df)[1:3] <- c("Inventory","Total_Sales","Contribution")
        RevCon.df <- RevCon.df[with(RevCon.df, order(-Contribution)), ]
        RevCon.df <- data.frame(RevCon.df, cumsum(RevCon.df[,3]))
        colnames(RevCon.df)[4] <- "Cumulative"
        RevCon.df
}