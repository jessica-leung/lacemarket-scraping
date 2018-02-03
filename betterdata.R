
library(ggplot2)

# Getting additional information on the dress from Lolibrary

data <- read.csv("sold_dresses.csv")
head(data)
full_data <- na.omit(data)
head(full_data)
df <- data.frame(matrix(ncol = 18, nrow = 0))
colnames(df) <- c("X", "Name", "Resale", "Lolibrary", "Brand","Type","Year","Product Number", "Colors","Features", "Bust", "Length", "Waist", "Shoulder Width", "Sleeve Length", "Other Notes", "Submitter", "Tags")

for (i in 1:nrow(full_data)){
	lolibrary_url <- full_data[i,"Lolibrary"]
	out <- tryCatch(readLines(lolibrary_url), error = function(e) e)
	if (!(any(class(out) == "error"))){
		lolibrary <- read_html(lolibrary_url)
		name <- lolibrary %>% html_nodes(".name")
		description <- lolibrary %>% html_nodes(".description") 
		df[i,1:4] <- full_data[i,1:4]
		for (j in 1:length(name)){
			df[i,trimws(name[j]%>%html_text())] <- trimws(description[j]%>%html_text())
		}
	}
   }

# Cleaning up data
df[c('Owner Waist', 'Owner Bust', 'Owner Notes', 'Owner Underbust')] <- NULL

df['Currency'] <- NA
df$Currency[grepl("€", df$Resale)] <- "EUR"
df$Currency[grepl("$", df$Resale)] <- "USD"
df$Currency[grepl("£", df$Resale)] <- "GBP"
df$Resale <- gsub("[^0-9]", "", df$Resale)

df <- subset(df, df$Currency == "USD" & !is.na(df$Price))

write.csv(df, file = "sold_dresses_2.csv")

# Plot a bar chart
ggplot(df, aes(Brand)) + geom_bar(aes(fill=Brand)) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.5)) + ggtitle("Number of Secondhand Sales") +   xlab("Brand")

# Plot a color-coded scatterplot by brand
ggplot(df, aes(as.numeric(Price), as.numeric(Resale), color=Brand)) + geom_point() + ggtitle("Original vs Resale Price") + xlab("Original (JPY)") +   ylab("Resale (USD)") +theme(plot.title = element_text(hjust = 0.5))

# Plot a scatterplot and draw a regression line
ggplot(ans, aes(as.numeric(Price), as.numeric(Resale))) + geom_point() + ggtitle("Original vs Resale Price") + xlab("Original (JPY)") +   ylab("Resale (USD)") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method = "lm", se = FALSE)