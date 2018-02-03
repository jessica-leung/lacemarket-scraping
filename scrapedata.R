
library(rvest)
options(stringsAsFactors=F)

# Getting sales from Lacemarket

reader <- function(page) {
    	page_url <- read_html(url(page))
    	links <- parg_url %>% html_nodes("a")
		lolibrary <- NA
		for (i in 1:length(links)){
			if (links[i] %>% html_text == "Lolibrary.Org"){
				lolibrary <- links[i] %>% html_attr("href")
			}
		}
    	data.frame(Name=doc %>% html_node(".auction-title") %>% html_text(),
        	Price=doc %>% html_node(".auction-price") %>% html_node("h3") %>% html_text(),
        	lolibrary)
	}

for (i in 1:99){
	mainurl <- paste("https://egl.circlly.com/auctions/search?q[category_id]=12&q[auction_status]=ended&page=",i,sep="")
	site <- read_html(mainurl)
	article.links <- site %>% html_nodes(".auction-title") %>% html_attr("href")
	temp_df <- do.call(rbind, lapply(article.links, reader))
	df <- rbind(df, temp_df)
}

write.csv(df, file = "sold_dresses.csv")