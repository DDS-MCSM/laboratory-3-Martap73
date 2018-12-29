#******************************************************************************#
#                                                                              #
#                    Lab 3 - Data Acquisition & Analysis                       #
#                                                                              #
#                     Your Name - Data Driven Securty                          #
#                                                                              #
#******************************************************************************#

## Crawling y Scrapping
require(rvest)
require(urltools)
require(curl)
require(httr)
require(plyr)
require(plotly)
require(xml2)
require(XML)
require(dplyr)

### 1.1 Obtención de la página web
Obtener_pagina_web <- function(){
  url.wiki <- "https://www.mediawiki.org/wiki/MediaWiki"
  tmp <- read_html(url.wiki)
  return (tmp)
  #tmp <- html_nodes(tmp, "table")
}

### 1.2 Analisis de el contenido de la web
Obtener_titulo<-function(x){
  node_title <- html_node(x, xpath = '//title')
  title <- html_text(node_title)
  return (title)
}

Obtener_dominio<-function(){
  url.wiki <- "https://www.mediawiki.org/wiki/MediaWiki"
  dominio<-domain(url.wiki)
  return (dominio)
}


### 1.3.	Extracción de enlaces
Obtener_links<-function(url){

  links <- url %>% html_nodes("a") %>% html_attr("href")
  links_text<-url%>% html_nodes("a") %>% html_text
  df<-do.call(rbind,
              lapply(1:length(links),
                     function(i)
                       data.frame(URL=unlist(links[i]),
                                  ENLACE=unlist(links_text[i]))))

  str_to_find = "/"
  v<-substring(df$URL, 1, nchar(str_to_find)) == str_to_find
  final.filtered <- df[v,]
  str="https"
  s<-substring(df$URL, 1, nchar(str)) == str
  f<-df[s,]
  final.filtered<-rbind(final.filtered,df[s,])
  row.has.na <- apply(final.filtered, 1, function(x){any(is.na(x))})
  sum(row.has.na)
  final.filtered <- final.filtered[!row.has.na,]
  return (final.filtered)

}

### 1.4 Exploración de enlaces
Obtener_URL_completas<-function(df)
{
  str_to_find = "/"
  records <- vector("list", length = length(df$URL))

  for (i in 1:nrow(df))
  {
    row <- df[i,1]
    #print("A")
    #print(i)
    #print(row)
    true<-(substring(row, 1, nchar(str_to_find)) == str_to_find)
    #print(true)
    if (true==TRUE)
    {
      #print(i)
      url<-paste(dominio,row,"/",sep="")
      #print(url)
    }
    if (true==FALSE)
    {
      url<-paste("",row,"/",sep="")
      #print(url)
    }

    records[(i)] <- url
  }

  return (records)
}


Explorar_enlaces<-function(x)
{

  rec <- vector("list", length = length(x))
  name=""
  estado=""
  y<-unlist(x)
  for (i in 1:length(x))
  {
    status<-HEAD(y[i])

    Sys.sleep(1)
    estado=status_code(status)
    name=y[i]

    str_to_find="http"
    true<-(substring(name, 1, nchar(str_to_find)) == str_to_find)

    if (true==TRUE)
    {
      valor="absoluto"
    }
    if (true==FALSE)
    {
      valor="relativo"
    }

    rec[[i]] <- data_frame(URL=name, ESTADO=estado,VALOR=valor)


  }


  df<-bind_rows((rec))
  return (df)
}




### Gráficos en R

### 2.1 Histograma

Plot_histogram<-function(x)
{
  counts <- ddply(x, .(x$URL, x$VALOR), nrow)
  names(counts) <- c("y", "m", "Freq")

  URL=counts$y
  COUNT=counts$Freq
  VALOR=counts$m
  plot <- ggplot(counts, aes(URL, COUNT,fill=VALOR))

  plot<-plot + geom_histogram(stat="identity", binwidth = 1) +
    labs(title="URL Count",
         subtitle="count URL",
         y="Count", x="URLs") +
    theme(legend.position="bottom",legend.title = element_blank(),axis.text.x = element_blank())+
    facet_grid(~counts$m, scales="free", space="free")

  plot<-ggplotly(plot)
  return (plot)
}

### 2.2 Un gráfico de barras


Plot_bar<-function(x)
{
 # x<-url_status
  count_relative<-sum( x$VALOR == "relativo" )
  counts_absolute<-sum(x$VALOR=="absoluto")

  str_to_find="https://www.mediawiki.org"
  counts_wiki<-sum(x$VALOR=="absoluto" & (substring(x$URL, 1, nchar(str_to_find)) == str_to_find))

  total_wiki<-count_relative+counts_wiki
  total_no_wiki<-counts_absolute-counts_wiki

  bar<-c(total_no_wiki,total_wiki)

  barp<-barplot(bar,
          main = "URLs WIKI vs NO_WIKI",
          xlab = "Tipo",
          ylab = "Suma",
          names.arg = c("NO_WIKI", "WIKI"),col = c("red","green"))

  legend("topleft",
         c("NO_WIKI","WIKI"),
         fill = c("red","green"))

  #return(barp)
}


### 2.3 Pie Chart

Plot_pie<-function(x)
{
  #x<-url_status
  table(x$ESTADO)
  df<-data.frame(table(x$ESTADO))
  pct <- round(df$Freq/sum(df$Freq)*100)

  # 3D Exploded Pie Chart
  library(plotrix)
  slices <- c(10, 12, 4, 16, 8)
  lbls <- c("US", "UK", "Australia", "Germany", "France")
  #pie3D(pct,labels=df$Var1,explode=0.3,
  #      main="Pie Chart of Countries ")
  lbls <- paste(df$Var1, pct) # add percents to labels
  lbls <- paste(lbls,"%",sep="") # ad % to labels
  pie(pct,labels = lbls, col=rainbow(length(df$Var1)),
      main="Porcentaje ResponseCode")

}


#url<-Obtener_pagina_web()
#titulo<-Obtener_titulo(url)
#dominio<-Obtener_dominio()
#df<-Obtencion_links()
#list_URL<-Obtener_URL_completas(df)
#url_status<-Explorar_enlaces(list_URL)
#p<-Plot_histogram(url_status)
#p
#b<-Plot_bar(url_status)
#b
#q<-Plot_pie(url_status)



