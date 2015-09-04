#helper functions for ratlantis

#' "%!in%" function
#'
#' This function allows you to check if an element is not in a larger group
#' @param x object you are trying to check for
#' @param table group you are checking against
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0


#' meannona function
#'
#' This function allows you to calculate the mean value of a set while excluuding
#' NA's
#' @param ... list for which you seeking a mean
meannona=function(...){
  mean(..., na.rm=T)

}

#' maxnona function
#'
#' This function allows you to calculate the max value of a set while excluuding
#' NA's
#' @param ... list for which you seeking a maximum
maxnona=function(...){
  max(..., na.rm=T)
}

#' minnona function
#'
#' This function allows you to calculate the mean value of a set while excluuding
#' NA's
#' @param ... list for which you seeking a minimum
minnona=function(...){
  min(..., na.rm=T)
}




#Below are modified files from Jorge Cornejo for accessing fishbase

fb_growth <-
  function(idFB = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.org/')
  {
    url <- paste(server, "PopDyn/PopGrowthList.php?ID=", idFB, sep = "")
    g <- XML::readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
    if (length(g) == 0)
    {
      gu <- c(as.character(idFB), as.character(Genus), as.character(Species),t(rep(NA, 13)))
      names(gu) <-   c('idFB', 'Genus', 'Species', 'Linf', 'LengthType', 'k', 't0', 'sex',
                       'M', 'Temp', 'Lm', 'phi', 'Country', 'Locality', 'Questionable',
                       'Captive')
      gu <- t(gu)
    }
    if (!is.null(g$dataTable))
    {
      gu <- cbind(as.character(idFB), as.character(Genus), as.character(Species),g$dataTable[,-1])
      names(gu) <-  c('idFB', 'Genus', 'Species', 'Linf', 'LengthType', 'k', 't0', 'sex',
                      'M', 'Temp', 'Lm', 'phi', 'Country', 'Locality', 'Questionable',
                      'Captive')
      gu$idFB <- as.character(gu$idFB)
      gu$Genus <- as.character(gu$Genus)
      gu$Species <- as.character(gu$Species)
      gu$Linf <- as.character(gu$Linf)
      gu$LengthType <- as.character(gu$LengthType)
      gu$k <- as.character(gu$k)
      gu$t0 <- as.character(gu$t0)
      gu$sex <- as.character(gu$sex)
      gu$M <- as.character(gu$M)
      gu$Temp <- as.character(gu$Temp)
      gu$Lm <- as.character(gu$Lm)
      gu$phi <- as.character(gu$phi)
      gu$Country <- as.character(gu$Country)
      gu$Locality <- as.character(gu$Locality)
      gu$Questionable <- as.character(gu$Questionable)
      gu$Captive <- as.character(gu$Captive)
    }

    return(data.frame(gu))
  }


#changed from Jorge's original by adding option to check for NULL response
fb_lengthweight <-
  function(idFB = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.us/')
  {

      url <- paste(server, "PopDyn/LWRelationshipList.php?ID=", idFB, sep = "")
      lw <- XML::readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
      if(is.null(dim(lw[[3]][-1,]))){
        lwu <- data.frame(idFB = as.character(idFB), Genus = as.character(Genus), Species = as.character(Species),
                          Score = NA, a = NA, b = NA, Doubful = NA, sex = NA, Length = NA, L_type = NA,
                          r2 = NA, SD_b = NA, SD_log10_a = NA, n = NA, Country = NA, Locality = NA, URL=url)
      }else if (dim(lw[[3]][-1,])[1] == 0)
      {
        lwu <- data.frame(idFB = as.character(idFB), Genus = as.character(Genus), Species = as.character(Species),
                          Score = NA, a = NA, b = NA, Doubful = NA, sex = NA, Length = NA, L_type = NA,
                          r2 = NA, SD_b = NA, SD_log10_a = NA, n = NA, Country = NA, Locality = NA, URL=url)
        #t(rep(NA, 13)))
        #names(lwu) <-   c('idFB','Genus', 'Species',  'Score', 'a', 'b', 'Doubful', 'sex', 'Length', 'L_type',
        #                  'r2', 'SD_b', 'SD_log10_a', 'n','Country', 'Locality')

      } else if (dim(lw[[3]][-1,])[1] != 0)
      {
        lwu <- cbind(as.character(idFB), as.character(Genus), as.character(Species), lw[[3]][-1,], URL=url)
        names(lwu) <-   c('idFB','Genus', 'Species',  'Score', 'a', 'b', 'Doubful', 'sex', 'Length', 'L_type',
                          'r2', 'SD_b', 'SD_log10_a', 'n','Country', 'Locality', 'URL')
      }
      lwu <- (lwu[,-4])

    return(as.data.frame(lwu))
  }


  fb_tl2 <-
    function(idFB = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.us/')
    {

      tl <- data.frame(idFB = idFB, Genus = Genus, Species = Species, tl = NA, tl_se = NA, BasedOn = NA)
      url <- NA;
      if (!is.na(Genus) & !is.na(Species))
      {
        url <- paste(server, "Summary/",Genus,"-",Species, ".html", sep = "")
      }
      if (!is.na(url))
      {
        g <- XML::htmlParse(url)
        g2 <- XML::toString.XMLNode(g)
        g3 <- toString(stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(g2, "\t", ""), "\n", ""), "\r", ""))
        g4 <- strsplit(g3, "<!-- Start Trophic Level -->")[[1]][2]
        g5 <- strsplit(g4,"se;")[[1]][1]
        BasedOn <- strsplit(g4,"se;")[[1]][2]
        BasedOn <- strsplit(BasedOn,"</div>")[[1]][1]
        g5.1 <- strsplit(g5,"</a>):")[[1]][2]
        g5.2 <- strsplit(g5.1," se;")[[1]][1]
        g6 <- stringr::str_replace_all(g5.2, "\U3e30613c", "")
        g6 <- stringr::str_replace_all(g6, "\U3e31623c", "")
        g6 <- strsplit(g6, " ")
        tl$tl <- as.numeric(gsub("[[:alpha:]]", "", g6[[1]][1]))
        tl$tl_se <- gsub("[[:alpha:]]", "",g6[[1]][3])
        tl$BasedOn <- as.character(BasedOn)
      }
      return(tl)
    }

  fb_maturity <-
    function(idFB = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.org/')
    {
      url <- paste(server, "Reproduction/MaturityList.php?ID=", idFB, sep = "")
      Mat <- XML::readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
      if (length(Mat)==0 | is.null(dim(Mat$dataTable)) )
      {
        Maturity <- data.frame(idFB = idFB, Genus = Genus, Species = Species, na0 = NA, lm = NA,
                               length_min = NA, na1 = NA, length_max = NA, Age_min = NA, na2 = NA,
                               Age_max = NA, tm = NA, Sex = NA, Country = NA, Locality = NA)
        #Maturity <- t(Maturity)
      } else
      {
        Maturity <- cbind(as.numeric(as.character(idFB)),  Mat$dataTable)
        names(Maturity) <- c('idFB', 'na0', 'lm', 'length_min', 'na1', 'length_max', 'Age_min', 'na2', 'Age_max', 'tm', 'Sex', 'Country', 'Locality')
      }
      Maturity <- Maturity[, names(Maturity) %in% c('idFB', 'Genus', 'Species', 'lm', 'length_min', 'length_max',
                                                    'Age_min', 'Age_max', 'tm', 'Sex', 'Country', 'Locality')]
      return(as.data.frame(Maturity))
    }
