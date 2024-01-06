########################
##### LOAN CONTROL #####
########################

##### PDF UPLOAD #####

#### Memory and other Options

memory.limit(500000)
options(scipen=999)
options(digits=10)

#### Libraries

library(pdftools)
library(tm)
library(tidyverse)
library(dplyr)
library(data.table)
library(readr)
library(zoo)
library(tidyr)
library(readr)

#### Import PDFs

# GL272D2 <- pdftools::pdf_text
# R0250_005 <- pdftools::pdf_text
# R6009 <- pdftools::pdf_text
# R6050 <- pdftools::pdf_text

## NOTE - Automate - set wd for location of daily PDFs from ImageCenter
#getwd()
#setwd("C:/Users/vend-jramos/Desktop/GL_BALANCING") # replace with public pathfile and paste("path",current date)

# Get the desktop path for the current user
#change1
# desktop_path <- normalizePath("~/Desktop", mustWork = TRUE)


# Path to the 'GL_BALANCING' folder on the desktop
gl_balancing_path <- file.path('/app', "GL_BALANCING")

setwd(gl_balancing_path)

files <- list.files(pattern = "pdf$")
files # files[1], files[2], etc..
loanctrl <- lapply(files,pdf_text)
length(loanctrl) # list object for the 4 PDF files
lapply(loanctrl, length) # how many pages each doc has
loanctrl[[1]][1] # view first page of GL

#### Import Rec (prior day)

read.xlsx # include path

#### Text mine PDFs
  ## [1] GL [2] OLD [3] REJECTS [4] DAILY JOURNAL

corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))

rowsGL <- scan(textConnection(loanctrl[[1]]), 
           what="character", sep = "\n")
rowsOLD <- scan(textConnection(loanctrl[[2]]), 
               what="character", sep = "\n")
rowsREJ <- scan(textConnection(loanctrl[[3]]), 
               what="character", sep = "\n")
rowsDTJ <- scan(textConnection(loanctrl[[4]]), 
               what="character", sep = "\n")

#### Restructure PDF into data frame

### GL ###

  ## Several data frames per PDF:
    ## Account/Account# and Dept/Dept# for all accounts in PDF
    ## Balance data for each account (list of data frames?)
    ## Subtotals/totals for all accounts in PDF
    ## OR append df with columns for acct/dept and keep totals/subtotals separate and arranged similarly?

rowsGLdat <- as.data.frame(rowsGL)

# This is a specific row removal case based on this specific GL, will need to come up with a way to automate cleaning based on what row starts with
rowsGLdatHead <- rowsGLdat[c(1:5, 49:54,88:93,98),] # Header
rowsGLdatMisc <- rowsGLdat[c(7:8,45:48,54:57,61:64,71:74,78:81,84:87,95:98),] # Account, dept, subtotals, totals; last row top report end balance
rowsGLdat <- rowsGLdat[-c(1:5, 49:54,88:93,98),]

# If row starts with date then hang onto it
is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = '%m/%d/%Y'))
rowsGLdatcorp <- as.data.frame(rowsGLdat[sapply(rowsGLdat, is.convertible.to.date)])
colnames(rowsGLdatcorp) <- "ChrColname"

######
## CHICKEN SCRATCH ##
# Data still too messy for this, may have to introduce more manual steps first
rowsGLdat2 <- rowsGLdatcorp %>% separate(ChrColname, 
                                         c('ADB DATE',
                                           'POSTED DATE',
                                           'JOURNAL ID',
                                           'ITEM NUMBER',
                                           'TRANS DESCRIPTION', # includes LF/DP/FM1...   039##... etc
                                           'DEBITS',
                                           'CREDITS'), 
                                         sep="t")

trimws(rowsGLdatcorp$ChrColname[1]) # Working body of text, first row
gsub( " .*$", "", trimws(rowsGLdatcorp$ChrColname[1])) # only provides first instance of string, want all
sapply(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+), "[", 1) # same as above
unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+)) # GOOD, we want to combine all strings after 4 and before the last 2
unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+))[-c(1:4,
                                                                 length(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+))),
                                                                 length(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+)))-1)] # GOOD, paste0

# WORKS - base case
rowsGLdat2r1 <- as.data.frame(cbind(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+))[1],
                                    unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+))[2],
                                    unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+))[3],
                                    unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+))[4],
                                    str_c(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+))[-c(1:4,
                                                                                                           length(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+))),
                                                                                                           length(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+)))-1)],
                                          collapse=" "),
                                    unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+))[length(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+)))-1],
                                    unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+))[length(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[1]), \\s+)))]
))
colnames(rowsGLdat2r1) <- c('ADB DATE',
                            'POSTED DATE',
                            'JOURNAL ID',
                            'ITEM NUMBER',
                            'TRANS DESCRIPTION', # includes LF/DP/FM1...   039##... etc
                            'DEBITS',
                            'CREDITS')
######

# Loop through each row and capture each sub-string and convert to data frame
rowsGLlist <- list()
for (i in 1:nrow(rowsGLdatcorp)){
  rowsGLlist[[i]] <- as.data.frame(cbind(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[i]), \\s+))[1],
                                        unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[i]), \\s+))[2],
                                        unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[i]), \\s+))[3],
                                        unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[i]), \\s+))[4],
                                        str_c(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[i]), \\s+))[-c(1:4,
                                                                                                               length(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[i]), \\s+))),
                                                                                                               length(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[i]), \\s+)))-1)],
                                              collapse=" "),
                                        unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[i]), \\s+))[length(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[i]), \\s+)))-1],
                                        unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[i]), \\s+))[length(unlist(strsplit(trimws(rowsGLdatcorp$ChrColname[i]), \\s+)))]
  ))
}
rowsGLdat2 <- as.data.frame(do.call(rbind,rowsGLlist))
colnames(rowsGLdat2) <- c('ADB DATE',
                          'POSTED DATE',
                          'JOURNAL ID',
                          'ITEM NUMBER',
                          'TRANS DESCRIPTION', # includes LF/DP/FM1...   039##... etc
                          'DEBITS',
                          'CREDITS')
str(rowsGLdat2) # reformat data
rowsGLdat2$`ADB DATE` <- as.Date(rowsGLdat2$`ADB DATE`,format = '%m/%d/%Y')
rowsGLdat2$`POSTED DATE` <- as.Date(rowsGLdat2$`POSTED DATE`,format = '%m/%d/%Y')
rowsGLdat2$DEBITS <- as.numeric(parse_number(rowsGLdat2$DEBITS))
rowsGLdat2$CREDITS <- as.numeric(parse_number(rowsGLdat2$CREDITS))

## How to include Account and Department #s? Is it necessary if we are only looking at sub-strings of TRANS DESCRIPTION to match records? Ending balance?
  # Is Account/Dept even necessary? We don't typically look at subtotals
  # Need to be able to clean and locate end balance, total for first account

rowsGLdatMisc # Made above, or produce based on which rows don't start with dates?
rowsGLdatOG <- data.frame(rowsGL)
# If cell = "Account" and cell below is populated then record Account number under variable Account until next occurrence of "Account"
EndBalance <- print(as.numeric(parse_number(unlist(strsplit(trimws(rowsGLdatOG[rowsGLdatOG$rowsGL %like% "TOTAL for Account",][1]), \\s+))[8])), digits = 10) # FOR REC

### REJECTS ###
  ## Rejects will be logged if they have the proper corresponding Transaction Code
  ## "POSTED REJECTS" leave open in the log and/or offset in GL
  ## Payments are SRCE790 or 663 or 999, TRAN610 or 662 or 619 or 666
  ## Advances are SRCE999 or 790 TRAN755
   ## SAVE AMOUNTS IN REC, Payments are POSITIVE, Advances/Checks are NEGATIVE - this may be a manual entry by the user, or arithmetic we perform after ingesting here 
   ## In Rec - save current_date(), SRCE, TRAN, TRAN DESC, ACCT#, Amount

rowsREJdat <- as.data.frame(rowsREJ)
rowsREJdat[rowsREJdat$rowsREJ %like% "TITLE",] # grabs SRCE and TRAN codes, need associated TRAN AMOUNTS

# Remove unwanted rows
rowsREJdatNum <- data.frame(grep('\\d+', rowsREJdat$rowsREJ, value=TRUE)) # only values with numeric information in the row, remove data.frame and use as matrix?
colnames(rowsREJdatNum) <- "Nums"
rowsREJdatNum <- data.frame(rowsREJdatNum[-(which(rowsREJdatNum$Nums %like% "L031")),]) # remove all lines including L031
colnames(rowsREJdatNum) <- "Nums"
rowsREJdatNum <- data.frame(rowsREJdatNum[-(which(rowsREJdatNum$Nums %like% "AMERANT")),]) # remove all lines including AMERANT
colnames(rowsREJdatNum) <- "Nums"
rowsREJdatNum <- data.frame(rowsREJdatNum[- grep("TOTAL", rowsREJdatNum$Nums),]) # remove TOTAL(S)
colnames(rowsREJdatNum) <- "Nums"

rowsREJlist <- list()
suppressWarnings({
for(i in 1:nrow(rowsREJdatNum)){
  rowsREJlist[[i]] <- ifelse(as.numeric(parse_number(unlist(strsplit(trimws(rowsREJdatNum$Nums[i]), \\s+))))[length(unlist(strsplit(trimws(rowsREJdatNum$Nums[i]), \\s+)))] > 8, rowsREJdatNum$Nums[i], NA)
}
})
rowsREJdat2 <- as.data.frame(do.call(rbind,rowsREJlist))
rowsREJdat2 <- na.omit(rowsREJdat2) 

# Clean rows with amounts, create new DF

rowsREJlistAmts <- list()
for(i in 1:length(rowsREJdat2[!rowsREJdat2$V1 %like% "TITLE",])){
  rowsREJlistAmts[[i]] <- as.numeric(parse_number(strsplit(trimws(rowsREJdat2[!rowsREJdat2$V1 %like% "TITLE",]),\\s+)[[i]][length(strsplit(trimws(rowsREJdat2[!rowsREJdat2$V1 %like% "TITLE",]),\\s+)[[i]])]))
}
rowsREJdat2Amts <- as.data.frame(do.call(rbind,rowsREJlistAmts))

# If there is an amount UNDER SRCE/TRAN codes then retain the codes and amounts, else drop SRCE/TRAN codes

rowsREJdat3 <- ifelse(rowsREJdat2[nrow(rowsREJdat2),] %like% "TITLE", 
                      as.data.frame(rowsREJdat2[1:(nrow(rowsREJdat2)-1),]), # remove bottom row, having DF quality issues
                      rowsREJdat2)
rowsREJdat3 <- data.frame(rowsREJdat3[[1]]) # this line is okay even if the ifelse statement = F
colnames(rowsREJdat3) <- "Val"

# Let's flag every occurrence of "TITLE" to count how many transactions are under each one

for(i in 1:nrow(rowsREJdat3)){
  rowsREJdat3$titleFlag <- ifelse(rowsREJdat3$Val %like% "TITLE",1,0)
}
# Repeat TITLE... until the next occurrence of 1
rowsREJdat3$Codes <- ifelse(rowsREJdat3$titleFlag==1,rowsREJdat3$Val,NA)
rowsREJdat3$Codes <- na.locf(rowsREJdat3$Codes)

# Cbind the two new DFs and that is our group of rejects for the Rec

rowsREJmatAmounts <- matrix(nrow=3)
for(i in 1:nrow(rowsREJdat3[!rowsREJdat3$Val %like% "TITLE",])){
  rowsREJmatAmounts[i,] <- paste(rowsREJdat3$Codes[rowsREJdat3$titleFlag==0][i],
                                  "&",
                                  as.numeric(parse_number(strsplit(trimws(rowsREJdat3[!rowsREJdat3$Val %like% "TITLE",1]),\\s+)[[i]][length(strsplit(trimws(rowsREJdat3[!rowsREJdat3$Val %like% "TITLE",1]),\\s+)[[i]])])))
}
rowsREJmatAmounts
rowsREJdatAmounts <- as.data.frame(rowsREJmatAmounts)
rowsREJdatAmounts <- separate(data = rowsREJdatAmounts, col = V1, into = c("Codes", "Amount"), sep = "\\&")
rowsREJdatAmounts$Amount <- as.numeric(parse_number(rowsREJdatAmounts$Amount))

### ONLINE DOLLAR VS DAILY JOURNAL ###
  ## Start at Title User ID A325 and match names to the amounts under each Title User ID (like CD Ctrl)
  ## Whenever we have multiple transactions for OLD, only first will have account ID (like CD Ctrl)
  ## Be wary of non-monetary codes
  ## SC529 totals credit and debit should be in GL
  ## SC530 advances/credit/payment memos in DTJ are 600s TRCD (exception 661)
  ## SC530 debit memos in DTJ are 700s/750 TRCD (exception 727)
  ## Leave open if does not entirely match, that can be done manually

rowsOLDdat <- as.data.frame(rowsOLD)
colnames(rowsOLDdat) <- "Cols"

# Remove header data

rowsOLDdatCorp <- as.data.frame(rowsOLDdat[!grepl("ONLINE DOLLAR TRANSACTION JRNL", rowsOLDdat$Cols),])
colnames(rowsOLDdatCorp) <- "Cols"
rowsOLDdatCorp <- as.data.frame(rowsOLDdatCorp[!grepl("ACCOUNT NO", rowsOLDdatCorp$Cols),])
colnames(rowsOLDdatCorp) <- "Cols"
rowsOLDdatCorp <- as.data.frame(rowsOLDdatCorp[!grepl("TRAN CODE & NAME", rowsOLDdatCorp$Cols),])
colnames(rowsOLDdatCorp) <- "Cols"
rowsOLDdatCorp <- as.data.frame(rowsOLDdatCorp[!grepl("ORIG CODE & NAME", rowsOLDdatCorp$Cols),])
colnames(rowsOLDdatCorp) <- "Cols"
rowsOLDdatCorp <- as.data.frame(rowsOLDdatCorp[!grepl("RET CODE & NAME", rowsOLDdatCorp$Cols),])
colnames(rowsOLDdatCorp) <- "Cols"
rowsOLDdatCorp <- as.data.frame(rowsOLDdatCorp[!grepl("USER ID:", rowsOLDdatCorp$Cols),])
colnames(rowsOLDdatCorp) <- "Cols"

# Create new working data for corpus and LIVE DDA totals

rowsOLDdatCorp2 <- rowsOLDdatCorp %>% filter(row_number() < which(Cols %like% 'BREAK TOTALS')) 
rowsOLDdatCorpTot <- rowsOLDdatCorp %>% filter(row_number() > which(Cols %like% 'NON-REAL'))

# Remove  L - LIVE => "" and then na.omit open lines

rowsOLDdatCorp2$Cols <- gsub("L - LIVE","",rowsOLDdatCorp2$Cols)
rowsOLDdatCorp2 <- as.data.frame(filter(rowsOLDdatCorp2, !grepl("^                                                                            ", rowsOLDdatCorp2$Cols)))
colnames(rowsOLDdatCorp) <- "Cols"

# Create loop to save transactions under each Account number as a list, then restructure list to have one row per memo
  # Remove line with backdated info and create column for date so each account transaction has 2 lines of code total (vs 3)? 

rowsOLDdatCorp2[grepl("1390101   0000001",rowsOLDdatCorp2$Cols),][i] # top of loop, row should equal this information and capture everything before next instance

rowsOLDdatCorp2top <- as.data.frame(rowsOLDdatCorp2[grepl("1390101   0000001",rowsOLDdatCorp2$Cols),])
colnames(rowsOLDdatCorp2top) <- "Cols"
rowsOLDdatCorp2bot <- as.data.frame(rowsOLDdatCorp2[!grepl("1390101   0000001",rowsOLDdatCorp2$Cols),])
colnames(rowsOLDdatCorp2bot) <- "Cols"

# Bottom has BACKDATED. Create DF with columns and then make empty cells NA, then copy data to empty row beneath and delete row with BACKDATED

rowsOLDbotList <- list()
for (i in 1:nrow(rowsOLDdatCorp2bot)){
  rowsOLDbotList[[i]] <- as.data.frame(cbind(
                                         str_c(unlist(strsplit(trimws(rowsOLDdatCorp2bot$Cols[i]), \\s+))[1],
                                               " ",
                                               unlist(strsplit(trimws(rowsOLDdatCorp2bot$Cols[i]), \\s+))[2]),
                                         unlist(strsplit(trimws(rowsOLDdatCorp2bot$Cols[i]), \\s+))[3],
                                         unlist(strsplit(trimws(rowsOLDdatCorp2bot$Cols[i]), \\s+))[4],
                                         unlist(strsplit(trimws(rowsOLDdatCorp2bot$Cols[i]), \\s+))[5],
                                         unlist(strsplit(trimws(rowsOLDdatCorp2bot$Cols[i]), \\s+))[6],
                                         unlist(strsplit(trimws(rowsOLDdatCorp2bot$Cols[i]), \\s+))[7],
                                         unlist(strsplit(trimws(rowsOLDdatCorp2bot$Cols[i]), \\s+))[8]
  ))
}
rowsOLDbotdat <- as.data.frame(do.call(rbind,rowsOLDbotList))
colnames(rowsOLDbotdat) <- c('MEMO CODE',
                          'AMOUNT',
                          'BACKDATE',
                          'SERIAL', # to be deleted
                          'SRC', # includes LF/DP/FM1...   039##... etc
                          'CONTROL',
                          'USER ID')


# Move non-date fields over one

off.set.rows <- c(which(is.na(rowsOLDbotdat[ , ncol(rowsOLDbotdat)])))
rowsOLDbotdat[off.set.rows, 5:ncol(rowsOLDbotdat)] <- rowsOLDbotdat[off.set.rows, 4:(ncol(rowsOLDbotdat)-1)] # shift rows after the third column (starting 4)
rowsOLDbotdat <- rowsOLDbotdat[,-c(3,4)]
rowsOLDbotdat <- fill(rowsOLDbotdat, 2:5, .direction = "down")
rowsOLDbotdat <- as.data.frame(rowsOLDbotdat[-(which(rowsOLDbotdat$`MEMO CODE` %like% "BACKDATED")),])
rowsOLDbotdat$AMOUNT <- as.numeric(parse_number(rowsOLDbotdat$AMOUNT))

# Repeat above for "top"

rowsOLDtopList <- list()
for (i in 1:nrow(rowsOLDdatCorp2top)){
  rowsOLDtopList[[i]] <- as.data.frame(cbind(
    unlist(strsplit(trimws(rowsOLDdatCorp2top$Cols[i]), \\s+))[1],
    str_c(unlist(strsplit(trimws(rowsOLDdatCorp2top$Cols[i]), \\s+))[-c(1,
                                                                          length(unlist(strsplit(trimws(rowsOLDdatCorp2top$Cols[i]), \\s+))),
                                                                          length(unlist(strsplit(trimws(rowsOLDdatCorp2top$Cols[i]), \\s+)))-1)],
          collapse=" "),
    str_c(unlist(strsplit(trimws(rowsOLDdatCorp2top$Cols[i]), \\s+))[length(unlist(strsplit(trimws(rowsOLDdatCorp2top$Cols[i]), \\s+)))-1],
          " ",
          unlist(strsplit(trimws(rowsOLDdatCorp2top$Cols[i]), \\s+))[length(unlist(strsplit(trimws(rowsOLDdatCorp2top$Cols[i]), \\s+)))])
  ))
}
rowsOLDtopdat <- as.data.frame(do.call(rbind,rowsOLDtopList))
colnames(rowsOLDtopdat) <- c('ACCOUNT NO',
                             'NAME',
                             'GL OFFSET ACCT')

# Column bind columns together such that one row is a transaction line

rowsOLDdatAmounts <- as.data.frame(cbind(rowsOLDtopdat[,1:2],rowsOLDbotdat,rowsOLDtopdat[,3])) # WORKING OLD DATA
colnames(rowsOLDdatAmounts)[8] <- "GL OFFSET ACCT" # column may not even be relevant
rowsOLDdatAmounts$'ABBR USER ID' <- paste(substr(rowsOLDdatAmounts$`USER ID`,1,1),substr(rowsOLDdatAmounts$`USER ID`, nchar(rowsOLDdatAmounts$`USER ID`) - 3 + 1, nchar(rowsOLDdatAmounts$`USER ID`)),sep="")

# Don't forget about the totals to rec to the GL!

rowsOLDtotList <- list()
for (i in 1:nrow(rowsOLDdatCorpTot)){
  rowsOLDtotList[[i]] <- as.data.frame(cbind(
    str_c(unlist(strsplit(trimws(rowsOLDdatCorpTot$Cols[i]), \\s+))[-c(length(unlist(strsplit(trimws(rowsOLDdatCorpTot$Cols[i]), \\s+))),
                                                                          length(unlist(strsplit(trimws(rowsOLDdatCorpTot$Cols[i]), \\s+)))-1)],
          collapse=" "),
    unlist(strsplit(trimws(rowsOLDdatCorpTot$Cols[i]), \\s+))[length(unlist(strsplit(trimws(rowsOLDdatCorpTot$Cols[i]), \\s+)))-1],
    unlist(strsplit(trimws(rowsOLDdatCorpTot$Cols[i]), \\s+))[length(unlist(strsplit(trimws(rowsOLDdatCorpTot$Cols[i]), \\s+)))]
  ))
}
rowsOLDdatTotals<- as.data.frame(do.call(rbind,rowsOLDtotList))
colnames(rowsOLDdatTotals) <- c('NON-REAL TIME BANK TOTALS',
                             'AMOUNT',
                             'NUMBER')
rowsOLDdatTotals$AMOUNT <- as.numeric(parse_number(rowsOLDdatTotals$AMOUNT))

### DAILY JOURNAL OPEN CHARGES ###
  ## Anything not eliminated from OLD should be noted in log
  ## Any FTM amounts from GL in DTJ should be logged
  ## Be wary of non-monetary codes as well as unbalanced items in log
  ## Match first pass using acct number in Trans Description and then amounts if no success

rowsDTJdat <- as.data.frame(rowsDTJ)
colnames(rowsDTJdat) <- "Cols"

# Get rid of header lines throughout document - necessary? May just jump to collecting relevant information?

rowsDTJdatCorp <- as.data.frame(rowsDTJdat[!grepl("DAILY TRANSACTION JOURNAL", rowsDTJdat$Cols),])
colnames(rowsDTJdatCorp) <- "Cols"
rowsDTJdatCorp <- as.data.frame(rowsDTJdatCorp[!grepl("ACCT NO", rowsDTJdatCorp$Cols),])
colnames(rowsDTJdatCorp) <- "Cols"
rowsDTJdatCorp <- as.data.frame(rowsDTJdatCorp[!grepl("STAT SHORT NAME ", rowsDTJdatCorp$Cols),])
colnames(rowsDTJdatCorp) <- "Cols"
rowsDTJdatCorp <- as.data.frame(rowsDTJdatCorp[!grepl("BUYDOWN AMT", rowsDTJdatCorp$Cols),])
colnames(rowsDTJdatCorp) <- "Cols"
rowsDTJdatCorp <- as.data.frame(rowsDTJdatCorp[!grepl("RET CODE & NAME", rowsDTJdatCorp$Cols),])
colnames(rowsDTJdatCorp) <- "Cols"

# Additional rows to remove that are not header rows

rowsDTJdatCorp <- as.data.frame(rowsDTJdatCorp[!grepl("   AMT", rowsDTJdatCorp$Cols),])
colnames(rowsDTJdatCorp) <- "Cols"
rowsDTJdatCorp <- as.data.frame(rowsDTJdatCorp[!grepl("TITLE   ", rowsDTJdatCorp$Cols),])
colnames(rowsDTJdatCorp) <- "Cols"
rowsDTJdatCorp <- as.data.frame(rowsDTJdatCorp[!grepl("DESCRIPTION:    ", rowsDTJdatCorp$Cols),]) # remove description since redundant and notes can be found manually
colnames(rowsDTJdatCorp) <- "Cols"
rowsDTJdatCorp <- as.data.frame(rowsDTJdatCorp[!grepl("       LCG", rowsDTJdatCorp$Cols),])
colnames(rowsDTJdatCorp) <- "Cols"
rowsDTJdatCorp <- as.data.frame(rowsDTJdatCorp[!grepl("AUTO ADJUST", rowsDTJdatCorp$Cols),])
colnames(rowsDTJdatCorp) <- "Cols"
rowsDTJdatCorp <- as.data.frame(rowsDTJdatCorp[!grepl("UNAPPLIED", rowsDTJdatCorp$Cols),])
colnames(rowsDTJdatCorp) <- "Cols"
rowsDTJdatCorp <- as.data.frame(rowsDTJdatCorp[!grepl("TOTAL TRANS", rowsDTJdatCorp$Cols),])
colnames(rowsDTJdatCorp) <- "Cols"
rowsDTJdatCorp <- as.data.frame(rowsDTJdatCorp[!grepl("BALANCING TOTALS", rowsDTJdatCorp$Cols),])
colnames(rowsDTJdatCorp) <- "Cols"

# Break DJ up into multiple DFs by source code
# Totals to GL: 529, 662, 790 (credit total + rejects credit should = GL POD credit), 794 (only use first instance of TRAN CD TOTAL), 795, 876
# SRCE 530 has odd totals to retain for GL - R006 (credit), all user IDs under monetary codes balancing totals A324 rec to GL
  # Need to add A324??
# 000 is independent automatic from prior days esp with Tran Control No. starting with U
# Grab key strings in rows relevant to totals/balance totals
# Grab row ==    "TOTALS      TRAN SRCE CODE: 529" or other 3-digit number with nothing after and then df[row+3 or 4,] for debits and credits 

#####

rowsDTJdatCorpTotals <- as.data.frame(rowsDTJdatCorp[grepl('^TOTALS',rowsDTJdatCorp$Cols),]) # Create temp df with all totals
colnames(rowsDTJdatCorpTotals) <- "Cols"

rowsDTJtotList <- list()
for (i in 1:nrow(rowsDTJdatCorpTotals)){
  rowsDTJtotList[[i]] <- as.data.frame(cbind(
    unlist(strsplit(trimws(rowsDTJdatCorpTotals$Cols[i]), \\s+))[1],
    unlist(strsplit(trimws(rowsDTJdatCorpTotals$Cols[i]), \\s+))[2],
    unlist(strsplit(trimws(rowsDTJdatCorpTotals$Cols[i]), \\s+))[3],
    unlist(strsplit(trimws(rowsDTJdatCorpTotals$Cols[i]), \\s+))[4],
    unlist(strsplit(trimws(rowsDTJdatCorpTotals$Cols[i]), \\s+))[5],
    unlist(strsplit(trimws(rowsDTJdatCorpTotals$Cols[i]), \\s+))[6],
    unlist(strsplit(trimws(rowsDTJdatCorpTotals$Cols[i]), \\s+))[7],
    unlist(strsplit(trimws(rowsDTJdatCorpTotals$Cols[i]), \\s+))[8],
    unlist(strsplit(trimws(rowsDTJdatCorpTotals$Cols[i]), \\s+))[9],
    unlist(strsplit(trimws(rowsDTJdatCorpTotals$Cols[i]), \\s+))[10],
    unlist(strsplit(trimws(rowsDTJdatCorpTotals$Cols[i]), \\s+))[11],
    unlist(strsplit(trimws(rowsDTJdatCorpTotals$Cols[i]), \\s+))[12] 
  ))
}
rowsDTJdatTotals<- as.data.frame(do.call(rbind,rowsDTJtotList))
rowsDTJdatTotals <- rowsDTJdatTotals[is.na(rowsDTJdatTotals$V6),1:5]
rowsDTJdatTotals$Code <- paste(rowsDTJdatTotals$V1,rowsDTJdatTotals$V2,rowsDTJdatTotals$V3,rowsDTJdatTotals$V4)
rowsDTJdatTotals <- as.data.frame(cbind(rowsDTJdatTotals$Code,rowsDTJdatTotals$V5))
colnames(rowsDTJdatTotals) <- c("Code Name", "SRCE") 
rowsDTJdatTotals <- rowsDTJdatTotals[which(rowsDTJdatTotals$SRCE != '999' & rowsDTJdatTotals$SRCE != '875'),] # and 530? balancing credit only? Specific rules?

#####

## Totals - UPDATE - need A324 Totals?

rowsDTJtotAmtsList <- list()
for(i in 1:length(rowsDTJdatCorp[which(rowsDTJdatCorp$Cols %like% 'DEBIT' | rowsDTJdatCorp$Cols %like% 'CREDIT'),])){
  rowsDTJtotAmtsList[[i]] <- rbind(rowsDTJdatCorp[which(rowsDTJdatCorp$Cols %like% 'DEBIT' | rowsDTJdatCorp$Cols %like% 'CREDIT')-1,][i] ,
                             rowsDTJdatCorp[which(rowsDTJdatCorp$Cols %like% 'DEBIT' | rowsDTJdatCorp$Cols %like% 'CREDIT'),][i] )
}
rowsDTJdatTotalsAmts<- as.data.frame(do.call(rbind,rowsDTJtotAmtsList)) # match lines to rowsDTJdatTotals

  # Clean the rows to make comparisons

colnames(rowsDTJdatTotalsAmts) <- "Cols"

rowsDTJtotList <- list()
for (i in 1:nrow(rowsDTJdatTotalsAmts)){
  rowsDTJtotList[[i]] <- as.data.frame(cbind(
    unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Cols[i]), \\s+))[1],
    unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Cols[i]), \\s+))[2],
    unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Cols[i]), \\s+))[3],
    unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Cols[i]), \\s+))[4],
    unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Cols[i]), \\s+))[5],
    unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Cols[i]), \\s+))[6],
    unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Cols[i]), \\s+))[7],
    unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Cols[i]), \\s+))[8],
    unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Cols[i]), \\s+))[9],
    unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Cols[i]), \\s+))[10],
    unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Cols[i]), \\s+))[11],
    unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Cols[i]), \\s+))[12] 
  ))
}
rowsDTJdatTotalsAmts<- as.data.frame(do.call(rbind,rowsDTJtotList))
rowsDTJdatTotalsAmts$Code <- paste(rowsDTJdatTotalsAmts$V1,rowsDTJdatTotalsAmts$V2,rowsDTJdatTotalsAmts$V3,rowsDTJdatTotalsAmts$V4)
rowsDTJdatTotalsAmts$SRCE <- rowsDTJdatTotalsAmts$V5
rowsDTJdatTotalsAmts$Extra <- paste(rowsDTJdatTotalsAmts$V6,rowsDTJdatTotalsAmts$V7,rowsDTJdatTotalsAmts$V8,rowsDTJdatTotalsAmts$V9,
                                    rowsDTJdatTotalsAmts$V10,rowsDTJdatTotalsAmts$V11,rowsDTJdatTotalsAmts$V12)
rowsDTJdatTotalsAmts <- rowsDTJdatTotalsAmts[,c(13:15)]
rownames(rowsDTJdatTotalsAmts) <- 1:nrow(rowsDTJdatTotalsAmts)

# Move every other row to be on same line

even_rows <- seq_len(nrow(rowsDTJdatTotalsAmts)) %% 2 
rowsDTJdatTotalsAmts_Amts <- rowsDTJdatTotalsAmts$Code[even_rows == 0] 
rowsDTJdatTotalsAmts_Else <- rowsDTJdatTotalsAmts[even_rows == 1,] 
rowsDTJdatTotalsAmts <- as.data.frame(cbind(rowsDTJdatTotalsAmts_Else,rowsDTJdatTotalsAmts_Amts))
colnames(rowsDTJdatTotalsAmts)[4] <- "Amounts" # need to split into debit/credit; want to keep Extra starting with NA

# Remove rows that don't start with NA in Extra (except 530 R006) and SRCE has length > 3 - LEFT WITH TOTALS and messy Amounts col

rowsDTJdatTotalsAmts <- rowsDTJdatTotalsAmts[!is.na(rowsDTJdatTotalsAmts$SRCE),]
rowsDTJdatTotalsAmts <- rowsDTJdatTotalsAmts[nchar(rowsDTJdatTotalsAmts$SRCE) == 3,]
rowsDTJdatTotalsAmts <- rowsDTJdatTotalsAmts[grepl("^NA", rowsDTJdatTotalsAmts$Extra) | grepl("R006", rowsDTJdatTotalsAmts$Extra),]
rowsDTJdatTotalsAmts <- as.data.frame(rowsDTJdatTotalsAmts %>% group_by(SRCE) %>% filter(row_number()==1))
rowsDTJdatTotalsAmts$SRCE <- as.factor(rowsDTJdatTotalsAmts$SRCE)
levels(rowsDTJdatTotalsAmts$SRCE)[which(levels(as.factor(rowsDTJdatTotalsAmts$SRCE)) %in% "530")] <- "530-R006" # Reassign 530, eliminate "Extra"
rowsDTJdatTotalsAmts <- rowsDTJdatTotalsAmts[,-which(names(rowsDTJdatTotalsAmts) %in% "Extra")]

# Create loop breaking down Amounts and whether it's a debit or credit
  # After loop, if DEBIT/CREDIT and Amount then create DEBIT/CREDIT variables and save amounts underneath for each
  # Still need Extra col? May be redundant

#####
# Issue with this due to inconsistent Amounts var
rowsDTJtotAmtsListDebCred <- list()
for (i in 1:nrow(rowsDTJdatTotalsAmts)){
  rowsDTJtotAmtsListDebCred[[i]] <- as.data.frame(cbind(
unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Amounts[i]), \\s+))[[which(unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Amounts[i]), \\s+)) %in% "DEBIT" )]], # grabs "debit"
unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Amounts[i]), \\s+))[[which(unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Amounts[i]), \\s+)) %in% "DEBIT" ) + 1]], # grabs debit amount
unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Amounts[i]), \\s+))[[which(unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Amounts[i]), \\s+)) %in% "CREDIT" )]], # grabs "credit"
unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Amounts[i]), \\s+))[[which(unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Amounts[i]), \\s+)) %in% "CREDIT" ) + 1]] # grabs credit amount
  ))}
rowsDTJdatTotalsAmtsDebCred <- as.data.frame(do.call(rbind,rowsDTJtotAmtsListDebCred))
# Append amounts and clean to rowsDTJdatTotalsAmts
#####

rowsDTJtotAmtsListDebCred <- list()
for (i in 1:nrow(rowsDTJdatTotalsAmts)){
  rowsDTJtotAmtsListDebCred[[i]] <- as.data.frame(cbind(
unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Amounts[i]), \\s+))[1],
unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Amounts[i]), \\s+))[2],
unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Amounts[i]), \\s+))[3],
unlist(strsplit(trimws(rowsDTJdatTotalsAmts$Amounts[i]), \\s+))[4]
  ))}
rowsDTJdatTotalsAmtsDebCred <- as.data.frame(do.call(rbind,rowsDTJtotAmtsListDebCred))
rownames(rowsDTJdatTotalsAmtsDebCred) <- paste(rowsDTJdatTotalsAmts[,1],rowsDTJdatTotalsAmts[,2])

rowsDTJdatTotalsAmtsDebCred2 <- matrix(ncol=3, nrow=nrow(rowsDTJdatTotalsAmtsDebCred)*2)
rowsDTJdatTotalsAmtsDebCred2[,1] <- sort(rep(paste(rowsDTJdatTotalsAmts[,1],rowsDTJdatTotalsAmts[,2]),2))
rowsDTJdatTotalsAmtsDebCred2[,2] <- matrix(rbind(rowsDTJdatTotalsAmtsDebCred$V1,rowsDTJdatTotalsAmtsDebCred$V3), ncol=1)
rowsDTJdatTotalsAmtsDebCred2[,3] <- matrix(rbind(rowsDTJdatTotalsAmtsDebCred$V2,rowsDTJdatTotalsAmtsDebCred$V4), ncol=1)
rowsDTJdatTotalsAmtsDebCred2 <- as.data.frame(rowsDTJdatTotalsAmtsDebCred2)
colnames(rowsDTJdatTotalsAmtsDebCred2) <- c("SRCE","TranType","Amount")

# Need DEBIT, CREDIT, NA, AMOUNT for each SRCE, only want one row for each SRCE
rowsDTJdatTotalsAmtsDebCred2$DEBIT <- ifelse(rowsDTJdatTotalsAmtsDebCred2$TranType == 'DEBIT',rowsDTJdatTotalsAmtsDebCred2$Amount,0)
rowsDTJdatTotalsAmtsDebCred2$CREDIT <- ifelse(rowsDTJdatTotalsAmtsDebCred2$TranType == 'CREDIT',rowsDTJdatTotalsAmtsDebCred2$Amount,0)
rowsDTJdatTotalsAmtsDebCred2$NAs <- ifelse(rowsDTJdatTotalsAmtsDebCred2$TranType == 'NA',rowsDTJdatTotalsAmtsDebCred2$Amount,0)
rowsDTJdatTotalsAmtsDebCred2 <- rowsDTJdatTotalsAmtsDebCred2[-which(rowsDTJdatTotalsAmtsDebCred2$NAs == 'NA'),]
rowsDTJdatAmounts <- as.data.frame(rowsDTJdatTotalsAmtsDebCred2[,-which(names(rowsDTJdatTotalsAmtsDebCred2) %in% c("TranType","Amount","NAs"))])
# If two SRCE codes are the same and there is a 0 for credit and $ in debit/vice-versa, consolidate 
rowsDTJdatAmounts$DEBIT <- as.numeric(parse_number(rowsDTJdatAmounts$DEBIT))
rowsDTJdatAmounts$CREDIT <- as.numeric(parse_number(rowsDTJdatAmounts$CREDIT))
rowsDTJdatAmounts <- as.data.frame(rowsDTJdatAmounts %>% group_by(SRCE) %>% summarise(across(c(DEBIT, CREDIT), sum))) 

## 530 SRCE Code 

# 530 is independent and codes in 600s/700s should be retained separately to rec to OLD (A325 especially/all other USER IDs TRAN CODES: 610, 650, 660, 680, 750)
# DEBIT - 750: Advance, 800: Reversal
# CREDIT - 610: Regular Payment, 650: Regular Payoff, 660: Special Payment, 680: Fee Payment, 619: ACH, 620: Payment
  # However 530 TRAN CTRL R006 is recorded in Totals to rec to GL
   # CLEAR WITH LUISA - A324 IN TOTALS?
# SRCE 530 has odd totals to retain for GL - R006 (credit), all user IDs under monetary codes balancing totals for A324 (need to add to Totals?) rec to GL (comment == line 416)
  # Note non-monetary codes and exclude

rowsDTJdatCorp # only need to work with SRCE 530, start at USER ID A325

# Start with raw version and only work with 530 - save down rowsDTJdatCorp2 

# Get rid of header lines throughout document - necessary? May just jump to collecting relevant information?

rowsDTJdatCorp2 <- as.data.frame(rowsDTJdat[!grepl("DAILY TRANSACTION JOURNAL", rowsDTJdat$Cols),])
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("ACCT NO", rowsDTJdatCorp2$Cols),])
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("STAT SHORT NAME ", rowsDTJdatCorp2$Cols),])
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("BUYDOWN AMT", rowsDTJdatCorp2$Cols),])
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("RET CODE & NAME", rowsDTJdatCorp2$Cols),])
colnames(rowsDTJdatCorp2) <- "Cols"

# Additional rows to remove that are not header rows

rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("   AMT", rowsDTJdatCorp2$Cols),])
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("DESCRIPTION:    ", rowsDTJdatCorp2$Cols),]) # remove description since redundant and notes can be found manually
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("       LCG", rowsDTJdatCorp2$Cols),])
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("AUTO ADJUST", rowsDTJdatCorp2$Cols),])
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("UNAPPLIED", rowsDTJdatCorp2$Cols),])
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("TOTAL TRANS", rowsDTJdatCorp2$Cols),])
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("BALANCING TOTALS", rowsDTJdatCorp2$Cols),])
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("TOTALS", rowsDTJdatCorp2$Cols),]) # NEW - want to get rid of totals
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("DEBIT", rowsDTJdatCorp2$Cols),]) # NEW - want to get rid of total debits
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("CREDIT", rowsDTJdatCorp2$Cols),]) # NEW - want to get rid of total credits
colnames(rowsDTJdatCorp2) <- "Cols"
rowsDTJdatCorp2 <- as.data.frame(rowsDTJdatCorp2[!grepl("TRANS ", rowsDTJdatCorp2$Cols),]) # NEW - want to get rid of transaction code thru lines
colnames(rowsDTJdatCorp2) <- "Cols"

## Clean data

# Start by cleaning all data together or only focus on 530?

rowsDTJdatCorpTitles <- as.data.frame(rowsDTJdatCorp2[grepl("TITLE", rowsDTJdatCorp2$Cols),])
colnames(rowsDTJdatCorpTitles) <- "Cols"

rowsDTJdatCorpTrans <- as.data.frame(rowsDTJdatCorp2[!grepl("TITLE", rowsDTJdatCorp2$Cols),])
colnames(rowsDTJdatCorpTrans) <- "Cols"

# In transactions remove rows that start with nothing/include only a number and "MISC"
# Reminder, filter for these codes and row below (data[<row code> + 1,]):
  # 530 is independent and codes in 600s/700s should be retained separately to rec to OLD (A325 especially/all other USER IDs TRAN CODES: 610, 650, 660, 680, 750)
    # DEBIT - 750: Advance, 800: Reversal
    # CREDIT - 610: Regular Payment, 650: Regular Payoff, 660: Special Payment, 680: Fee Payment, 619: ACH, 620: Payment  

rowsDTJdatCorpTrans <- as.data.frame(rowsDTJdatCorpTrans[!grepl("                                                                          ", rowsDTJdatCorpTrans$Cols),]) # NEW - want to get rid of total credits
colnames(rowsDTJdatCorpTrans) <- "Cols"
rowsDTJdatCorpTrans <- as.data.frame(rowsDTJdatCorpTrans[!grepl("MISC ", rowsDTJdatCorpTrans$Cols),]) # NEW - want to get rid of transaction code thru lines
colnames(rowsDTJdatCorpTrans) <- "Cols"

# Create transaction ID with 11-digit # ID and 6-digit note #, maps to GL/SSMS# Grab key strings in rows relevant to transaction information
  # User will have to manually find descriptions (POST REJECT, Transaction Acct #, etc) for discrepancies

rowsDTJdatCorpTransList <- list()
for (i in 1:nrow(rowsDTJdatCorpTrans)){
  rowsDTJdatCorpTransList[[i]] <- if (nchar(unlist(strsplit(trimws(rowsDTJdatCorpTrans$Cols[i]), \\s+))[1]) == 11){
                                    rowsDTJdatCorpTrans[i,] # Want to make sure first element in row/string has 11 characters 
                                  } 
}
rowsDTJdatCorpTransCln <- as.data.frame(do.call(rbind,rowsDTJdatCorpTransList))
colnames(rowsDTJdatCorpTransCln) <- "Cols"

# Individual items to GL: 530 - amounts, account names, and TRAN CONTROL NO maps to OLD abbr user ID

rowsDTJTransDatList <- list()
for (i in 1:nrow(rowsDTJdatCorpTransCln)){
  rowsDTJTransDatList[[i]] <- as.data.frame(cbind(
   unlist(strsplit(trimws(rowsDTJdatCorpTransCln$Cols[i]), \\s+))[1],
   unlist(strsplit(trimws(rowsDTJdatCorpTransCln$Cols[i]), \\s+))[2],
   unlist(strsplit(trimws(rowsDTJdatCorpTransCln$Cols[i]), \\s+))[3],
   unlist(strsplit(trimws(rowsDTJdatCorpTransCln$Cols[i]), \\s+))[4],
   unlist(strsplit(trimws(rowsDTJdatCorpTransCln$Cols[i]), \\s+))[5],
   unlist(strsplit(trimws(rowsDTJdatCorpTransCln$Cols[i]), \\s+))[6],
   unlist(strsplit(trimws(rowsDTJdatCorpTransCln$Cols[i]), \\s+))[7],
   unlist(strsplit(trimws(rowsDTJdatCorpTransCln$Cols[i]), \\s+))[8],
   unlist(strsplit(trimws(rowsDTJdatCorpTransCln$Cols[i]), \\s+))[9],
   unlist(strsplit(trimws(rowsDTJdatCorpTransCln$Cols[i]), \\s+))[10],
   unlist(strsplit(trimws(rowsDTJdatCorpTransCln$Cols[i]), \\s+))[11],
   unlist(strsplit(trimws(rowsDTJdatCorpTransCln$Cols[i]), \\s+))[12]
  ))
}
rowsDTJTransDat<- as.data.frame(do.call(rbind,rowsDTJTransDatList))

# Only SRCE 530
rowsDTJTransDat530 <- as.data.frame(rowsDTJTransDat[rowsDTJTransDat$V3 == '530' | rowsDTJTransDat$V4 == '530' | rowsDTJTransDat$V5 == '530',]) # only 530 transactions 

# Fill in NAs in last column for formatting
off.set.rows.530 <- c(which(is.na(rowsDTJTransDat530[ ,c(ncol(rowsDTJTransDat530)-1,ncol(rowsDTJTransDat530))])))
rowsDTJTransDat530[off.set.rows.530, 5:ncol(rowsDTJTransDat530)] <- rowsDTJTransDat530[off.set.rows.530, 3:(ncol(rowsDTJTransDat530)-2)] # shift rows after the third column (starting 4)
rowsDTJTransDat530 <- rowsDTJTransDat530[!is.na(rowsDTJTransDat530$V1),]
rowsDTJTransDat530[rowsDTJTransDat530$V3 == "PARTICIPATED" & is.na(rowsDTJTransDat530$V12), 4:ncol(rowsDTJTransDat530)] <- rowsDTJTransDat530[rowsDTJTransDat530$V3 == "PARTICIPATED" & is.na(rowsDTJTransDat530$V12), 3:(ncol(rowsDTJTransDat530)-1)]
rowsDTJTransDat530 <- rowsDTJTransDat530[,-c(3,4,8,10)]

# Subset for proper TRAN CDs
  # DEBIT - 750: Advance, 800: Reversal
  # CREDIT - 610: Regular Payment, 650: Regular Payoff, 660: Special Payment, 680: Fee Payment, 619: ACH, 620: Payment  

rowsDTJTransDat530 <- subset(rowsDTJTransDat530,rowsDTJTransDat530$V9 %in% c("750","800", "610", "650", "660", "680", "619", "620", "770"))
colnames(rowsDTJTransDat530) <- c("AccountNumber1", "AccountNumber2", "SRCE", "SourceControl", "Amount", "TRAN", "EFF DATE", "PAID DATE")

# Need to append Name
paste0(rowsDTJTransDat530$AccountNumber1," ", rowsDTJTransDat530$AccountNumber2)

rowsDTJdatCorpNamesList <- list()
for (i in 1:nrow(rowsDTJdatCorpTrans)){
  rowsDTJdatCorpNamesList[[i]] <- if (nchar(unlist(strsplit(trimws(rowsDTJdatCorpTrans$Cols[i]), \\s+))[1]) == 11){
    rowsDTJdatCorpTrans[i+1,] # Want to make sure we capture name if row above has first element in row/string has 11 characters 
  } 
}
rowsDTJdatCorpNamesCln <- as.data.frame(do.call(rbind,rowsDTJdatCorpNamesList))
colnames(rowsDTJdatCorpNamesCln) <- "Cols"

# Last two columns in Names will always be last two, but first amount will be dynamic
# Names - remove rows that don't start with A 

rowsDTJNamesDatList <- list()
for (i in 1:nrow(rowsDTJdatCorpNamesCln)){
  rowsDTJNamesDatList[[i]] <- as.data.frame(cbind(
    str_c(unlist(strsplit(trimws(rowsDTJdatCorpNamesCln$Cols[i]), \\s+))[-c(length(unlist(strsplit(trimws(rowsDTJdatCorpNamesCln$Cols[i]), \\s+))),
                                                                         length(unlist(strsplit(trimws(rowsDTJdatCorpNamesCln$Cols[i]), \\s+)))-1)],
          collapse=" "),
    unlist(strsplit(trimws(rowsDTJdatCorpNamesCln$Cols[i]), \\s+))[length(unlist(strsplit(trimws(rowsDTJdatCorpNamesCln$Cols[i]), \\s+)))-1],
    unlist(strsplit(trimws(rowsDTJdatCorpNamesCln$Cols[i]), \\s+))[length(unlist(strsplit(trimws(rowsDTJdatCorpNamesCln$Cols[i]), \\s+)))]
  ))
}
rowsDTJNamesDat <- as.data.frame(do.call(rbind,rowsDTJNamesDatList))
rowsDTJNamesDat <- as.data.frame(cbind(rowsDTJNamesDat$V1, rowsDTJTransDat[,1:2]))
colnames(rowsDTJNamesDat) <- c("Name","AccountNumber1","AccountNumber2")

rowsDTJdat530 <- left_join(rowsDTJTransDat530,rowsDTJNamesDat, by=c('AccountNumber1'='AccountNumber1', 'AccountNumber2'='AccountNumber2'))
rowsDTJdat530 <- unique(rowsDTJdat530)
rowsDTJdat530 <- as.data.frame(rowsDTJdat530 %>% group_by(rowsDTJdat530[,1:8]) %>% top_n(n=1))

rowsDTJdat530 <- data.frame(rowsDTJdat530[,9],rowsDTJdat530[,1:8])
colnames(rowsDTJdat530)[1] <- "AccountName"

## FIX ISSUE WITH A324 IN TOTALS TO GL AND INCLUDE? ##
# Carry in rec: TRAN CONTROL NO U001 at top of DTJ, SRCE 999 TRAN 000 in section 3 if not already there (new account)

### NON-POSTED RESULTS - necessary? ###
#install.packages(("openxlsx"))
library(openxlsx)
user_home <- Sys.getenv("USERPROFILE")
#change2
# gl_balancing_path <- file.path(user_home, "Desktop", "GL_BALANCING")
gl_balancing_path <- file.path("/app", "GL_BALANCING")

cat("GL_BALANCING path:", gl_balancing_path, "\n")

# Check if the dir exists
if (!dir.exists(gl_balancing_path)) {
  dir.create(gl_balancing_path, recursive=TRUE)
  cat("Directory created at: ",gl_balancing_path, "\n")
} else {
  cat("Directory already exists at: ", gl_balancing_path,"\n")
}

# Funciton to construct a full path file and save a df
save_csv_to_gl_balancing <- function(df, file_name) {
  #setwd(gl_balancing_path)
  
  # Full path file
  full_path_file <- file.path(gl_balancing_path, file_name)
  
  # Write the df to csv
  write.csv(df, full_path_file, row.names = FALSE)
  cat("File saved to:", full_path_file, "\n")
}

# Save each df as a separate csv file
save_csv_to_gl_balancing(rowsGLdat2, "rowsGLdat2.csv")
save_csv_to_gl_balancing(rowsDTJdat530, "rowsDTJdat530.csv")

# Clean the MEMO CODE in the file
rowsOLDbotdat_clean <- na.omit(rowsOLDbotdat, subset = "MEMO CODE")
save_csv_to_gl_balancing(rowsOLDbotdat_clean, "rowsOLDbotdat_clean.csv")

save_csv_to_gl_balancing(rowsOLDtopdat, "rowsOLDtopdat.csv")
save_csv_to_gl_balancing(rowsREJdatAmounts, "rowsREJdatAmounts.csv")
save_csv_to_gl_balancing(rowsOLDdatAmounts, "rowsOLDdatAmounts.csv")

print(gl_balancing_path)
