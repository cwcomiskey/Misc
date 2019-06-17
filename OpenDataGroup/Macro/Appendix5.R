# Recreate Appendix 5 table ======
# library(pdftools); library(readr); library(stringr); library(reshape2)
for(i in 1:14){
  if(i==1){
    A5 <- pdf_text("Appendix5.pdf")
    A <- character
  }
  A_i <- read_lines(A5[i])
  A <- c(A, A_i)
  if(i==14) rm(A5, A_i, i)
}
# A <- str_trim(A)
# A <- A[!str_detect(A, "Appendix")]
# A <- A[-1]
# ==== Data prep ==== #

# str_split_fixed("FA   Cereals and cereal products", " ", 2)

A5 <- read_table("Appendix5.txt", col_names = FALSE)
A5 <- as.data.frame(str_split_fixed(A5$X2, " ", 2), stringsAsFactors = FALSE)
names(A5) <- c("Code", "Category")
A5 <- mutate(A5, Code = str_sub(Code, 2), Category = str_sub(str_trim(Category), 1, -2))

fwrite(A5, "Appendix5.txt", sep = " ")
