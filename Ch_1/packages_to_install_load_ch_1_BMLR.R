
packs =c("knitr"
         ,"gridExtra"
         ,"GGally"
         ,"kableExtra"
         ,"jtools"
         ,"rsample"
         ,"broom"
         ,"tidyverse")

install_ch_1_bmlr = lapply(X = packs, FUN = install.packages, character.only = FALSE)

load_ch_1_bmlr = lapply(X = packs, FUN = library, character.only = TRUE)