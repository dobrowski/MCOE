

library(tidyverse)
library(here)



options(scipen = 9999)

pub.sch <- read.delim(here("inst","data","pubschls (1).txt"))


logo.names <- tibble::tribble(
    ~DistCode,~logoname,
    "10272", "logo/MCOE.png",
    "66076", "logo/Lagunita.PNG",
    "", "logo/BSCS.png",
    "", "logo/ISM.jpg",
    "", "logo/LFLCS.png",
    "", "logo/MBCS.png",
    "66142", "logo/SCESD.png",
    "66159", "logo/SUHSD.png",
    "66167", "logo/SanAntonioUSD.jpg",
    "66209", "logo/SoledadUSD.jpg"
)


mry.dist <- pub.sch %>%
    filter(str_detect(County,"Monterey")) %>%
    mutate(cds = as.character(CDSCode),
           DistCode = str_sub(cds,3,7)
           ) %>%
    left_join(logo.names)


usethis::use_data(mry.dist, overwrite = TRUE)
usethis::use_data(mry.dist, overwrite = TRUE, internal = TRUE)