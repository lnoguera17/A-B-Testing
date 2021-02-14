#install.packages("funneljoin")
library(funneljoin)
library(tidyverse)



registered
landed

landed %>% after_inner_join(registered,
                            by_user = "user_id",
                            by_time = "timestamp",
                            type = "any-any"
                            )


