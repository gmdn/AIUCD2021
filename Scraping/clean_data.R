library(dplyr)
library(stringr)

my_df <- readRDS("./my_df.rds")

my_df_extended <- my_df %>%
  mutate(technical = NA) %>%
  mutate(popular = NA)

#str(my_df)

# 1) separare la variabile Text in due (carattere separazione ",")
elements <- str_split(my_df_extended$text, ",")

# 1 bis) check the elements with length differnt than 2
correct_elements <- which(sapply(elements, length) == 2)

# 2) trovare quale e' il termine popolare (trovare la sequenza (pop))
#log_el <- lapply(X = elements, FUN = grep, pattern = "pop")
log_el <- unlist(lapply(X = elements,   #per cosa sta log_el?
                        FUN = function(x) {
                          return(grepl(x[1], pattern = "pop")) #selezionando solo il primo elemento di "elements" la funzione si applica solo a quello, corretto? Per far s? che venga applicata a tutti gli elementi in successione non andrebbe usato un for loop?
                        }
)
)

pop_terms <- sapply(elements[log_el], `[[`, 1) #qui non capisco il secondo e il terzo argomento della funzione sapply, ovvero `[[` e  1. Lo stesso vale per la funzione seguente, con `[[` e  2. A cosa servono/si riferiscono?
tech_terms <- sapply(elements[log_el], `[[`, 2)

# popular elements are in position 1 in the list
pop_correct_elements_first <- unlist(lapply(X = elements[correct_elements],   #per cosa sta log_el?
                                            FUN = function(x) {
                                              return(grepl(x[1], pattern = "pop")) #selezionando solo il primo elemento di "elements" la funzione si applica solo a quello, corretto? Per far s? che venga applicata a tutti gli elementi in successione non andrebbe usato un for loop?
                                            }
)
)

head(pop_correct_elements_first)

# popular elements are in position 2 in the list
pop_correct_elements_second <- unlist(lapply(X = elements[correct_elements],   #per cosa sta log_el?
                                             FUN = function(x) {
                                               return(grepl(x[2], pattern = "pop")) #selezionando solo il primo elemento di "elements" la funzione si applica solo a quello, corretto? Per far s? che venga applicata a tutti gli elementi in successione non andrebbe usato un for loop?
                                             }
)
)

head(pop_correct_elements_second)

# extract content 
pop_terms <- sapply(elements[correct_elements][pop_correct_elements_first], `[[`, 1)
tech_terms <- sapply(elements[correct_elements][pop_correct_elements_first], `[[`, 2)

my_df_extended$popular[correct_elements][pop_correct_elements_first] <- pop_terms
my_df_extended$technical[correct_elements][pop_correct_elements_first] <- tech_terms


# extract content 
pop_terms <- sapply(elements[correct_elements][pop_correct_elements_second], `[[`, 2)
tech_terms <- sapply(elements[correct_elements][pop_correct_elements_second], `[[`, 1)

my_df_extended$popular[correct_elements][pop_correct_elements_second] <- pop_terms
my_df_extended$technical[correct_elements][pop_correct_elements_second] <- tech_terms

# rimuovi simbolo newline "\n"
my_df_extended$popular <- str_replace_all(my_df_extended$popular, pattern = "\n", replacement = " ")
my_df_extended$technical <- str_replace_all(my_df_extended$technical, pattern = "\n", replacement = " ")

# remove (pop)
my_df_extended$popular <- str_replace_all(my_df_extended$popular, pattern = "\\(pop\\)", replacement = "")

# rimuovi spazi ad inizio/fine stringa
my_df_extended$popular <- str_trim(my_df_extended$popular)
my_df_extended$technical <- str_trim(my_df_extended$technical)

head(my_df_extended)

dim(my_df_extended)

my_df_extended %>%
  group_by(language) %>%
  summarise(count = n())

saveRDS(my_df_extended, file = "my_df_extended.rds")
