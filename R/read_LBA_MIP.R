
#' Function to read and separate data from LBA-MIP experiment.
#' @param path Directory that contains  LBA_MIP's zip
#' @export 
#' @importFrom dplyr %>% bind_rows as.tbl 
LBA_MIP_read <- function(path){

  zips.files <- list.files(path = path, pattern = ".zip$",full.names = TRUE)
  sapply(zips.files, unzip, exdir = path,overwrite = TRUE)
  
    lapply(list.files(path = paste0(path),full.names = T ) %>% 
             grep(pattern = ".zip$",invert = TRUE,value = TRUE) %>%
             list.files(pattern = ".txt$",full.names = TRUE), 
           read.table, header = FALSE, skip = 16,
           col.names = read.table(list_txt[1],header = FALSE,skip = 12,nrows = 1,sep = ",") %>% 
             setNames(NULL) %>% as.vector() %>% t %>% c) %>%
    setNames(  sapply(basename(list_txt) %>% strsplit(split = "uaz.") ,function(i) i[1]) ) %>%
    bind_rows(.id = "site" ) %>%
    as.tbl()

}
