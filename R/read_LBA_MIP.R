
#' Function to read and separate data from LBA-MIP experiment.
#' @param path Directory that contains  LBA_MIP's zip
#' @export 
#' @importFrom dplyr %>% bind_rows as.tbl 
LBA_MIP_read <- function(path){

  zips.files <- list.files(path = path, pattern = ".zip$",full.names = TRUE)
  sapply(zips.files, unzip, exdir = path,overwrite = TRUE)
  
  list_txt <- list.files(path = paste0(path),full.names = T ) %>% 
    grep(pattern = ".zip$",invert = TRUE,value = TRUE) %>%
    list.files(pattern = ".txt$",full.names = TRUE)
    
  lapply(list_txt, read.table, header = FALSE, skip = 16,
         col.names = read.table(list_txt[1],header = FALSE,skip = 12,nrows = 1,sep = ",") %>% 
           setNames(NULL) %>% as.vector() %>% t %>% c) %>%
    setNames(  sapply(basename(list_txt) %>% strsplit(split = "uaz.") ,function(i) i[1]) ) %>%
    bind_rows(.id = "site" ) %>%
    as.tbl() %>%
    mutate(date = as.Date(DoY -1, origin = as.Date(paste0(Year,"-01-01")))) %>%
    unite(date, date, Hour, Minute,sep = " ") %>%
    mutate(date = fast_strptime(date, format = "%Y-%m-%d %H %M")) %>%
    group_by(Year) %>%
    mutate(site = gsub(x = site,pattern = as.character(paste0("_",unique(Year))) , replacement = "") ) %>%
    ungroup()
  

}
