matach_book_download = function(url = 'https://school.kotar.cet.ac.il/KotarApp/Viewer.aspx?nBookID=104717391',
                                book_name = 'kesem1',
                                horizontal_res = 1280,
                                vertical_res = 8000) {

options(Ncpus = parallel::detectCores()-2)
lbs <- c('RSelenium', 'base64enc', 'magick', 'progress', 'png', 'magrittr', 'dplyr', 'data.table', 'grid')
req <- substitute(suppressMessages(require(x, character.only = TRUE)))
sapply(lbs, function(x) eval(req) || {install.packages(x, quiet=TRUE); eval(req)})

horizontal_res = horizontal_res
vertical_res = vertical_res

# Sample url's (SHORT and LONG)
#url = 'https://school.kotar.cet.ac.il/KotarApp/Viewer.aspx?nBookID=104717391'
#url = 'https://school.kotar.cet.ac.il/KotarApp/Viewer.aspx?nBookID=94485083'

url = url
book_name = book_name

chrome_opts <- list(
                  chromeOptions = list(
                                   args = c(
                                            paste0('--window-size=', horizontal_res, ',',vertical_res)
                                            , '--disable-browser-side-navigation'
                                            , '--dns-prefetch-disable'
                                            , '--headless'
                                            , '--disable-gpu'
                                            )
                                   # Block images:
                                   , prefs = list("profile.managed_default_content_settings.images" = 2)
))

# Terminate running ports
system("lsof -ti:4567 -sTCP:LISTEN | xargs kill")

rD <- rsDriver(browser = 'chrome', extraCapabilities = chrome_opts, verbose = FALSE)
remDr <- rD[['client']]

remDr$navigate(url)
webElem <- remDr$findElement("css", "body")
# Not needed here:
#web_text <- remDr$findElement("css", "body")$getElementText()
#web_source = unlist(remDr$getPageSource())

page_length = remDr$executeScript("return document.body.scrollHeight")[[1]]+1
image_list = list()

Sys.sleep(3)
invisible(remDr$executeScript(paste0('window.scrollTo(0, ', min((page_length-1), vertical_res), ');')))
Sys.sleep(3)

webElem$sendKeysToElement(list(key = "home"))


Sys.sleep(10)

df_list = list()
while(TRUE){
        # Take screenshot and remove alpha channel
        df_list[[1]] = try(image_data(image_read(base64decode(toString(remDr$screenshot()))))[1:3,,])
        if (!is(df_list[[1]], 'try-error')) break else Sys.sleep(3)
        
}

list1 = list(); list1[[1]] = list(); list1[[2]] = list()
for (color in seq(3)) {
    for (column1 in 1:dim(df_list[[1]])[2]) {
      if (!identical('b2', paste0(unique(df_list[[1]][color,column1,])))) break}
    for (column2 in dim(df_list[[1]])[2]:1) {
      if (!identical('b2', paste0(unique(df_list[[1]][color,column2,])))) break}

  list1[[1]][color] = column1
  list1[[2]][color] = column2
}

min_col = min(unlist(list1[[1]]))
max_col = max(unlist(list1[[2]]))

df_list[[1]] = df_list[[1]][,min_col:max_col,]
page_df = list()
page_df[[1]] = data.frame(rownum = seq(dim(df_list[[1]])[3]), 
                     totals = ifelse(apply(df_list[[1]][1,,], 2, function(v) identical(paste0(unique(v)),'b2'))==TRUE &
                                     apply(df_list[[1]][1,,], 2, function(v) identical(paste0(unique(v)),'b2'))==TRUE & 
                                     apply(df_list[[1]][1,,], 2, function(v) identical(paste0(unique(v)),'b2'))==TRUE,
                                     1,0))
                                    
print(c(
        remDr$executeScript('return window.pageYOffset;')[[1]]+1, 
        min(remDr$executeScript('return window.pageYOffset;')[[1]]+vertical_res, page_length)
        ))
print('printed')

page_length/vertical_res

pb <- progress_bar$new(
  format = "  capturing :what [:bar] :percent eta: :eta",
  clear = FALSE, total = max(1,(floor(page_length/vertical_res)-1)), width = 90)

if ((page_length/vertical_res) > 1) {
  for (i in 1:max(1,(floor(page_length/vertical_res)-1))) {
      
      
      remDr$executeScript(paste0('window.scrollTo(0, ', 
                               i*vertical_res-1, 
                               ');'))
      
      start_pt = remDr$executeScript('return window.pageYOffset;')[[1]]+2
      end_pt = remDr$executeScript('return window.pageYOffset;')[[1]]+vertical_res+1
      
      Sys.sleep(10)
      if (pb$.__enclos_env__$private$total > 1) {
      pb$tick(tokens = list(what = paste0(i, ' from ', (floor(page_length/vertical_res)-1), 
                                          ', sections: ', start_pt, '-', end_pt)))
      }
      
      while(TRUE){
        df_list[[(1+i)]] = try(image_data(image_read(base64decode(toString(remDr$screenshot()))))[1:3,min_col:max_col,])
        if(!is(df_list[[(1+i)]], 'try-error')) break else Sys.sleep(3)
      }
      
      page_df[[i+1]] = data.frame(rownum = seq(dim(df_list[[1]])[3])+(i*2*vertical_res), 
                                  totals = ifelse(apply(df_list[[1+i]][1,,], 2, function(v) identical(paste0(unique(v)),'b2'))==TRUE &
                                                 apply(df_list[[1+i]][1,,], 2, function(v) identical(paste0(unique(v)),'b2'))==TRUE & 
                                                 apply(df_list[[1+i]][1,,], 2, function(v) identical(paste0(unique(v)),'b2'))==TRUE,
                                                 1,0))
      
  }
}

if ((floor(page_length/vertical_res)-1 > 0) &
    (page_length/vertical_res - floor(page_length/vertical_res) > 0)) {
  
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(10)
  nums = ceiling(page_length/vertical_res)
  
  while(TRUE){
    df_list[[nums]] = try(image_read(base64decode(toString(remDr$screenshot()))))
    if(!is(df_list[[nums]], 'try-error')) break else Sys.sleep(3)
  }
  
  df_list[[nums]] = image_data(image_crop(df_list[[nums]], geometry_area(y_off=image_info(df_list[[nums]])[3][[1]]*
                                ((floor(page_length/vertical_res)+1) - (page_length/vertical_res)))))[1:3,min_col:max_col,]
  page_df[[nums]] = data.frame(rownum = seq(dim(df_list[[nums]])[3])+((nums-1)*dim(page_df[[1]])[1]), 
                               totals = ifelse(apply(df_list[[nums]][1,,], 2, function(v) identical(paste0(unique(v)),'b2'))==TRUE &
                                                 apply(df_list[[nums]][1,,], 2, function(v) identical(paste0(unique(v)),'b2'))==TRUE & 
                                                 apply(df_list[[nums]][1,,], 2, function(v) identical(paste0(unique(v)),'b2'))==TRUE,
                                                 1,0))
}

remDr$close()
invisible(rD$server$stop())

invisible(gc())  

page_df = data.table::rbindlist(page_df)

pages_func = function(i) {
  page_df %>%
          mutate(runs = data.table::rleid(totals)) %>%
          group_by(runs) %>% 
          mutate(totals2 = n(),
                 p_br = ifelse(totals2>i & totals==1, 1,0)) %>%
          ungroup() %>%
          mutate(runs2 = data.table::rleid(p_br)) %>% 
          filter(p_br==0) %>% 
          group_by(runs2) %>% 
          summarize(first = min(rownum), last = max(rownum))
}

for (i in seq(nrow(page_df))) {
  pages = pages_func(i)

  if (i==1) {counter=nrow(pages)}
  if ((i > 1 & nrow(pages) < counter) | 
      nrow(pages) == 1) {
        pages = pages_func(i-1)
        break
}}
  
n = nrow(pages)
num_cols = ncol(df_list[[1]])

df_list = sapply(df_list, image_read)
df_list2 = list()
for (i in seq(length(df_list))) {df_list2 = append(df_list2, df_list[[i]])}
df_list = NULL
df_list2 = image_append(df_list2, stack=TRUE)[[1]]

pb <- progress_bar$new(
  format = "  printing :what [:bar] :percent eta: :eta",
  clear = FALSE, total = n, width = 90)

# Determine page ratio based on the first page
page_ratio = (pages[[1,3]]-pages[[1,2]]+1)/(num_cols)

# Set page size ('letter') and specify no margins
pdf(paste0(book_name, '.pdf'), width = 11/page_ratio, height = 11)
par(mai=c(0,0,0,0))
for (i in 1:n) {
  if (n > 1) {pb$tick(tokens = list(what = paste0(i, ' from ', n)))}
  grid.raster(image_read(df_list2[,,pages[[i,2]]:pages[[i,3]]]))
  if (i < n) plot.new()
}
dev.off()

}
