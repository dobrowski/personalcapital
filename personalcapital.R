
### References ----

# https://rpubs.com/johndharrison/RSelenium-Basics
# https://ropensci.org/tutorials/rselenium_tutorial/
# https://github.com/yusuzech/r-web-scraping-cheat-sheet


### Open packages ----

library(RSelenium)
library(rvest)
# library(httr)
library(tidyverse)
library(lubridate)
library(keyring)


###  Set variables -----

url <- "https://home.personalcapital.com/page/login/app#/portfolio/allocation"
# email <- key_get("personal capital username")
# pass <- key_get("personal capital")


### Start selenium browser session ----

driver <- rsDriver(chromever = "74.0.3729.6")
remote_driver <- driver[["client"]]
remote_driver$open()

# Go to websire but not log in page so that I can set cookies
remote_driver$navigate(url = "https://personalcapital.com")


# Set cookies from previously 2-Factor Authenticated session
# my_cookies <- remote_driver$getAllCookies()

my_cookies <- read_rds("my_cookies.rds")

for (i in 1:15) {
  remote_driver$addCookie(name = my_cookies[i][[1]]$name,
                          value = my_cookies[i][[1]]$value,
                          domain = my_cookies[i][[1]]$domain,
                          httpOnly = my_cookies[i][[1]]$httpOnly,
                          expiry = my_cookies[i][[1]]$expiry,
                          secure = my_cookies[i][[1]]$secure
  )
}

# Login for this session (it seems to remember me and not need to log in again, but that might change)
remote_driver$navigate(url = "https://home.personalcapital.com")

# Find field to enter username and click button
webElem <- remote_driver$findElement(using = 'name', value = "username")
webElem$sendKeysToElement(list(key_get("personal capital username")))

webElem <- remote_driver$findElement(using = 'tag name', value = 'button')
webElem$clickElement()

# Find field to enter password and click button
webElem <- remote_driver$findElement(using = 'name', value = "passwd")
webElem$sendKeysToElement(list(key_get("personal capital")))

webElem <- remote_driver$findElement(using = 'css selector', value = '#form-password > fieldset > div.form-actions > button.btn.btn-primary')
webElem$clickElement()



###  Go to page with data tables I want 

remote_driver$navigate(url = url)

# webElem <- remote_driver$findElement(using = 'id', value = "DataTables_Table_0")
# webElemtxt <- webElem$getElementAttribute("outerHTML")[[1]]
# table.all <- webElemtxt %>% read_html() %>% html_table() %>% as.list()


# remote_driver$navigate(url = "https://home.personalcapital.com/page/login/app#/portfolio/allocation/us-stocks")
# 
# webElem <- remote_driver$findElement(using = 'id', value = "DataTables_Table_1")
# webElemtxt <- webElem$getElementAttribute("outerHTML")[[1]]
# table.us.stocks <- webElemtxt %>% read_html() %>% html_table() %>% as.data.frame()



#  Setup for scraping 

pages <- c("",
           "/intl-stocks",
           "/us-stocks",
           "/alternatives",
           "/us-bonds",
           "/intl-bonds")


pageindex <- tibble(num = 0:5, pages = pages, tables = NA )

full <- data.frame()


# Actually scrape all the tables 

for( x in 1:6) {
  remote_driver$navigate(url = paste0(url,pageindex[x,2]))
  
  webElem <- remote_driver$findElement(using = 'id', value = paste0("DataTables_Table_",pageindex[x,1]))
  webElemtxt <- webElem$getElementAttribute("outerHTML")[[1]]
  table <- webElemtxt %>% read_html() %>% html_table() %>% as.data.frame()
  
  table$Var.1 <- x
  
  pageindex[x,3] <- table

  full <- bind_rows(full, table)
  }


# 
# BACKUP 
# for( page in pages) {
#   remote_driver$navigate(url = paste0(url,"/",page))
#   
#   webElem <- remote_driver$findElement(using = 'id', value = paste0("DataTables_Table_",n))
#   webElemtxt <- webElem$getElementAttribute("outerHTML")[[1]]
#   table.all <- webElemtxt %>% read_html() %>% html_table() %>% as.data.frame()
#   
#   n <- n+1
# }

# saving my cookies for next time 
write_rds(my_cookies, "my_cookies.rds")
my_cookies2 <- remote_driver$getAllCookies()
write_rds(my_cookies2, "my_cookies2.rds")

### Stop browser ---- 
driver$server$stop()



### Manipulate data frames ----
new.allocations <- pageindex %>%
  mutate(cat = str_remove(pages,"/"),
         Var.1 = row_number(),
         date = today()) %>%
  select(Var.1, cat, date) %>%
  right_join(full) %>%
  mutate(Allocation = as.numeric(str_remove( X..Total, "%")),
         value = gsub('\\$', '', Value),
         value = as.numeric(str_remove( value, ","))
     ) %>%
  select(-Var.6,-X1.Day.., -X..Total, -Value)



allocations <- read_csv("allocations.csv") 
allocations <- allocations %>% bind_rows(new.allocations) %>% distinct()
write_csv(allocations, "allocations.csv")



