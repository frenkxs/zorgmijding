## code to prepare the list of eligible contacts and save it as internal data in the library
path_contact.types <- "D:/Research/zorgmijding_package/data/contactsoort.xls"

consult_contact <- readxl::read_excel(path = path_contact.types)[[1]]
consult_contact <- stats::na.omit(consult.contact)

usethis::use_data(consult_contact, overwrite = TRUE)
