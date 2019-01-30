# Kotar-ebook-scarpe-convert
R code to download free ebooks from school.kotar.cet.ac.il (using RSelenium) and convert to pdf with dynamic page splits

Example:

url = 'https://school.kotar.cet.ac.il/KotarApp/Viewer.aspx?nBookID=94485083'

book_name = 'kesem1'

- The following will launch RSelenium, then scroll down the page and capture the ebook, which is being generated dynamically
- The image is then scanned for page splits (i.e., gray borders), and then saves the book to pdf

matach_book_download(url, book_name)

