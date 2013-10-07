val getPg = (s:String) => { println(s); io.Source.fromURL(s).getLines }
val urls = (1 to 5).map ( x => """http://www.amazon.com/best-sellers-books-Amazon/zgbs/books/ref=zg_bs_books_pg_""" + x + """?_encoding=UTF8&pg=""" + x ).toList
val htmls = urls.map(getPg)
val books = htmls.flatMap( _.filter( x=>x.startsWith("http")))
val bs = books.distinct

