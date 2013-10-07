
def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
try { f(param) } finally { param.close() }

def appendToFile(fileName:String, textData:String) =
  using (new java.io.FileWriter(fileName, true)) { 
    fileWriter => using (new java.io.PrintWriter(fileWriter)) {
      printWriter => printWriter.println(textData)
    }
  }

val url = """http://www.amazon.com/Killing-Jesus-Bill-OReilly/product-reviews/0805098542/ref=dp_top_cm_cr_acr_txt?showViewpoints=1"""

def getReviews( stream : Stream[String] )( implicit acc : List[(String,String)] = List() )
	: List[(String,String)] = {
	if(stream.isEmpty) acc else {
		val nStream = stream.dropWhile(!_.contains("name"))
		if(nStream.isEmpty) acc else {
			val nm = nStream.head.dropWhile(_!='=').drop(2).takeWhile(_!='"')
			val sStream = nStream.dropWhile(!_.contains("out of 5 stars"))
			if(sStream.isEmpty) acc else {
				val scrTxt = sStream.head
				val idx = scrTxt.indexOf("title") + 7
				val scr = scrTxt.substring( idx, idx +3 )

				val pStream = sStream.dropWhile(!_.contains("profile"))
				if (pStream.isEmpty) acc else {
					val profileTxt = pStream.head
					val idx = profileTxt.indexOf("profile") + 8
					val profile = profileTxt.drop(idx).takeWhile(_!='>')
					val newStream = pStream.dropWhile(!_.contains("comment"))
					getReviews(newStream)( (profile,scr) :: acc )
				}
			}
		}
	}
}


def getNextReviewURL( currentPage : Stream[String] ) : Option[String] = {
	val htmlLine = currentPage.filter( x=> x.contains("Previous") && x.contains("Next") && x.contains("href") ).distinct
	if(htmlLine.size !=1 ) None else { 
		val hrefs = htmlLine.head.split("href")
		val href = hrefs.filter(_.contains(">Next"))
		if(href.size !=1 ) None else {
			val hrefTxt = href.head
			Some(hrefTxt.dropWhile(_!='"').drop(1).takeWhile(_!='"'))
		}
	}
}

def getAllReviews( stream : Stream[String] )(implicit acc : List[(String,String)] = List() ) : List[(String,String)] = {
	val stream2 = stream.dropWhile(!_.contains("""table id="productReviews""""))
	val rev1 = getReviews( stream2 )( acc )
	getNextReviewURL( stream2 ) match {
		case None => acc
		case Some(nextURL) => getAllReviews( getPg(nextURL).toStream )(rev1)
	}
}

def hasReviews( stream : Stream[String] ) : Boolean =
{
	!stream.foldLeft(false)((acc,e) => acc || e.contains("There are no customer reviews yet"))
}


def getBSReviews( bs : List[String] ) = {
  
  val rfn : Stream[String] => List[(String,String)]= (x:Stream[String]) => if(hasReviews(x)) getAllReviews(x) else List() 
  val fn = getPg andThen(_.toStream) andThen rfn
  bs.map( x=> { appendToFile(loc, x); val r = fn(x); r.foreach( y=>appendToFile(loc, y._1 +":"+ y._2) ); r } )

}
