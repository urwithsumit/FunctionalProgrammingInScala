
/**
  * Perform multiple Regex replacement operations on a String.
  *
  * @param s
  * @param rgx
  * @return
  */
def clean(s: String, rgx: Map[String, String] = Map(("\\s+" -> "_"))) =
  rgx.foldLeft(s)((r, c) => r.replaceAll(c._1, c._2).trim.toUpperCase)

clean("PAGE -  _ _ - - _        BAG -___ - - - - _",
  Map("-" -> " ",
    "\\s+" -> "_",
    "\\_+" -> "_")
)