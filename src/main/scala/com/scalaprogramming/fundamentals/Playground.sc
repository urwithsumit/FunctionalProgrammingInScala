
/**
  * Perform multiple Regex replacement operations on a String.
  *
  * @param s
  * @param rgx
  * @return
  */
def clean(s: String, rgx: Map[String, String] = Map(("\\s+" -> "_"))) =
  rgx.foldLeft(s)((r, c) => r.replaceAll(c._1, c._2).trim.toUpperCase)

List("HEAP RETENTION - AUG 31", "PAGE - BERGEN", "PAO - MID CNTY APPL DAY", "PAO - MID CNTY APPL DAY2",
  "PAO - MOR CNTY APPL DAY", "PAO - UNION CNTY APPL DAY").map(x =>
  println(clean(x,
    Map("-" -> " ",
      "\\s+" -> "_",
      "\\_+" -> "_"))))