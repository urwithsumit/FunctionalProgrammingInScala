package com.scalaprogramming.work

import java.io.File

import scala.io.Source

object FileMergeApp extends App {

  val Templates = List("3DC",
    "ACTIVE_AR",
    "CRITICAL_CARE_MESSAGE",
    "DLQNAMEACCT",
    "Email_By_Portion",
    "Email_Main_Body",
    "Email_PAGE_2018",
    "Email_Strom_Message",
    "FC_schd",
    "hard_Inactive_AR",
    "HEAP EMERGENCY - AUG31",
    "HEAP RETENTION - AUG 31",
    "MISSED FSP 2018",
    "PAGE - BERGEN",
    "PAO - MID CNTY APPL DAY",
    "PAO - MID CNTY APPL DAY2",
    "PAO - MOR CNTY APPL DAY",
    "PAO - UNION CNTY APPL DAY",
    "soft_inactive_AR",
    "Soft_Resi_AR",
    "Test_Main_Body")

  def clean(s: String) = s.trim.replace("\"", "")

  val InputFileLocation = new File("C:\\v3eclipse\\PSEG\\Reports_11Nov\\Consolidated\\Part2")
  val OutputFile = new File("C:\\v3eclipse\\PSEG\\Reports_11Nov\\InstantInfo_Par2.csv")
  //Create a new File with each run.
  /*
    OutputFile.deleteOnExit()

    val bw = new BufferedWriter(new FileWriter(OutputFile))

    InputFileLocation.listFiles.map { c =>
      println(s"Processing: ${c}")
      Source.fromFile(c).getLines().foreach {
        line =>
          if (line.contains("Instant Info")) {
            bw.write(line)
            bw.write("\n")
          }
      }
    }

    bw.flush()
  */

  /*

    val NotificationCategory = Source.fromFile(OutputFile).getLines().foldLeft(Map[String, Set[String]]())((r, c) => {
      if (c.contains("Instant Info")) {
        val arr = clean(c).split(",")
        r.get(arr(6)) match {
          case Some(x) if (arr(34) == "EMAIL" || arr(34) == "VOICE" || arr(34) == "SMS") => r.updated(arr(6), x + arr(34))
          case _ => r.updated(arr(6), Set(arr(5)))
        }
      } else r

    })

    println(s"NotificationCategory Record Size: ${NotificationCategory.size}")
    println(NotificationCategory.foreach(c => println(s" ${clean(c._1)} = ${c._2.mkString(",")}")))
  */


  /**
    * This covers Campaign to Template selected for Migration.
    */
  /*
    val CampaignTemplateCategory = Source.fromFile(OutputFile).getLines().foldLeft(Map[String, Set[String]]())((r, c) => {
      val Key = 51
      val Value = 37
      if (c.contains("Instant Info")) {
        val arr = clean(c).split(",")
        r.get(arr(Key)) match {
          case Some(x) if Templates.contains(arr(Value)) => {
            r.updated(arr(Key), x + arr(Value))
          }
          case _ => if (Templates.contains(arr(Value))) r.updated(arr(Key), Set(arr(Value))) else r
        }
      } else r
    })

    println(s"CampaignTemplateCategory Record Size: ${CampaignTemplateCategory.size}")
    println(CampaignTemplateCategory.foreach(c => println(s" ${clean(c._1)} = ${c._2.mkString(",")}")))

  */

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * This covers Campaign to all Template send since December 2018 to 8 Nov 2019
    */
  /* val AllCampaignTemplateCategory = Source.fromFile(OutputFile).getLines().foldLeft(Map[String, Set[String]]())((r, c) => {
     if (c.contains("Instant Info")) {
       val arr = clean(c).split(",")
       r.get(arr(6)) match {
         case Some(x) => {
           r.updated(arr(6), x + arr(37))
         }
         case _ => r.updated(arr(6), Set(arr(37)))
       }
     } else r
   })

   println(s"AllCampaignTemplateCategory Record Size: ${AllCampaignTemplateCategory.size}")
   println(AllCampaignTemplateCategory.foreach(c => println(s" ${clean(c._1)} = ${c._2.mkString(",")}")))

 */
  ////////////////////////////////////////////////////////
  /**
    * This covers Campaign to all available templates..
    */
  /*
    val GetConsumerId = Source.fromFile(OutputFile).getLines().foldLeft(Map[String, Set[String]]())((r, c) => {
      val Key = 6
      val Value = 11
      if (c.contains("Instant Info")) {
        val arr = clean(c).split(",")
        r.get(arr(Key)) match {
          case Some(x) => {
            r.updated(arr(Key), x + arr(Value).replaceAll(s"\\d+", ""))
          }
          case _ =>  r.updated(arr(Key), Set(arr(Value).replaceAll(s"\\d+", "")))
        }
      } else r
    })

    println(s"GetConsumerId Record Size: ${GetConsumerId.size}")
    println(GetConsumerId.foreach(c => println(s" ${clean(c._1)} = ${c._2.mkString(",")}")))
  */

  /*
  val GetProductMediaId = Source.fromFile(OutputFile).getLines().foldLeft(Map[String, Set[String]]())((r, c) => {
    val Key = 37
    val Value = 34
    if (c.contains("Instant Info")) {
      val arr = clean(c).split(",")
      r.get(arr(Key)) match {
        case Some(x) => {
          r.updated(arr(Key), x + arr(Value).replaceAll(s"\\d+", ""))
        }
        case _ =>  r.updated(arr(Key), Set(arr(Value).replaceAll(s"\\d+", "")))
      }
    } else r
  })

    println(s"GetConsumerId Record Size: ${GetProductMediaId.size}")
    println(GetProductMediaId.foreach(c => println(s" ${clean(c._1)} = ${c._2.mkString(",")}")))

  */

/*

  val GetCampaignMediaId = Source.fromFile(OutputFile).getLines().foldLeft(Map[String, Set[String]]())((r, c) => {
    val Key = 6
    val Value = 11
    if (c.contains("Instant Info")) {
      val arr = c.split(",")
      val v = s"${clean(arr(37))}#${clean(arr(34))}#${clean(arr(Value)).replaceAll(s"\\d+", "")}"
      r.get(arr(Key)) match {
        case Some(x) => {
          r.updated(arr(Key), x + v)
        }
        case _ => r.updated(arr(Key), Set(v))
      }
    } else r
  })

  println(s"GetConsumerId Record Size: ${GetCampaignMediaId.size}")
  println(GetCampaignMediaId.foreach(c => println(s" ${clean(c._1)} = ${c._2.withFilter(_.contains("NO#VOICE")).map(x => x).mkString(",\n")}")))
*/



  val GetCampaignMediaId = Source.fromFile(OutputFile).getLines().foldLeft(Map[String, Map[String, Int]]())((r, c) => {
    val Key = 6
    val Value = 11
    val arr = c.split(",")
    if (c.contains("Instant Info") && c.contains("NO") && c.contains("VOICE")) {

      val v = s"${clean(arr(37))}#${clean(arr(34))}"
      r.get(arr(Key)) match {
        case Some(x) => {
          r.updated(arr(Key), x.updated(v, x.get(v).getOrElse(0) + 1))
        }
        case _ => r.updated(arr(Key), Map(v -> 1))
      }
    } else r
  })

  println(s"GetConsumerId Record Size: ${GetCampaignMediaId.size}")
  println(GetCampaignMediaId.foreach(c => println(s" ${clean(c._1)} = ${c._2.mkString(",")} \n")))


}
