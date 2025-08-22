import sbt.*

object Dependecies {
  /*
   * Versions
   */
  lazy val scalaTestVersion = "3.2.19"
  lazy val cucumberVersion = "8.31.1"
  /*
   * Libraries
   */
  val cucumberCore = "io.cucumber" % "cucumber-core" % "7.27.2" % Test
  val cucumberJunit =
    "io.cucumber" % "cucumber-junit" % cucumberVersion % Test
  val cucumberJvm = "io.cucumber" % "cucumber-jvm" % cucumberVersion % Test
  val cucumberScala = "io.cucumber" %% "cucumber-scala" % cucumberVersion % Test
  val scalactic = "org.scalactic" %% "scalactic" % scalaTestVersion
  val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.18.1" % Test
  val scalaTestJUnit5 =
    "org.scalatestplus" %% "junit-5-10" % "3.2.19.1" % Test
  val mockitoCore = "org.mockito" % "mockito-core" % "5.19.0" % Test
  val scalaTestMockito =
    "org.scalatestplus" %% "mockito-5-10" % "3.2.18.0" % Test
  /*
   * Bundles
   */
  val scalaTestBundle: Seq[ModuleID] = Seq(scalaTest, scalactic)
  val cucumberBundle: Seq[ModuleID] = Seq(cucumberCore, cucumberScala)
  val mockitoBundle: Seq[ModuleID] = Seq(mockitoCore, scalaTestMockito)
}
