import sbt._, Keys._

object Build extends Build {
  lazy val baseSettings = Seq(
    scalaVersion := "2.10.2",
    organization := "com.github.hexx",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:_")
  )

  lazy val indentParser = Project(
    id = "indent-parser",
    base = file(".")
  ).settings(
    baseSettings ++ seq(
      name := "indent-parser",
      version := "0.1.0",
      libraryDependencies ++= Seq(
      ),
      initialCommands in console += Seq(
      ).map("import " + _ + "\n").mkString
    ) : _*
  )

  lazy val yamlParser = Project(
    id = "yaml-parser",
    base = file("yaml")
  ).settings(
    baseSettings ++ seq(
      name := "yaml-parser",
      version := "0.1.0",
      libraryDependencies ++= Seq(
      ),
      initialCommands in console += Seq(
        "com.github.hexx.yaml._"
      ).map("import " + _ + "\n").mkString
    ) : _*
  ).dependsOn(indentParser)
}
