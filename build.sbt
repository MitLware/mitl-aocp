lazy val root = project.
  settings(
    name := "AOCP",
    version := "0.1.0",
    scalaVersion := "2.12.3"
    , mainClass in (Compile, run) := Some("aocp.AOCPExample")
  )

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "0.9.0",
  "org.typelevel" %% "cats-kernel" % "0.9.0",
  "org.typelevel" %% "cats-macros" % "0.9.0",
  "com.github.julien-truffaut" %% "monocle" % "1.4.0",
  "org.scalaz" %% "scalaz-core" % "7.2.13",
  "org.apache.commons" % "commons-lang3" % "3.5",
  "org.apache.commons" % "commons-math3" % "3.6.1"
)

// End ///////////////////////////////////////////////////////////////


