name := "com/chmist/futureeither"

version := "1.0"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.0" % "test"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
    