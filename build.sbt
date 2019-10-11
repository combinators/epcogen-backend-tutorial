version := "1.0.0-SNAPSHOT"
organization := "org.combinators"

scalaVersion := "2.12.9"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.typesafeRepo("releases")
)

scalacOptions in (Compile) ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:implicitConversions",
  "-Ypartial-unification",
  "-language:higherKinds"
)

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.5" % "test",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.typelevel" %% "cats-core" % "2.0.0-M4",
  "org.typelevel" %% "cats-free" % "2.0.0-M4"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

