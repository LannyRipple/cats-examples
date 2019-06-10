val scalaVer = "2.11.11-bin-typelevel-4"

lazy val root =
    (project in file(".")).
      settings(
          name := "cats-examples",
          organization := "net.sfdc.ljr",
          version := "0.1.0-SNAPSHOT",
          scalaOrganization := "org.typelevel",
          scalaVersion := scalaVer,
          scalacOptions ++= Seq(
                "-deprecation"
              , "-feature"
              , "-unchecked"
          //    , "-Ypartial-unification"
          ),
          libraryDependencies ++= Seq(
              "org.typelevel" %% "kittens" % "1.2.1"
          )
      )
