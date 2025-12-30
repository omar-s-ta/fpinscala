name := "fpinscala"

ThisBuild / scalaVersion := "3.7.2"

ThisBuild / githubWorkflowBuild := Seq(WorkflowStep.Sbt(name = Some("Build project"), commands = List("test:compile")))

ThisBuild / scalacOptions ++= List("-feature", "-deprecation", "-Xkind-projector:underscores")

ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "1.2.1" % Test
