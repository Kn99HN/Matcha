name := "minidafny"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "com.googlecode.kiama" % "kiama_2.11" % "1.8.0"

//libraryDependencies += "com.regblanc" %% "scala-smtlib" % "0.2"

resolvers +=  "dzufferey maven repo" at "https://github.com/dzufferey/my_mvn_repo/raw/master/repository"

libraryDependencies += "io.github.dzufferey" %% "scala-smtlib-interface" % "0.1-SNAPSHOT"

parallelExecution in Test := false
