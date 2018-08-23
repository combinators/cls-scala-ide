import sbt.Keys.{libraryDependencies, _}
import sbt.Resolver
import play.sbt.PlayLayoutPlugin
import de.heikoseeberger.sbtheader.FileType
import play.twirl.sbt.SbtTwirl



lazy val commonSettings = Seq(
  organization := "org.combinators",

  scalaVersion := "2.12.4",
  crossScalaVersions := Seq("2.11.12", scalaVersion.value),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.typesafeRepo("snapshots")
  ),

  headerLicense := Some(HeaderLicense.ALv2("2018", "Anna Vasileva")),

  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions"
  )
) ++ publishSettings

lazy val examples = (Project(id = "cls-scala-ide-examples", base = file("examples")))
  .settings(commonSettings: _*)
  .settings(noPublishSettings: _*)
  .enablePlugins(SbtTwirl)
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin)
  .settings(
    moduleName := "cls-scala-ide-example"
  ).dependsOn(root)

lazy val root = (Project(id = "cls-scala-ide", base = file(".")))
  .settings(commonSettings: _*)
  .enablePlugins(SbtTwirl)
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin)
  .settings(
    moduleName := "cls-scala-ide",

    libraryDependencies ++= Seq(
      "org.combinators" %% "cls-scala-presentation-play-git" % "1.0.0-RC1+1-00659e19",
      "org.combinators" %% "cls-scala" % "2.1.0+7-9e42ea3e",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
      "org.scalactic" %% "scalactic" % "3.0.5" % "test",
      "org.scalatest" %% "scalatest" % "3.0.5" % "test",
      "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % "test",
      "org.webjars" %% "webjars-play" % "2.6.1",
      "org.webjars" % "bootstrap" % "3.3.7-1",
      "org.webjars.bower" % "cytoscape" % "3.2.5",
      "com.typesafe.play" %% "play-json" % "2.6.2",
      guice
    ),

    sourceDirectories in (Compile, TwirlKeys.compileTemplates) := Seq(sourceDirectory.value / "main" / "html-templates"),
    sources in (Test, play.sbt.routes.RoutesKeys.routes) ++= ((unmanagedResourceDirectories in Test).value * "routes").get,
    PlayKeys.playMonitoredFiles ++= (sourceDirectories in (Compile, TwirlKeys.compileTemplates)).value,

    headerMappings := headerMappings.value ++ Seq(
      FileType("html") -> HeaderCommentStyle.twirlStyleBlockComment,
      FileType("css") -> HeaderCommentStyle.cStyleBlockComment,
      FileType("js") -> HeaderCommentStyle.cStyleBlockComment,
      FileType("routes") -> HeaderCommentStyle.hashLineComment
    ),
    unmanagedSources.in(Compile, headerCreate) ++= sources.in(Compile, TwirlKeys.compileTemplates).value,
    unmanagedSources.in(Compile, headerCreate) ++= sources.in(Compile, resourceDirectories).value
  )

lazy val publishSettings = Seq(
  homepage := Some(url("https://combinators.org")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scmInfo := Some(ScmInfo(url("https://www.github.com/combinators/cls-scala-ide"), "scm:git:git@github.com:combinators/cls-scala-ide.git")),
  developers := List(
    Developer("", "Anna Vasileva", "anna.vasileva@tu-dortmund.de", url("https://ls14-www.cs.tu-dortmund.de/cms/de/mitarbeiter/wimis/Vasileva.html")),
    Developer("JanBessai", "Jan Bessai", "jan.bessai@tu-dortmund.de", url("http://janbessai.github.io"))
  ),

  pgpPublicRing := file("travis/local.pubring.asc"),
  pgpSecretRing := file("travis/local.secring.asc")
)

lazy val noPublishSettings = Seq(
  publish := Seq.empty,
  publishLocal := Seq.empty,
  publishArtifact := false
)
