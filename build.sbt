import play.sbt.routes.RoutesKeys
import uk.gov.hmrc.DefaultBuildSettings

ThisBuild / majorVersion := 0
ThisBuild / scalaVersion := "3.3.3"

lazy val microservice = Project("service-commissioning-status", file("."))
  .enablePlugins(play.sbt.PlayScala, SbtDistributablesPlugin)
  .settings(
    PlayKeys.playDefaultPort :=  8858,
    libraryDependencies      ++= AppDependencies.compile ++ AppDependencies.test,
    // https://www.scala-lang.org/2021/01/12/configuring-and-suppressing-warnings.html
    // suppress warnings in generated routes files
    // Disabled until implemented in a later Scala version
//    scalacOptions            += "-Wconf:src=routes/.*:s",
    RoutesKeys.routesImport ++= Seq(
      "uk.gov.hmrc.servicecommissioningstatus.{LifecycleStatus, ServiceName, ServiceType, TeamName}",
      "uk.gov.hmrc.servicecommissioningstatus.Binders._",
    )
  )
  .settings(resolvers += Resolver.jcenterRepo)
  .settings(CodeCoverageSettings.settings: _*)

lazy val it =
  (project in file("it"))
    .enablePlugins(PlayScala)
    .dependsOn(microservice % "test->test")
    .settings(DefaultBuildSettings.itSettings())
