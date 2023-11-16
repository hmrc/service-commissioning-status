import play.sbt.routes.RoutesKeys
import uk.gov.hmrc.DefaultBuildSettings.integrationTestSettings
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings

lazy val microservice = Project("service-commissioning-status", file("."))
  .enablePlugins(play.sbt.PlayScala, SbtDistributablesPlugin)
  .settings(
    majorVersion             :=  0,
    scalaVersion             :=  "2.13.10",
    PlayKeys.playDefaultPort :=  8858,
    libraryDependencies      ++= AppDependencies.compile ++ AppDependencies.test,
    // https://www.scala-lang.org/2021/01/12/configuring-and-suppressing-warnings.html
    // suppress warnings in generated routes files
    scalacOptions            += "-Wconf:src=routes/.*:s",
    RoutesKeys.routesImport ++= Seq(
      "uk.gov.hmrc.servicecommissioningstatus.{ServiceName, ServiceType, TeamName}",
      "uk.gov.hmrc.servicecommissioningstatus.Binders._",
    )
  )
  .configs(IntegrationTest)
  .settings(integrationTestSettings(): _*)
  .settings(resolvers += Resolver.jcenterRepo)
  .settings(CodeCoverageSettings.settings: _*)
