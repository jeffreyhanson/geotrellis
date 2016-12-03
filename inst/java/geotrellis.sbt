name := "geotrellis" 

version := "1.0.0" 

scalaVersion := "2.11.8"

resolvers += Resolver.bintrayRepo("azavea", "geotrellis")

libraryDependencies ++= Seq(
  "com.azavea.geotrellis" %% "geotrellis-spark"  % "0.10.3",
  "com.azavea.geotrellis" %% "geotrellis-raster" % "0.10.3",
  "com.azavea.geotrellis" %% "geotrellis-vector" % "0.10.3",
  "com.azavea.geotrellis" %% "geotrellis-util"   % "0.10.3",
  "org.apache.spark"      %% "spark-core"        % "2.0.1" % "provided",
  "org.scalatest"         %% "scalatest"         % "3.0.0" % "test"
)

assemblyJarName := "geotrellis.jar"

assemblyMergeStrategy in assembly := {
 case "reference.conf" => MergeStrategy.concat
 case "application.conf" => MergeStrategy.concat
 case "META-INF/MANIFEST.MF" => MergeStrategy.discard
 case "META-INF\\MANIFEST.MF" => MergeStrategy.discard
 case "META-INF/ECLIPSE.RSA" => MergeStrategy.discard
 case "META-INF/ECLIPSE.SD" => MergeStrategy.discard
 case _ => MergeStrategy.first
}
 
