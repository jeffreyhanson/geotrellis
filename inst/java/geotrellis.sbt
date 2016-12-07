name := "geotrellis" 

version := "1.0.0" 

scalaVersion := "2.11.8"

resolvers += "LocationTech GeoTrellis Releases" at "https://repo.locationtech.org/content/repositories/geotrellis-releases"

libraryDependencies ++= Seq(
  "org.locationtech.geotrellis" %% "geotrellis-spark"  % "1.0.0-RC3",
  "org.locationtech.geotrellis" %% "geotrellis-raster" % "1.0.0-RC3",
  "org.locationtech.geotrellis" %% "geotrellis-vector" % "1.0.0-RC3",
  "org.locationtech.geotrellis" %% "geotrellis-util"   % "1.0.0-RC3",
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
 
