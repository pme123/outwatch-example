
enablePlugins(ScalaJSPlugin)

name := "Outwatchexample"

version := "0.1.0"

organization := "pme"

scalaVersion := "2.12.1"

jsEnv := PhantomJSEnv().value


libraryDependencies ++= Seq(
  "io.github.outwatch" %%% "outwatch" % "0.9.2"
  //"com.github.lukajcb" %%% "rxscala-js" % "0.14.0" // as long a local build
, "net.scalapro" %%% "sortable-js-facade" % "0.2.1"
  , "org.scalatest" %%% "scalatest" % "3.0.1" % Test
)

jsDependencies ++= Seq(
  "org.webjars" % "jquery" % "1.11.1" / "jquery.js" minified "jquery.min.js"
  , "org.webjars" % "bootstrap" % "3.3.6" / "bootstrap.js" minified "bootstrap.min.js" dependsOn "jquery.js"
  // needs sortable 1.6.0 - not available with webjars
  ,  ProvidedJS / "scripts/Sortable.min.js"
  //, "org.webjars.bower" % "github-com-RubaXa-Sortable" % "1.4.2"  % Provided / "1.4.2/Sortable.js" minified "Sortable.min.js"

)
