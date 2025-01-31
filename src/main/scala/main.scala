import Utils.{Configuration, ConfigurationParser,Init}

object main extends App{
  val directory = getClass.getResource("/bd/config0s1.txt").getPath
  var parameters :ConfigurationParser= new ConfigurationParser
  parameters.parseConfigurationFile(directory)
  val conf:Configuration= new Configuration(parameters)

  val iniciar = new Init(conf)
  iniciar.run()

}
