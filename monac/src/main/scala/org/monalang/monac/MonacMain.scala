package org.monalang.monac

import org.monalang.monac.iface.CompileOptions
import org.monalang.monac.iface.OptionName

object MonacMain extends App {
  val configuration = CompileOptions(args.toList)
  println("sources: " + configuration.sources)
  println("objects: " + configuration.objects)
  println("output: " + configuration.get(OptionName.OUTPUT))
}