package org.monalang.monac.tests

import org.monalang.monac.iface.CompileOptions
import org.monalang.monac.iface.OptionName
import org.scalatest.FlatSpec

// TODO test more scenarios
class CompileOptionsSpec extends FlatSpec {
  "CompileConfiguration" should "detect source files and object files" in {
    val configuration = CompileOptions(List("bla.mona", "some.mo"))
    assert(configuration.sources == Set("bla.mona"))
    assert(configuration.objects == Set("some.mo"))
  }

  it should "detect the provided output option" in {
    val configuration = CompileOptions(List("bla.mona", "--output", "name", "some.mo"))
    assert(configuration.get(OptionName.OUTPUT) == Some(List("name")))
  }

  it should "report the missing architecture option as None" in {
    val configuration = CompileOptions(List("bla.mona", "-o", "name", "some.mo"))
    assert(configuration.get(OptionName.ARCHITECTURE) == None)
  }

  it should "report the missing output option as None" in {
    val configuration = CompileOptions(List("bla.mona", "some.mo"))
    assert(configuration.get(OptionName.OUTPUT) == None)
  }

  it should "throw an IllegalStateException when provided duplicate input files" in {
    intercept[IllegalStateException] {
      val configuration = CompileOptions(List("bla.mona", "bla.mona", "-o", "name", "some.mo"))
    }
    intercept[IllegalStateException] {
      val configuration = CompileOptions(List("bla.mona", "bla.mona", "-o", "name", "some.mo", "some.mo"))
    }
  }

  it should "throw an IllegalStateException when the output option is used multiple times" in {
    intercept[IllegalStateException] {
      val configuration = CompileOptions(List("bla.mona", "-o", "name", "--output", "name2"))
      configuration.get(OptionName.OUTPUT)
    }
  }
}