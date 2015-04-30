package test.ammonite.ops

import ammonite.ops._
import utest._
import utest.framework.TestSuite


object ShelloutTests extends TestSuite{
  val tests = TestSuite {
    'basic{
      val listed = (%ls).output.toSet
      val expected = Set(
        "build.sbt", "ops", "pprint", "tools",
        "project", "readme", "readme.md", "repl", "target"
      )
      assert(
        listed == expected,
        (%ls "ops/src/test/resources/testdata").output.toSet ==
        Set("folder1", "folder2", "File.txt")
      )
    }
    'failures{
      intercept[RuntimeException]{ %ls "does-not-exist" }
    }
  }
}
