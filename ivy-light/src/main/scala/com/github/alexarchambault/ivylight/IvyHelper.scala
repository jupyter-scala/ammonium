package com.github.alexarchambault.ivylight

import java.io.File

import org.apache.ivy.Ivy
import org.apache.ivy.core.module.descriptor.Configuration.Visibility
import org.apache.ivy.core.module.descriptor._
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.core.settings.IvySettings
import org.apache.ivy.plugins.parser.xml.XmlModuleDescriptorWriter
import org.apache.ivy.plugins.resolver._
import org.apache.ivy.util._

// From Ammonite's IvyThing and sbt-ivy

object IvyHelper {

  var maxLevel = 2
  Message.setDefaultLogger {
    new AbstractMessageLogger {
      def doEndProgress(msg: String) = Console.err println "Done"
      def doProgress() = Console.err print "."
      def log(msg: String, level: Int) = if (level <= maxLevel) Console.err.println(msg)
      def rawlog(msg: String, level: Int) = log(msg, level)
    }
  }

  def resolve(artifacts: Seq[(String, String, String)], resolvers: Seq[DependencyResolver], verbosity: Int = 2) = {
    maxLevel = verbosity

    val settings = new IvySettings()
    val chain = new SbtChainResolver("sbt-chain", resolvers, settings)

    settings.setBaseDir(ResolverHelpers.ivyHome)
    settings.addResolver(chain)
    settings.setDefaultResolver(chain.getName)

    val ivy = new Ivy()
    ivy.setSettings(settings)
    ivy.bind()

    val md = new DefaultModuleDescriptor(
      ModuleRevisionId.newInstance("ammonite", "shell", "working"),
      "release", null, false
    )

    /*
     * Here, we're trying to setup Ivy like SBT does
     */

    md.setLastModified(System.currentTimeMillis)
    md.setDescription("")

    md.addConfiguration(new Configuration("compile", Visibility.PUBLIC, "", Array(), true, null))
    md.addConfiguration(new Configuration("runtime", Visibility.PUBLIC, "", Array("compile"), true, null))
    md.addConfiguration(new Configuration("test", Visibility.PUBLIC, "", Array("runtime"), true, null))
    md.addConfiguration(new Configuration("provided", Visibility.PUBLIC, "", Array(), true, null))
    md.addConfiguration(new Configuration("optional", Visibility.PUBLIC, "", Array(), true, null))

    md.setDefaultConfMapping("*->default(compile)")

    for ((groupId, artifactId, version) <- artifacts) {
      val dep = new DefaultDependencyDescriptor(
        md, ModuleRevisionId.newInstance(groupId, artifactId, version),
        false, false, true
      )

      dep.addDependencyConfiguration("compile", "default(compile)")
      md.addDependency(dep)
    }

    //creates an ivy configuration file
    val ivyfile = File.createTempFile("ivy", ".xml")
    ivyfile.deleteOnExit()
    XmlModuleDescriptorWriter.write(md, ivyfile)

    val resolveOptions = new ResolveOptions()
      .setResolveId(ResolveOptions.getDefaultResolveId(md))
      .setRefresh(true)
      .setOutputReport(false)

    //init resolve report
    val report = ivy.resolve(ivyfile.toURI.toURL, resolveOptions)

    // TODO Use these:
    // report.getAllProblemMessages
    // report.getUnresolvedDependencies

    //so you can get the jar libraries
    report.getAllArtifactsReports.map(_.getLocalFile)
  }
}
