package com.github.alexarchambault.ivylight

import java.io.{File, IOException}
import java.net.{URISyntaxException, URL}
import java.util.Collections

import org.apache.ivy.core.module.descriptor.{Artifact => IArtifact, _}
import org.apache.ivy.core.resolve.ResolveData
import org.apache.ivy.core.settings.IvySettings
import org.apache.ivy.plugins.repository.{TransferEvent, RepositoryCopyProgressListener}
import org.apache.ivy.plugins.repository.file.{FileRepository, FileResource}
import org.apache.ivy.plugins.repository.url.URLRepository
import org.apache.ivy.plugins.resolver._
import org.apache.ivy.util._
import org.xml.sax.SAXParseException

import scala.xml.XML

object ResolverHelpers {

  trait ChecksumFriendlyURLResolver extends RepositoryResolver {
    val signerName0 = {
      val f =
        try {
          val f = getClass.getDeclaredField("signerName")
          f.setAccessible(true); Some(f)
        }
        catch { case (_: java.lang.NoSuchFieldException) | (_: java.lang.SecurityException) | (_: java.lang.NoSuchMethodException) => None }

      () =>
        f.fold(null: String)(_.get(this).asInstanceOf[String])
    }

    // visibility changing override
    override def putChecksum(artifact: IArtifact, src: File, dest: String, overwrite: Boolean, algorithm: String) =
      super.putChecksum(artifact, src, dest, overwrite, algorithm)

    // visibility changing override
    override def putSignature(artifact: IArtifact, src: File, dest: String, overwrite: Boolean) =
      super.putSignature(artifact, src, dest, overwrite)

    override protected def put(artifact: IArtifact, src: File, dest: String, overwrite: Boolean): Unit = {
      // verify the checksum algorithms before uploading artifacts!
      val checksums = getChecksumAlgorithms
      val repository = getRepository

      if (checksums.exists(!ChecksumHelper.isKnownAlgorithm(_)))
        throw new IllegalArgumentException(s"Unknown checksum algorithm(s): ${checksums.filter(!ChecksumHelper.isKnownAlgorithm(_))}")

      repository.put(artifact, src, dest, overwrite)

      // Fix for sbt#1156 - Artifactory will auto-generate MD5/sha1 files, so
      // we need to overwrite what it has.
      for (checksum <- checksums)
        putChecksum(artifact, src, dest, true, checksum)

      if (signerName0() != null)
        putSignature(artifact, src, dest, true: java.lang.Boolean)
    }
  }

  trait DescriptorRequired extends BasicResolver {
    override def getDependency(dd: DependencyDescriptor, data: ResolveData) = {
      val prev = descriptorString(isAllownomd)
      setDescriptor(descriptorString(hasExplicitURL(dd)))
      try super.getDependency(dd, data) finally setDescriptor(prev)
    }

    def descriptorString(optional: Boolean) =
      if (optional) BasicResolver.DESCRIPTOR_OPTIONAL else BasicResolver.DESCRIPTOR_REQUIRED

    def hasExplicitURL(dd: DependencyDescriptor): Boolean =
      dd.getAllDependencyArtifacts.exists(_.getUrl != null)
  }

  class WarnOnOverwriteFileRepo extends FileRepository() {
    override def put(source: File, destination: String, overwrite: Boolean): Unit =
      try super.put(source, destination, overwrite)
      catch {
        case e: java.io.IOException if e.getMessage.contains("destination already exists") =>
          Message.warn(s"Attempting to overwrite $destination\n\tThis usage is deprecated and will be removed in sbt 1.0.")
          super.put(source, destination, true)
      }
  }

  class LocalIfFileRepo extends URLRepository {
    private val repo = new WarnOnOverwriteFileRepo()
    private val progress = new RepositoryCopyProgressListener(this)

    private def toFile(url: URL): File =
      try { new File(url.toURI) }
      catch { case _: URISyntaxException => new File(url.getPath) }

    override def getResource(source: String) = {
      val url = new URL(source)
      if (url.getProtocol == "file")
        new FileResource(repo, toFile(url))
      else
        super.getResource(source)
    }

    override def put(source: File, destination: String, overwrite: Boolean): Unit = {
      val url = new URL(destination)
      if (url.getProtocol == "file") {
        // Here we duplicate the put method for files so we don't just bail on trying ot use Http handler
        val resource = getResource(destination)

        if (!overwrite && resource.exists())
          throw new IOException("destination file exists and overwrite == false")

        fireTransferInitiated(resource, TransferEvent.REQUEST_PUT)

        try {
          val totalLength = source.length
          if (totalLength > 0)
            progress.setTotalLength(totalLength)

          FileUtil.copy(source, new java.io.File(url.toURI), progress)
        } catch {
          case ex: IOException => fireTransferError(ex); throw ex
          case ex: RuntimeException => fireTransferError(ex); throw ex
        } finally progress.setTotalLength(null)
      } else
        super.put(source, destination, overwrite)
    }
  }

  class PluginCapableResolver(pattern: String) extends IBiblioResolver with ChecksumFriendlyURLResolver with DescriptorRequired {
    def setPatterns() = {
      val patterns = Collections.singletonList(pattern)

      // done this way for access to protected methods.
      setArtifactPatterns(patterns)
      setIvyPatterns(patterns)
    }
  }


  lazy val userHome = new File(System.getProperty("user.home"))
  lazy val ivyHome = new File(userHome, ".ivy2")

}

object Resolver {
  import ResolverHelpers._

  case class Patterns(ivyPatterns: Seq[String],
                      artifactPatterns: Seq[String],
                      isMavenCompatible: Boolean,
                      descriptorOptional: Boolean = false,
                      skipConsistencyCheck: Boolean = false)


  def mavenResolver(name: String, root: String): DependencyResolver = {
    val pattern = "[organisation]/[module](_[scalaVersion])(_[sbtVersion])/[revision]/[artifact]-[revision](-[classifier]).[ext]"
    val resolvePattern = {
      val normBase = root.replace('\\', '/')
      if (normBase.endsWith("/") || pattern.startsWith("/")) normBase + pattern else normBase + "/" + pattern
    }

    val resolver = new PluginCapableResolver(resolvePattern)
    resolver.setRepository(new LocalIfFileRepo)
    resolver.setName(name)
    resolver.setM2compatible(true)
    resolver.setRoot(root)
    resolver.setPatterns() // has to be done after initializeMavenStyle, which calls methods that overwrite the patterns
    resolver
  }

  def fileOrURLRepository(name: String, patterns: Patterns, fileIsLocalTransactional: Option[(Boolean, Option[Boolean])], settings: IvySettings = new IvySettings()): DependencyResolver = {
    val resolver =
      fileIsLocalTransactional match {
        case Some((isLocal, isTransactional)) =>
          val res = new FileSystemResolver with DescriptorRequired {
            // Workaround for #1156
            // Temporarily in sbt 0.13.x we deprecate overwriting
            // in local files for non-changing revisions.
            // This will be fully enforced in sbt 1.0.
            setRepository(new WarnOnOverwriteFileRepo())
          }
          res.setLocal(isLocal)
          isTransactional.foreach(value => res.setTransactional(value.toString))
          res
        case None =>
          new URLResolver with ChecksumFriendlyURLResolver with DescriptorRequired
      }

    resolver.setName(name)
    resolver.setM2compatible(patterns.isMavenCompatible)
    resolver.setDescriptor(if (patterns.descriptorOptional) BasicResolver.DESCRIPTOR_OPTIONAL else BasicResolver.DESCRIPTOR_REQUIRED)
    resolver.setCheckconsistency(!patterns.skipConsistencyCheck)
    patterns.ivyPatterns.foreach(p => resolver.addIvyPattern(settings substitute p))
    patterns.artifactPatterns.foreach(p => resolver.addArtifactPattern(settings substitute p))
    resolver
  }


  lazy val mavenLocal = {
    def mavenLocalDir = {
      def loadHomeFromSettings(f: => File) =
        try {
          Some(f).filter(_.exists())
            .map(XML.loadFile(_).\("localRepository").text)
            .filter(_.nonEmpty)
            .map(new File(_))
        } catch {
          // Occurs inside File constructor when property or environment variable does not exist
          case _: NullPointerException => None
          // Occurs when File does not exist
          case _: IOException          => None
          case e: SAXParseException    => System.err.println(s"WARNING: Problem parsing ${f.getAbsolutePath}, ${e.getMessage}"); None
        }

      loadHomeFromSettings(new File(userHome, ".m2/settings.xml"))
        .orElse(loadHomeFromSettings(new File(new File(System.getenv("M2_HOME")), "conf/settings.xml")))
        .getOrElse(new File(userHome, ".m2/repository"))
    }

    mavenResolver("Maven2 Local", mavenLocalDir.toURI.toString)
  }

  lazy val defaultMaven = mavenResolver("public", "https://repo1.maven.org/maven2/")
  def sonatypeRepo(status: String) = mavenResolver(s"sonatype-$status", s"https://oss.sonatype.org/content/repositories/$status")

  lazy val localRepo = {
    val id = "local"
    val localBasePattern = "[organisation]/[module]/(scala_[scalaVersion]/)(sbt_[sbtVersion]/)[revision]/[type]s/[artifact](-[classifier]).[ext]"
    val pList = List(s"${ivyHome.getAbsolutePath}/$id/$localBasePattern")
    fileOrURLRepository(id, Patterns(pList, pList, isMavenCompatible = false), fileIsLocalTransactional = Some((true, None)))
  }
}