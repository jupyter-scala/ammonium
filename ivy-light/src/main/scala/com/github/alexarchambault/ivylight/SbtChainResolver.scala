package com.github.alexarchambault.ivylight

import scala.collection.JavaConversions._

import java.io.File
import java.text.ParseException

import org.apache.ivy.core.settings.IvySettings
import org.apache.ivy.core.{ IvyContext, LogOptions }
import org.apache.ivy.core.module.descriptor.{ Artifact => IArtifact, DefaultModuleDescriptor, ModuleDescriptor, DependencyDescriptor }
import org.apache.ivy.core.resolve.{ ResolvedModuleRevision, ResolveData }
import org.apache.ivy.plugins.repository.file.{ FileRepository => IFileRepository, FileResource }
import org.apache.ivy.plugins.repository.url.URLResource
import org.apache.ivy.plugins.resolver._
import org.apache.ivy.plugins.resolver.util.{ HasLatestStrategy, ResolvedResource }
import org.apache.ivy.util.{ Message, StringUtils => IvyStringUtils }

// Extracted from SBT, and refactored a little.
// Most comments come from SBT.

// I only have a very vague idea of how it works.

case class SbtChainResolver(
  name: String,
  resolvers: Seq[DependencyResolver],
  settings: IvySettings
) extends ChainResolver {

  // TODO - We need to special case the project resolver so it always "wins" when resolving with inter-project dependencies.

  // Initialize ourselves.
  setName(name)
  setReturnFirst(true)
  setCheckmodified(false)
  // Here we append all the resolvers we were passed *AND* look for
  // a project resolver, which we will special-case.
  resolvers.foreach(add)

  // Technically, this should be applied to module configurations.
  // That would require custom subclasses of all resolver types in ConvertResolver (a delegation approach does not work).
  // It would be better to get proper support into Ivy.
  // A workaround is to configure the ModuleConfiguration resolver to be a ChainResolver.
  //
  // This method is only used by the pom parsing code in Ivy to find artifacts it doesn't know about.
  // In particular, a) it looks up source and javadoc classifiers b) it looks up a main artifact for packaging="pom"
  // sbt now provides the update-classifiers or requires explicitly specifying classifiers explicitly
  // Providing a main artifact for packaging="pom" does not seem to be correct and the lookup can be expensive.
  //
  // Ideally this could just skip the lookup, but unfortunately several artifacts in practice do not follow the
  // correct behavior for packaging="pom" and so it is only skipped for source/javadoc classifiers.
  override def locate(artifact: IArtifact) = {
    val hasImplicitClassifier =
      artifact.getQualifiedExtraAttributes.keys.exists(_.asInstanceOf[String] startsWith "m:")

    if (hasImplicitClassifier) null else super.locate(artifact)
  }

  override def getDependency(dd: DependencyDescriptor, data: ResolveData) = {
    if (data.getOptions.getLog != LogOptions.LOG_QUIET)
      Message.info(s"Resolving ${dd.getDependencyRevisionId} ...")

    val resolved = doGetDependency(dd, data)

    if (resolved == null)
      null
    else {
      val desc = resolved.getDescriptor
      val updatedDescriptor = desc
      new ResolvedModuleRevision(resolved.getResolver, resolved.getResolver, updatedDescriptor, resolved.getReport, resolved.isForce)
    }
  }

  // Modified implementation of ChainResolver#getDependency.
  // When the dependency is changing, it will check all resolvers on the chain
  // regardless of what the "latest strategy" is set, and look for the published date
  // or the module descriptor to sort them.
  // This implementation also skips resolution if "return first" is set to true,
  // and if a previously resolved or cached revision has been found.
  private def doGetDependency(dd: DependencyDescriptor, data0: ResolveData) = {

    def isChanging = dd.getDependencyRevisionId.getRevision endsWith "-SNAPSHOT"

    def forcedRevision(rmr: ResolvedModuleRevision) =
      new ResolvedModuleRevision(rmr.getResolver, rmr.getArtifactResolver, rmr.getDescriptor, rmr.getReport, true)

    // useLatest - Means we should always download the JARs from the internet, no matter what.
    //             This will only be true *IF* the depenendency is dynamic/changing *and* latestSnapshots is true.
    // If you find multiple candidates,
    // - If `isReturnFirst` is true, you return the first value found
    // - If not, we will ATTEMPT to look at the publish date, which is not correctly discovered for Maven modules and
    //   leads to undefined behavior.
    val useLatest = dd.isChanging || isChanging
    if (useLatest)
      Message.verbose(s"$getName is changing. Checking all resolvers on the chain")

    val data = new ResolveData(data0, doValidate(data0))
    // Returns the value if we've already been resolved from some other branch of the resolution tree.
    val resolved = Option(data.getCurrentResolvedModuleRevision)
    // If we don't have any previously resolved date, we try to pull the value from the cache.
    val resolvedOrCached =
      resolved orElse {
        Message.verbose(s"$getName: Checking cache for: $dd")
        Option(findModuleInCache(dd, data, true)) map { mr =>
          Message.verbose(s"$getName: module revision found in cache: ${mr.getId}")
          forcedRevision(mr)
        }
      }

    // Default value for resolution.  We use this while we loop...
    //  If useLatest is true, we want to try to download from the internet so we DO NOT start with a valid value.
    var temp = if (useLatest) None else resolvedOrCached

    // Cast resolvers to something useful. TODO - we dropping anything here?
    val resolvers = getResolvers.toArray.toVector collect { case x: DependencyResolver => x }

    // Here we do an attempt to resolve the artifact from each of the resolvers in the chain.
    // -  If we have a return value already, AND isReturnFirst is true AND useLatest is false, we DO NOT resolve anything
    // -  If we do not, try to resolve.
    // RETURNS:   Left -> Error
    //            Right -> Some(resolved module)  // Found in this resolver, can use this result.
    //            Right -> None                   // Do not use this resolver
    val results = resolvers map { resolver =>
      // if the revision is cached and isReturnFirst is set, don't bother hitting any resolvers, just return None for this guy.
      if (isReturnFirst && temp.isDefined && !useLatest) Right(None)
      else {
        // We actually do resolution.
        val oldLatest =
          resolver match {
            case r: HasLatestStrategy if r.getLatest != "default" =>
              val oldLatest = r.getLatestStrategy
              r.setLatestStrategy(getLatestStrategy)
              Some(oldLatest)
            case _ =>
              None
          }

        try {
          val previouslyResolved = temp
          // if the module qualifies as changing, then resolve all resolvers
          data.setCurrentResolvedModuleRevision(if (useLatest) null else temp.orNull)

          temp = Option(resolver.getDependency(dd, data))
          Right(
            if (temp eq previouslyResolved) None
            else
              temp map { x =>
                (if (useLatest) reparseModuleDescriptor(dd, data, resolver, x) else forcedRevision(x), resolver)
              }
          )
        } catch { case ex: Exception =>
            Message.verbose(s"problem occurred while resolving $dd with $resolver: ${IvyStringUtils.getStackTrace(ex)}")
            Left(ex)
        } finally {
          resolver match {
            case r: HasLatestStrategy => oldLatest.foreach(r.setLatestStrategy)
            case _ =>
          }
          checkInterrupted()
        }
      }
    }

    def revisionDate(rmr: ResolvedModuleRevision, resolver: DependencyResolver) = {
      Message.warn(s"Sorting results from $rmr, using ${rmr.getPublicationDate} and ${rmr.getDescriptor.getPublicationDate}")
      // Just issue warning about issues with publication date, and fake one on it for now.
      Option(rmr.getPublicationDate) orElse Option(rmr.getDescriptor.getPublicationDate) match {
        case None =>
          (resolver.findIvyFileRef(dd, data), rmr.getDescriptor) match {
            case (null, _) =>
              // In this instance, the dependency is specified by a direct URL or some other sort of "non-ivy" file
              if (dd.isChanging)
                Message.warn(s"Resolving a changing dependency (${rmr.getId}) with no ivy/pom file!, resolution order is undefined!")
              0L
            case (ivf, dmd: DefaultModuleDescriptor) =>
              val lmd = new java.util.Date(ivf.getLastModified)
              Message.debug(s"Getting no publication date from resolver: $resolver for ${rmr.getId}, setting to: $lmd")
              dmd.setPublicationDate(lmd)
              ivf.getLastModified
            case _ =>
              Message.warn(s"Getting null publication date from resolver: $resolver for ${rmr.getId}, resolution order is undefined!")
              0L
          }
        case Some(date) => // All other cases ok
          date.getTime
      }
    }

    val errors = results collect { case Left(e) => e }
    val foundRevisions = results collect { case Right(Some(x)) => x }
    val sorted =
      if (useLatest)
        foundRevisions.sortBy((revisionDate _).tupled).lastOption .map { case (rmr, resolver) =>
          Message.warn(s"Choosing $resolver for ${rmr.getId}")

          // Now that we know the real latest revision, let's force Ivy to use it
          val artifactOpt = findFirstArtifactRef(rmr.getDescriptor, dd, data, resolver)
          artifactOpt match {
            case None =>
              if (resolver.getName != "inter-project")
                throw new RuntimeException(s"\t${resolver.getName}: no ivy file nor artifact found for $rmr")

            case Some(artifactRef) =>
              val systemMd = toSystem(rmr.getDescriptor)
              getRepositoryCacheManager.cacheModuleDescriptor(resolver, artifactRef,
                toSystem(dd), systemMd.getAllArtifacts.head, None.orNull, getCacheOptions(data))
          }

          rmr
        }
      else
        foundRevisions.lastOption.map(_._1) // we have to take last because resolvers are hit in reverse order.

    // If the value is already in cache, SORTED will be a Seq(None, None, ...) which means we'll fall over to the previously cached or resolved version.
    val mrOpt = sorted orElse resolvedOrCached

    mrOpt match {
      case None if errors.size == 1 =>
        errors.head match {
          case e: RuntimeException => throw e
          case e: ParseException   => throw e
          case e: Throwable        => throw new RuntimeException(e.toString, e)
        }
      case None if errors.size > 1 =>
        val err = errors.toList.map(IvyStringUtils.getErrorMessage).mkString("\n\t", "\n\t", "\n")
        throw new RuntimeException(s"several problems occurred while resolving $dd: $err")
      case _ =>
        if (resolved == mrOpt) resolved.orNull
        else mrOpt.map{ rmr =>
          if (isDual) new ResolvedModuleRevision(rmr.getResolver, this, rmr.getDescriptor, rmr.getReport, rmr.isForce)
          else rmr
        }.orNull
    }
  }

  // Ivy seem to not want to use the module descriptor found at the latest resolver
  private def reparseModuleDescriptor(dd: DependencyDescriptor, data: ResolveData, resolver: DependencyResolver, rmr: ResolvedModuleRevision) =
  // TODO - Redownloading/parsing the ivy file is not really the best way to make this correct.
  //        We should figure out a better alternative, or directly attack the resolvers Ivy uses to
  //        give them correct behavior around -SNAPSHOT.
    Option(resolver.findIvyFileRef(dd, data)) .flatMap { ivyFile =>
      ivyFile.getResource match {
        case r: FileResource =>
          try {
            val parser = rmr.getDescriptor.getParser
            val md = parser.parseDescriptor(settings, r.getFile.toURI.toURL, r, false)
            Some(new ResolvedModuleRevision(resolver, resolver, md, rmr.getReport, true))
          } catch { case _: ParseException => None }
        case _ => None
      }
    } .getOrElse {
      Message.warn(s"Unable to reparse ${dd.getDependencyRevisionId} from $resolver, using ${rmr.getPublicationDate}")
      rmr
    }

  private def findFirstArtifactRef(md: ModuleDescriptor, dd: DependencyDescriptor, data: ResolveData, resolver: DependencyResolver) =
    resolver match {
      case resolver: BasicResolver =>
        def artifactRef(artifact: IArtifact) = {
          IvyContext.getContext.set(resolver.getName + ".artifact", artifact)

          try {
            Option(resolver.doFindArtifactRef(artifact, data.getDate)) .orElse {
              Option(artifact.getUrl) .map { url =>
                Message.verbose(s"\tusing url for $artifact: $url")
                val resource =
                  if ("file" == url.getProtocol) new FileResource(new IFileRepository(), new File(url.getPath))
                  else new URLResource(url)
                new ResolvedResource(resource, artifact.getModuleRevisionId.getRevision)
              }
            }
          } finally IvyContext.getContext.set(resolver.getName + ".artifact", null)
        }

        val artifactRefs =
          for {
            conf <- md.getConfigurations.toIterator
            af <- md.getArtifacts(conf.getName).toIterator
            ref <- artifactRef(af)
          } yield ref

        if (artifactRefs.hasNext) Some(artifactRefs.next()) else None

      case _ =>
        None
    }

}