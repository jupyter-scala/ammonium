package ammonite.interpreter

case class ImportData(fromName: String,
                      toName: String,
                      wrapperName: String,
                      prefix: String,
                      isImplicit: Boolean)
