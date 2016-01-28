package ammonite

import org.apache.spark.sql.Row

import pprint.PPrinter

package object spark {

  implicit val pprintRow: PPrinter[Row] = PPrinter.Literal

}
