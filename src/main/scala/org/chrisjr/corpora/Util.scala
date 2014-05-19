package org.chrisjr.corpora

import java.io._

object Util {
  def pickle[T <% Serializable](outFile: File, obj: T) = {
    try {
      val out = new ObjectOutputStream(new FileOutputStream(outFile))
      out.writeObject(obj)
      out.close()
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

  def unpickle[T <% Serializable](inFile: File): T = {
    val in = new ObjectInputStream(new FileInputStream(inFile))
    val obj = in.readObject()
    in.close()
    obj.asInstanceOf[T]
  }
}