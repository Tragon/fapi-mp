package util

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import scala.util.Using

trait Serializer[M] {

  protected var data: Seq[M]

  protected def saveFile: String

  def save(): Unit = {
    println("Saving...")
    Using.resource(new ObjectOutputStream(new FileOutputStream(saveFile))) (_.writeObject(data))
    println("Saved successfully")
  }

  def load(): Unit = {
    val f = new java.io.File(saveFile)
    if(f.exists) {
      println("Loading...")
      Using.resource(new ObjectInputStream(new FileInputStream(f))) { reader =>
        data = reader.readObject.asInstanceOf[Seq[M]]
      }
      println("Loaded successfully")
    } else {
      println("No load file found")
    }
  }
}
