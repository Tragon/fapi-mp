package util

import java.io.{File, FileInputStream, ObjectInputStream, ObjectStreamClass}

class CustomObjectInputStream(file: File) extends ObjectInputStream(new FileInputStream(file)) {
  override protected def readClassDescriptor(): ObjectStreamClass = {
    val resultClassDescriptor = super.readClassDescriptor()
    val localClass = Class.forName(resultClassDescriptor.getName)
    val localClassDescriptor = ObjectStreamClass.lookup(localClass)
    if (localClassDescriptor != null) {
      localClassDescriptor
    } else {
      resultClassDescriptor
    }
  }
}
