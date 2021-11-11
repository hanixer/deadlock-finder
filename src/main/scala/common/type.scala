package deadlockFinder
package common

sealed trait Type

case object IntType extends Type:
  override def toString: String = "int"

case object DoubleType extends Type:
  override def toString: String = "double"

case object FloatType extends Type:
  override def toString: String = "float"

case object BooleanType extends Type:
  override def toString: String = "boolean"

case object VoidType extends Type:
  override def toString: String = "void"

case class ClassType() extends Type:
  override def toString: String = "someClass"