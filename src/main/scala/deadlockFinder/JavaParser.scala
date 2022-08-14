package deadlockFinder

import org.eclipse.jdt.core.dom.{AST, ASTParser, CompilationUnit}

import java.nio.file.{Files, Path, Paths}

object JavaParser:
  def parseFile(file: String): CompilationUnit =
    parseFile(Path.of(file))
  
  def parseFile(file: Path): CompilationUnit =
    val source = Files.readString(file)
    parse(source)
  
  def parse(source: String): CompilationUnit =
    val parser = ASTParser.newParser(AST.JLS16)
    val mpjHomeStr = System.getenv("MPJ_HOME")
    if mpjHomeStr == null || mpjHomeStr.isEmpty then
      throw new Exception("MPJ_HOME environment variable is not set")

    val mpjHome = Path.of(mpjHomeStr)
    if !Files.exists(mpjHome) then
      throw new Exception("Please set MPJ_HOME environment variable to existing location of MPJ Express library.")

    val mpjJar = mpjHome.resolve("lib").resolve("mpj.jar")
    parser.setSource(source.toCharArray)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setResolveBindings(true)
    parser.setEnvironment(Array(mpjJar.toString), Array.empty, Array.empty, true)
    parser.setUnitName("Unit name")    
    parser.createAST(null).asInstanceOf[CompilationUnit]  

