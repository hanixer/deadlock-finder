package deadlockFinder

import org.eclipse.jdt.core.dom.{AST, ASTParser, CompilationUnit}

import java.nio.file.{Files, Path}

object JavaParser:
  def parseFile(file: String): CompilationUnit =
    parseFile(Path.of(file))
  
  def parseFile(file: Path): CompilationUnit =
    val source = Files.readString(file)
    parse(source)
  
  private def parse(source: String): CompilationUnit =
    val parser = ASTParser.newParser(AST.JLS16)
    parser.setSource(source.toCharArray)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setResolveBindings(true)
    parser.setEnvironment(Array.empty, Array.empty, Array.empty, true)
    parser.setUnitName("Unit name")
    parser.createAST(null).asInstanceOf[CompilationUnit]  

