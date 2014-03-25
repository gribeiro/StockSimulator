package com.stocksimulator.java_loader

import java.io.IOException
import java.io.PrintWriter
import java.io.StringWriter
import java.lang.reflect.InvocationTargetException
import java.net.URI
import java.util.Arrays
import javax.tools.Diagnostic
import javax.tools.DiagnosticCollector
import javax.tools.JavaCompiler
import javax.tools.JavaFileObject
import javax.tools.SimpleJavaFileObject
import javax.tools.ToolProvider
import javax.tools.JavaCompiler.CompilationTask
import javax.tools.JavaFileManager
import javax.tools.JavaFileObject.Kind
import com.stocksimulator.java.JavaSourceFromString
import scala.collection.JavaConverters._
import com.stocksimulator.debug.Log
import java.util.Locale
import java.io.File
import java.net.URLClassLoader

object MemoryCompiler {

  val compiler = ToolProvider.getSystemJavaCompiler()
  val diagnostics = new DiagnosticCollector[JavaFileObject]()
  if( compiler == null) throw new Exception( "Compiler unavailable");
  def apply(name: String, source: String) = {
	val file = new JavaSourceFromString(name, source)
println(source)
	val compilationUnits = Arrays.asList(file)
var classPath = System.getProperty("java.class.path");
	val options = Arrays.asList("-d", ".", "-classpath", classPath)
println(classPath)
	val fileManager = compiler.getStandardFileManager(diagnostics, Locale.ENGLISH, null);
	val task = compiler.getTask(null, null, diagnostics, null, null, compilationUnits)
	val success = task.call()


	val diag = diagnostics.getDiagnostics().asScala
	println(task)
	//val compiledFile = new File("./"+name+".class")
	//val url = compiledFile.toURL()
	//val loader = new URLClassLoader(Array(url))
	
	for(d <- diag) {
	  println(d.getCode)
	  println(d.getKind)
	  println(d.getPosition)
	  println(d.getStartPosition())
	  println(d.getEndPosition())
	  println(d.getSource())
	  println(d.getMessage(null))
	}
	println("Compile ok: " + success)
	val classLoader = Thread.currentThread().getContextClassLoader()
	val thisClass = Class.forName(name, true, classLoader)
	if(success)
	(Some(thisClass), success)
	else (None, success)
  }
}

