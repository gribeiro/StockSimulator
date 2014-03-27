package com.stocksimulator.java_loader

import java.io.IOException
import java.io.PrintWriter
import java.io.StringWriter
import java.lang.reflect.InvocationTargetException
import java.net.URI
import java.util.Arrays
import java.util.ArrayList
import javax.tools.Diagnostic
import javax.tools.DiagnosticCollector
import javax.tools.JavaCompiler
import javax.tools.JavaFileObject
import javax.tools.SimpleJavaFileObject
import javax.tools.ToolProvider
import javax.tools.JavaCompiler.CompilationTask
import javax.tools.JavaFileManager
import javax.tools.StandardLocation
import javax.tools.JavaFileObject.Kind
import javax.tools._
import com.stocksimulator.java.JavaSourceFromString
import scala.collection.JavaConverters._
import com.stocksimulator.debug.Log
import java.util.Locale
import java.net._
import java.io.File
import java.net.URLClassLoader
import scala.collection.mutable.ArrayBuffer

import com.stocksimulator.abs._
import com.stocksimulator.java_loader._
import javax.script._
object MemoryCompiler {
	var savedName: String = ""
	var savedSource: String = ""

	def loadAgain(currentStrat: JavaStdStrategy) = {
		val name = savedName
		val source = savedSource
		val strategy = currentStrat
		val compiler = ToolProvider.getSystemJavaCompiler()
		val allFiles = ArrayBuffer.empty[String]
		val cPath = (new File("""C:\Teste\lib""")).listFiles.map(f => f.toString) mkString ";"

		val diagnostics = new DiagnosticCollector[JavaFileObject]()
		if( compiler == null) throw new Exception( "Compiler unavailable")
		val classPath = System.getProperty("java.class.path")
		val files = classPath.split("[;]")
		val arquivos = new ArrayList[File]

		for(string <- files) {
			arquivos.add(new File(string))
		}
		val file = new JavaSourceFromString(name, source)
		//println(source)
		val compilationUnits = Arrays.asList(file)
		val optionList = new ArrayList[String]
		optionList.addAll(Arrays.asList("-classpath", cPath+""";C:\Teste\classes""" ))

		val fileManager = compiler.getStandardFileManager(diagnostics, null, null);
		//fileManager.setLocation(StandardLocation.CLASS_PATH, arquivos)
		val task = compiler.getTask(null, null, diagnostics, optionList, null, compilationUnits)
		val success = task.call()


		val diag = diagnostics.getDiagnostics().asScala

		//val compiledFile = new File("./"+name+".class")
		//val url = compiledFile.toURL()
		//val loader = new URLClassLoader(Array(url))
		
		for(d <- diag) {
			println(d.getCode)
			println(d.getKind)
			println("Line number:" + d.getLineNumber())
			println(d.getPosition)
			println(d.getStartPosition())
			println(d.getEndPosition())
			println(d.getSource())
			println(d.getMessage(null))
		}
		println("Compile ok: " + success)
		//val classLoader = Thread.currentThread().getContextClassLoader()
		val newClass = new File(System.getProperty("user.dir"))
		val url = newClass.toURI().toURL()
		println(url)
		val allUrls = (new File("""C:\Teste\lib""")).listFiles.map(f => new File(f.toString)).map(f => f.toURI().toURL())
		val krasses = (new File("""C:\Teste\classes""")).toURI().toURL()
		val urls = Array(krasses, url) ++ allUrls
		val newCL = new URLClassLoader(urls)
		println(newCL)
		val thisClass = newCL.loadClass(name)
		//val thisClass = Class.forName(name, true, classLoader)
		val a = thisClass.getDeclaredConstructor().newInstance()
	
		val manager = new ScriptEngineManager
    	val engine = manager.getEngineByName("jruby")
    	engine.put("newStrat", a)
    	val scriptToLoad = """ 
    	java_import 'com.stocksimulator.java_loader.JavaAdapter'
    	java_import 'com.stocksimulator.java_loader.JavaStdStrategy'
    	java_import 'com.stocksimulator.java_loader.MemoryCompiler'
    	java_import 'com.stocksimulator.abs.Strategy'
    	
    	class MicroAdapter < JavaAdapter
    		
    		
    		def setStrategy(strat)
    			$newStrat.setStrategy(strat)
    		end
    		
    		def onQuotes()
    			$newStrat.onQuotes()
    		end

    		def onStart()
    			$newStrat.onStart()
    		end
    	end
    	"""
    	engine.eval(scriptToLoad)
    	engine
		
}
	def apply(name: String, source: String) = {
		savedName = name
		savedSource = source
	}
}

