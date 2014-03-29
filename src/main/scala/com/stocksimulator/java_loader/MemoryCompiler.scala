package com.stocksimulator.java_loader

import java.io.IOException
import java.io.PrintWriter
import java.io.StringWriter
import java.lang.reflect.InvocationTargetException
import java.net.URI
import java.util.Arrays
import java.util.ArrayList
import java.util.List
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
import com.stocksimulator.java._

import javax.script._
object MemoryCompiler {
	var savedName: String = ""
	var savedSource: String = ""

	def loadAgain:Object = {
		val name = savedName
		val source = savedSource
		JavaComp.load(name, source)
	
		
}
	def apply(name: String, source: String):Object = {
		savedName = name
		savedSource = source
		loadAgain
	}
}

