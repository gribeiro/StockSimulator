package com.stocksimulator.java;

import java.util.ArrayList;
import java.util.List;

import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.ToolProvider;
import javax.tools.JavaFileObject;
public class JavaComp {
	
	public static Class<?> klass(String name, String src) throws Exception {
        String fullName = name;

        

        // We get an instance of JavaCompiler. Then
        // we create a file manager
        // (our custom implementation of it)
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        JavaFileManager fileManager = new
            ClassFileManager(compiler
                .getStandardFileManager(null, null, null));

        // Dynamic compiling requires specifying
        // a list of "files" to compile. In our case
        // this is a list containing one "file" which is in our case
        // our own implementation (see details below)
        List<JavaFileObject> jfiles = new ArrayList<JavaFileObject>();
        jfiles.add(new JavaSourceFromString(fullName, src));
        // We specify a task to the compiler. Compiler should use our file
        // manager and our list of "files".
        // Then we run the compilation with call()
        compiler.getTask(null, fileManager, null, null,
            null, jfiles).call();
  
        Class<?> klass = fileManager.getClassLoader(null).loadClass(fullName);
        return klass;
	}
    public static Object load(String name, String src) throws Exception {
        // Full name of the class that will be compiled.
        // If class should be in some package,
        // fullName should contain it too
        // (ex. "testpackage.DynaClass")
        String fullName = name;

 

        // We get an instance of JavaCompiler. Then
        // we create a file manager
        // (our custom implementation of it)
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        JavaFileManager fileManager = new
            ClassFileManager(compiler
                .getStandardFileManager(null, null, null));

        // Dynamic compiling requires specifying
        // a list of "files" to compile. In our case
        // this is a list containing one "file" which is in our case
        // our own implementation (see details below)
        List<JavaFileObject> jfiles = new ArrayList<JavaFileObject>();
        jfiles.add(new JavaSourceFromString(fullName, src));
        // We specify a task to the compiler. Compiler should use our file
        // manager and our list of "files".
        // Then we run the compilation with call()
        compiler.getTask(null, fileManager, null, null,
            null, jfiles).call();
  
        Class<?> klass = fileManager.getClassLoader(null).loadClass(fullName);
        System.out.println("Fora do for");
   
        for (Class<?> cls : klass.getDeclaredClasses()) {
        	System.out.println("Classe:" + cls.getName());
        	fileManager.getClassLoader(null).loadClass(cls.getName());
        }
        System.out.println("Dps do for");
        // Creating an instance of our compiled class and
        // running its toString() method
        Object instance = klass.newInstance();
        return instance;
    }
}
