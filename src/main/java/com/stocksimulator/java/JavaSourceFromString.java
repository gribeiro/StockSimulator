package com.stocksimulator.java;

import com.stocksimulator.abs.*;
import com.stocksimulator.java_loader.*;


import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.util.Arrays;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;
import javax.tools.JavaCompiler.CompilationTask;
import javax.tools.JavaFileObject.Kind;

public class JavaSourceFromString extends SimpleJavaFileObject {
    /**
     * The source code of this "file".
     */
    final String code;

    /**
     * Constructs a new JavaSourceFromString.
     * @param name the name of the compilation unit represented by this file object
     * @param code the source code for the compilation unit represented by this file object
     */
    public JavaSourceFromString(String name, String code) {
        super(URI.create("string:///" + name.replace('.','/') + Kind.SOURCE.extension),
              Kind.SOURCE);
       // System.out.println(URI.create("string:///" + name.replace('.','/') + Kind.SOURCE.extension));
        this.code = code;
    }

    @Override
    public CharSequence getCharContent(boolean ignoreEncodingErrors) {
        return code;
    }
}