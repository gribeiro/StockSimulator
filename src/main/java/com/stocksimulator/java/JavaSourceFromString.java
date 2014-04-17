package com.stocksimulator.java;

import java.net.URI;
import javax.tools.SimpleJavaFileObject;

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