package hint.interpreter;


import java.io.*;


/**
 * Container for parameters to the helium compiler.
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0 (5 feb 2003)
 * @since   HI 1.0
 */

public class HeliumParameters
{
    private String  expression;
    private File    module;
    private boolean evaluateExpressionType;
    private boolean compileOnly;


    public HeliumParameters()
    {
        expression  = "()";
        module      = null;
        evaluateExpressionType = false;
        compileOnly            = false;
    }


    public void setExpression(String expression)
    {
        if (expression == null)
            throw new IllegalArgumentException("null expression not allowed");

        this.expression = expression;
    }


    public void setModule(File file)
    {
        if (file == null)
            throw new IllegalArgumentException("null module not allowed");
        if (!file.exists())
            throw new IllegalArgumentException("module doesn\'t exists: "+file.getPath());

        module = file;
    }


    public void setEvaluateExpressionType()
    {
        evaluateExpressionType = true;
    }


    public void setCompileOnly()
    {
        compileOnly = true;
    }


    public String getExpression()
    {
        return expression;
    }


    public File getModule()
    {
        return module;
    }


    public boolean evaluateExpressionType()
    {
        return evaluateExpressionType;
    }


    public boolean compileOnly()
    {
        return compileOnly;
    }
}
