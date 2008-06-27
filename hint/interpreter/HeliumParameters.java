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
    private String  alertMessage;
    private File    module;
    private boolean evaluateExpressionType;
    private boolean compileOnly;
    private boolean alert;

    public HeliumParameters()
    {
        expression   = "()";
        alertMessage = "";
        module       = null;
        evaluateExpressionType = false;
        compileOnly            = false;
        alert                  = false;
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
            throw new IllegalArgumentException("module doesn\'t exist: "+file.getPath());

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

    public void setAlert(String message)
    {
        alert = true;
        alertMessage = message;
    }

    public boolean isAlert()
    {
        return alert;
    }

    public String getAlertMessage() {
        return alertMessage;
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
