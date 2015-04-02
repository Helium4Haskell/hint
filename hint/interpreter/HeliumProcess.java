package hint.interpreter;


import java.io.*;
import java.util.*;

import hint.inputfilter.*;
import hint.util.*;

import hint.interpreter.ProcessEnvironment;


/**
 * An execution of the helium compiler. The parameters are provided by the
 * HeliumParameters container. Before the compiler is executed, a temporary
 * file will be created that is used to define the expression the user
 * specified and to load the module.
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.1 (27 feb 2003)
 * @since   Hint 1.0
 */

public class HeliumProcess extends AbstractIOProcess
{
    public static final int    COMPILATION_SUCCESSFULL_EXITCODE = 0;

    public static final String MAIN_FUNCTION         = "interpreter_main";
    public static final String HELIUM_INPUT_MODULE   = "Interpreter";
    public static final String HELIUM_COMMAND_NAME   = "helium";
    public static final String HELIUM_FILE_EXTENSION = ".hs";
    public static final String LVM_FILE_EXTENSION    = ".lvm";
    public static final String HELIUM_INPUT_FILE     = HELIUM_INPUT_MODULE + HELIUM_FILE_EXTENSION;
    public static final String MODULE_HEADER         = "module "+HELIUM_INPUT_MODULE+" where";

    private File inputModule;


    public HeliumProcess(InputObserver observer, HeliumParameters parameters) throws IOException
    {
        super();

        inputModule = createInputModule(parameters.getExpression(), parameters.getModule());

        InputObserver finalDelegate;
        if (parameters.evaluateExpressionType())
            finalDelegate = new KeepMaintypeFilter(observer, parameters.getExpression());
        else
            finalDelegate = observer;

        boolean removeMinorWarnings = parameters.evaluateExpressionType();
        setObserver( new RedirectInputFilter
                       ( new LineBufferInputFilter
                           ( new EvidenceInputFilter
                               ( finalDelegate
                               , removeMinorWarnings
                               )
                           )
                       , RedirectInputFilter.TARGET_ERROR
                       , RedirectInputFilter.TARGET_ERROR
                       )
                   );

        String heliumCommand = ProcessEnvironment.getEnvironment().getHeliumBinDir()+File.separator+HELIUM_COMMAND_NAME;
        Commandline heliumCommandline;
        if (parameters.getModule() != null)
            heliumCommandline = new Commandline(heliumCommand, parameters.getModule().getParentFile());
        else
            heliumCommandline = new Commandline(heliumCommand);
        
        String lvmPath = ProcessEnvironment.getEnvironment().getLVMEnvironmentSetting();
        heliumCommandline.addParameter("-P" + lvmPath);
        
        if (ProcessEnvironment.getEnvironment().getOverloading())
            heliumCommandline.addParameter("--overloading");
        else
            heliumCommandline.addParameter("--no-overloading");


        if (parameters.evaluateExpressionType())
            heliumCommandline.addParameter("--dump-information");

        if (parameters.isAlert()) {
            heliumCommandline.addParameter("--alert=" + parameters.getAlertMessage());
            heliumCommandline.addParameter("-b");
            heliumCommandline.addParameter("--enable-logging");            
        }
        else // Only necessary if alert is not on:
            if (ProcessEnvironment.getEnvironment().getLoggingOn()) 
                heliumCommandline.addParameter("--enable-logging");
            else
                heliumCommandline.addParameter("--disable-logging");

        if (parameters.isAlert() || ProcessEnvironment.getEnvironment().getLoggingOn()) { // If logging enabled send along settings
            heliumCommandline.addParameter("--host=" + ProcessEnvironment.getEnvironment().getHost());
            heliumCommandline.addParameter("--port=" + ProcessEnvironment.getEnvironment().getPort());            
        }
            
        String additional = ProcessEnvironment.getEnvironment().getAdditionalHeliumParameters();
        if (!additional.trim().equals("")) { // Essential check 
            heliumCommandline.addParameters(additional);
        }
        heliumCommandline.addParameter(inputModule.getAbsolutePath());

        // System.out.println("Executing: "+heliumCommandline);

        execute(heliumCommandline);
    }


    public void sendData(String data)
    {
        throw new IllegalStateException("The compiler will not use input: "+data);
    }


    public File getCompiledLVMFile()
    {
        if (getExitCode() == COMPILATION_SUCCESSFULL_EXITCODE)
            return getLVMModuleFile(inputModule);

        return null;
    }


    public static File getLVMModuleFile(File inputModule)
    {
        File directory = inputModule.getParentFile();
        return new File(directory, Dir.stripFileExtension(inputModule.getName(), HELIUM_FILE_EXTENSION) + LVM_FILE_EXTENSION);
    }


    protected static File createInputModule(String expression, File importModule) throws IOException
    {
        File moduleFile = new File(ProcessEnvironment.getEnvironment().getTempPathDescr(), HELIUM_INPUT_FILE);
        moduleFile.deleteOnExit();

        File futureLVMFile = getLVMModuleFile(moduleFile);
        futureLVMFile.deleteOnExit();
        if (futureLVMFile.exists())
            futureLVMFile.delete();

        PrintWriter module = new PrintWriter(new FileOutputStream(moduleFile));
        module.println(MODULE_HEADER);

        if (importModule != null)
            module.println("import "+Dir.stripFileExtension(importModule.getName(), HELIUM_FILE_EXTENSION));

        module.println(MAIN_FUNCTION + " = " + addIndent(expression));
        module.flush();
        module.close();

        return moduleFile;
    }


    protected static String addIndent(String expression)
    {
        StringTokenizer tokenizer = new StringTokenizer(expression, "\n", true);

        if (!tokenizer.hasMoreTokens())
            return expression;

        String result = tokenizer.nextToken();
        if (tokenizer.hasMoreTokens())
            result += tokenizer.nextToken();

        while(tokenizer.hasMoreTokens())
        {
            result += " ";

            result += tokenizer.nextToken();
            if (tokenizer.hasMoreTokens())
                result += tokenizer.nextToken();
        }

        return result;
    }
}
