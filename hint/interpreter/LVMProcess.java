package hint.interpreter;


import java.io.*;

import hint.util.*;


/**
 * An execution of the LVM, using the specified module file as starting point.
 *
 * @author Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0
 * @since HI 1.0
 */

public class LVMProcess extends AbstractIOProcess
{
    public static final String LVM_COMMAND_NAME = "lvmrun";


    public LVMProcess(File modulePath, File workingDirectory, InputObserver observer) throws IOException
    {
        super();

        setObserver(observer);

        Commandline lvmCommandline;
        lvmCommandline = new Commandline(LVM_COMMAND_NAME, workingDirectory);
        
        String lvmPath = ProcessEnvironment.getEnvironment().getLVMEnvironmentSetting();
        lvmCommandline.addParameter("-P" + lvmPath);
        lvmCommandline.addParameter(modulePath.getAbsolutePath());
        
        // System.out.println("Executing: "+lvmCommandline);
        try {
          execute(lvmCommandline);
        }
        catch (IOException e) {
          System.out.println("Could not run ``lvmrun''. Did you maybe forget to install it?");
          System.out.println("Do so by running ``cabal install lvmrun'' from the commandline");
          System.out.println("The Java run time returned the following exception:\n "+ e);         
        }
    }
}
