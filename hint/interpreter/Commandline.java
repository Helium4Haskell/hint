package hint.interpreter;


import java.util.*;
import java.io.*;


/**
 * Abstraction of the commandline that can be used as input for the
 * Runtime.exec() method. It'll seach the paths provided by the environment
 * to find the actual location of the executable. This prevents 'strange'
 * behavior when the user uses (for example) batch files with the same name
 * in the active directory.
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.1 (12 feb 2003)
 * @since   HI 1.0
 */

public class Commandline
{
    private LinkedList parameters;
    private String     absolutePath;
    private File       workingDirectory;

    private static final String EXTENSION = ".exe";


    public Commandline(String commandName)
    {
        this(commandName, null);
    }


    public Commandline(String commandName, File workingDirectory)
    {
        this.absolutePath     = getAbsolutePath(commandName);
        this.parameters       = new LinkedList();
        this.workingDirectory = workingDirectory;
    }


    public void addParameters(String parameters)
    {
        StringTokenizer tokenizer = new StringTokenizer(parameters);
        while(tokenizer.hasMoreTokens())
            addParameter(tokenizer.nextToken());
    }


    public void addParameter(String param)
    {
        parameters.addLast(param);
    }


    public String[] toCommandArray()
    {
        LinkedList command = new LinkedList(parameters);
        command.addFirst(absolutePath);

        String[] commandArray = new String[1 + parameters.size()];
        return (String[]) command.toArray(commandArray);
    }


    public String getAbsolutePath(String commandName)
    {
        String envPath = ProcessEnvironment.getEnvironment().getPathEnvironmentSetting();
        if (envPath == null)
            return commandName;

        StringTokenizer tokenizer = new StringTokenizer(envPath, File.pathSeparator);
        while(tokenizer.hasMoreTokens())
        {
            File dir = new File(tokenizer.nextToken());

            File commandWithExtension = new File(dir, commandName + EXTENSION);
            if (commandWithExtension.exists())
                return commandWithExtension.getPath();
			// !!! gevaarlijk omdat ie ook paden vindt zo (c:\apps\helium als c:\apps in je pad zit)
            File commandWithoutExtension = new File(dir, commandName);
            if (commandWithoutExtension.exists())
                return commandWithoutExtension.getPath();
        }

        return commandName;
    }


    public File getWorkingDirectory()
    {
        return workingDirectory;
    }
}
