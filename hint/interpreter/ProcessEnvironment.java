package hint.interpreter;


import java.io.*;
import java.util.*;


/**
 * Abstraction of the environment of a process. This implementation uses the
 * environment that is provided by the operating system and specifies a
 * few settings that are per default present on a MS-Windows platform.
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 */

public class ProcessEnvironment
{
    private static ProcessEnvironment currentEnvironment = null;

    private String  editorCommandlineTemplate;
    private String  browserCommandlineTemplate;
    private String  additionalHeliumParameters;
    private int     fontSize;
    private boolean overloading;

    public static final String  DEFAULT_EDITOR_COMMANDLINE_TEMPLATE  = "\"C:\\apps\\ConTEXT\\ConTEXT.exe\" %f /g%c:%r";
    public static final String  DEFAULT_BROWSER_COMMANDLINE_TEMPLATE = "\"C:\\Program Files\\Internet Explorer\\iexplore.exe\" %u";
    public static final String  DEFAULT_ADDITIONAL_HELIUM_PARAMETERS = "";
    public static final int     DEFAULT_FONTSIZE                     = 12;
    public static final boolean DEFAULT_OVERLOADING                  = true;

    public static final String CONFIG_FILENAME = ".hint.conf";


    public static synchronized ProcessEnvironment getEnvironment()
    {
        if (currentEnvironment == null)
            setEnvironment(new ProcessEnvironment());

        return currentEnvironment;
    }


    public static synchronized void setEnvironment(ProcessEnvironment context)
    {
        currentEnvironment = context;
    }


    public ProcessEnvironment()
    {
        setEditorCommandlineTemplate(DEFAULT_EDITOR_COMMANDLINE_TEMPLATE);
        setBrowserCommandlineTemplate(DEFAULT_BROWSER_COMMANDLINE_TEMPLATE);
        setAdditionalHeliumParameters(DEFAULT_ADDITIONAL_HELIUM_PARAMETERS);
        setFontSize(DEFAULT_FONTSIZE);
        setOverloading(DEFAULT_OVERLOADING);
    }


    public void saveSettings() throws IOException
    {
        String userHomeDirectory = System.getProperty("user.home");
        File configFile = new File(userHomeDirectory, CONFIG_FILENAME);

        Properties props = new Properties();
        props.setProperty("editorCommandlineTemplate",  getEditorCommandlineTemplate());
        props.setProperty("browserCommandlineTemplate", getBrowserCommandlineTemplate());
        props.setProperty("additionalHeliumParameters", getAdditionalHeliumParameters());
        props.setProperty("fontSize",                   Integer.toString(getFontSize()));
        props.setProperty("overloading",                Boolean.toString(getOverloading()));

        FileOutputStream outputStream = new FileOutputStream(configFile);
        props.store(outputStream, "Hint");
        outputStream.flush();
        outputStream.close();
    }


    public void loadSettings() throws IOException
    {
        String userHomeDirectory = System.getProperty("user.home");
        File configFile = new File(userHomeDirectory, CONFIG_FILENAME);

        if (!configFile.exists())
            return;

        Properties props = new Properties();
        InputStream inputStream = new FileInputStream(configFile);
        props.load(inputStream);
        inputStream.close();

        if (props.containsKey("editorCommandlineTemplate"))
            setEditorCommandlineTemplate(props.getProperty("editorCommandlineTemplate"));

        if (props.containsKey("browserCommandlineTemplate"))
            setBrowserCommandlineTemplate(props.getProperty("browserCommandlineTemplate"));

        if (props.containsKey("additionalHeliumParameters"))
            setAdditionalHeliumParameters(props.getProperty("additionalHeliumParameters"));
        
        if (props.containsKey("overloading"))
            setOverloading(Boolean.valueOf(props.getProperty("overloading")).booleanValue());

		try {
			if (props.containsKey("fontSize"))
				setFontSize(Integer.parseInt(props.getProperty("fontSize")));
		} catch (NumberFormatException e) {}

    }


    public String[] getEnvironmentSettings()
    {
        return null;
    }


    public String getPathEnvironmentSetting()
    {
        String envPath = System.getProperty("PATH");
        if (envPath == null)
            envPath = System.getProperty("java.library.path");

		// System.out.println("PATH = " + envPath);
        return envPath;
    }


    public String getLVMEnvironmentSetting()
    {
        String path = System.getProperty("LVMPATH");
        String simple = "simple";

        if (path.endsWith(simple))
            path = path.substring(0, path.length()-simple.length()-1);

        // strip off trailing path seperator
        if (path.length() >= 2 && path.endsWith(":"))
            path = path.substring(0, path.length()-2);

        if (!overloading)
            path = path + File.separator + simple;

        return path;
    }


    public File getTempPath()
    {
        String tempdir;

        tempdir = System.getProperty("java.io.tmpdir");
        if (tempdir == null)
            tempdir = System.getProperty("TEMP");
        if (tempdir == null)
            tempdir = System.getProperty("TMP");
        if (tempdir == null)
            throw new IllegalStateException("No such TEMP environment variable");

        File path = new File(tempdir);
        if (!path.exists() || !path.isDirectory())
            throw new IllegalStateException("Invalid temporary directory");

        if (!path.canWrite())
            throw new IllegalStateException("Temporary directory not writable");

        return path;
    }


    public String getEditorCommandlineTemplate()
    {
        return editorCommandlineTemplate;
    }


    public void setEditorCommandlineTemplate(String commandline)
    {
        if (commandline == null)
            throw new IllegalArgumentException("commandline is null");

        editorCommandlineTemplate = commandline;
    }


    public String getBrowserCommandlineTemplate()
    {
        return browserCommandlineTemplate;
    }


    public void setBrowserCommandlineTemplate(String commandline)
    {
        if (commandline == null)
            throw new IllegalArgumentException("commandline is null");

        browserCommandlineTemplate = commandline;
    }


    public String getAdditionalHeliumParameters()
    {
        return additionalHeliumParameters;
    }


    public void setAdditionalHeliumParameters(String parameters)
    {
        if (parameters == null)
            throw new IllegalArgumentException("parameters is null");

        additionalHeliumParameters = parameters;
    }

    public int getFontSize()
    {
        return fontSize;
    }

    public void setFontSize(int newSize)
	{
		if (newSize > 40) newSize = 40;
		if (newSize < 10) newSize = 10;
		fontSize = newSize;
	}
	
	
	public boolean getOverloading()
	{
	    return overloading;
    }
    
    
    public void setOverloading(boolean b)
    {
        overloading = b;
    }
}

