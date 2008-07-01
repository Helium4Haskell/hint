package hint.interpreter;


import java.io.*;
import java.util.*;


/**
 * Abstraction of the environment of a process. This implementation uses the
 * environment that is provided by the operating system and specifies a
 * few settings that are per default present on a MS-Windows platform.
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl> and Jurriaan Hage <jur@cs.uu.nl>
 * 
 * Some example settings for the editor and browser:
 * 
 *
 * For the editor:
 *
 *    C:\\apps\\ConTEXT\\ConTEXT.exe %f /g%c:%r                   -- Windows, should be tested
 *   On MacOSX, tested on Leopard, some on Tiger:
 *      open -a jEdit %f
 *   or java -jar /Applications/jEdit\ 4.2/jedit.jar -reuseview -- %f +line:%r
 *
 *      open -a BBEdit %f
 *   or bbedit +%r %f if you want it to jump to lines and have the command line versions of BBEdit installed. 
 *
 *      open -a TextEdit %f
 *      open -a XCode %f
 *      open -a Aquamacs\ Emacs %f
 *   or /Applications/Aquamacs\ Emacs.app/Contents/MacOS/Aquamacs\ Emacs +%r:%c %f
 *
 * With open you can not add line indicators to jump to. Too bad.
 * Then you need a separate Linux/Unix style command, like bbedit or the direct access to jEdit and Aquamacs\ Emacs, 
 * and obviously, those must be able to understand line jump indicators (but they usually do).
 *
 * For the browser:
 *    C:\\Program Files\\Internet\ Explorer\\iexplore.exe %u       -- Windows
 *    On MacOSX, all tested on Leopard, some also tested on Tiger: 
 *      open -a Mozilla %u         
 *      open -a Safari %u
 *      open -a Firefox %u
 *      open -a Netscape %u
 */

public class ProcessEnvironment
{
    private static ProcessEnvironment currentEnvironment = null;

    private String  basePath;
    private String  lvmPaths;
    private String  editorCommandlineTemplate;
    private String  browserCommandlineTemplate;
    private String  additionalHeliumParameters;
    private String  tempPath;
    private int     fontSize;
    private boolean overloadingOn;
    private boolean loggingOn;
    private String  host;
    private int     port;
    
    public static final String  DEFAULT_BASEPATH                   = "/usr/local/helium";
    public static final String  DEFAULT_LVMPATHS                   = "";
    public static final String  DEFAULT_TEMPPATH                   = ".";
    public static final String  DEFAULT_EDITORCOMMANDLINETEMPLATE  = "\"C:\\apps\\ConTEXT\\ConTEXT.exe\" \"%f\" /g%c:%r";
    public static final String  DEFAULT_BROWSERCOMMANDLINETEMPLATE = "\"C:\\Program Files\\Internet Explorer\\iexplore.exe\" %u";
    public static final String  DEFAULT_ADDITIONALHELIUMPARAMETERS = "";
    public static final int     DEFAULT_FONTSIZE                   = 12;
    public static final boolean DEFAULT_OVERLOADINGON              = true;
    public static final boolean DEFAULT_LOGGINGON                  = false;
    public static final String  DEFAULT_HOST                       = "helium.zoo.cs.uu.nl";
    public static final int     DEFAULT_PORT                       = 5010;
    
    public static final String  BASEPATH_KEY                   = "basepath";
    public static final String  LVMPATHS_KEY                   = "lvmpaths";
    public static final String  TEMPPATH_KEY                   = "temppath";
    public static final String  EDITORCOMMANDLINETEMPLATE_KEY  = "editorcommandlinetemplate";
    public static final String  BROWSERCOMMANDLINETEMPLATE_KEY = "browsercommandlinetemplate";
    public static final String  ADDITIONALHELIUMPARAMETERS_KEY = "additionalheliumparameters";
    public static final String  FONTSIZE_KEY                   = "fontsize";
    public static final String  OVERLOADINGON_KEY              = "overloadingon";
    public static final String  LOGGINGON_KEY                  = "loggingon";
    public static final String  HOST_KEY                       = "host";
    public static final String  PORT_KEY                       = "port";
    
    
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
        setBasePath(DEFAULT_BASEPATH);
        setLvmPaths(DEFAULT_LVMPATHS);
        setTempPath(DEFAULT_TEMPPATH);
        setEditorCommandlineTemplate(DEFAULT_EDITORCOMMANDLINETEMPLATE);
        setBrowserCommandlineTemplate(DEFAULT_BROWSERCOMMANDLINETEMPLATE);
        setAdditionalHeliumParameters(DEFAULT_ADDITIONALHELIUMPARAMETERS);
        setFontSize(DEFAULT_FONTSIZE);
        setOverloading(DEFAULT_OVERLOADINGON);
        setLoggingOn(DEFAULT_LOGGINGON);
        setHost(DEFAULT_HOST);
        setPort(DEFAULT_PORT);
    }


    public void saveSettings() throws IOException
    {
        String userHomeDirectory = System.getProperty("user.home");
        File configFile = new File(userHomeDirectory, CONFIG_FILENAME);

        Properties props = new Properties();
        props.setProperty(BASEPATH_KEY, getBasePath());
        props.setProperty(LVMPATHS_KEY, getLvmPaths());        
        props.setProperty(TEMPPATH_KEY, getTempPath());        
        props.setProperty(EDITORCOMMANDLINETEMPLATE_KEY,  getEditorCommandlineTemplate());
        props.setProperty(BROWSERCOMMANDLINETEMPLATE_KEY, getBrowserCommandlineTemplate());
        props.setProperty(ADDITIONALHELIUMPARAMETERS_KEY, getAdditionalHeliumParameters());
        props.setProperty(FONTSIZE_KEY, Integer.toString(getFontSize()));
        props.setProperty(OVERLOADINGON_KEY, Boolean.toString(getOverloading()));
        props.setProperty(LOGGINGON_KEY, Boolean.toString(getLoggingOn()));
        props.setProperty(HOST_KEY, getHost());        
        props.setProperty(PORT_KEY, Integer.toString(getPort()));

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

        if (props.containsKey(BASEPATH_KEY))
            setBasePath(props.getProperty(BASEPATH_KEY));

        if (props.containsKey(LVMPATHS_KEY))
            setLvmPaths(props.getProperty(LVMPATHS_KEY));

        if (props.containsKey(TEMPPATH_KEY))
            setTempPath(props.getProperty(TEMPPATH_KEY));

        if (props.containsKey(EDITORCOMMANDLINETEMPLATE_KEY))
            setEditorCommandlineTemplate(props.getProperty(EDITORCOMMANDLINETEMPLATE_KEY));

        if (props.containsKey(BROWSERCOMMANDLINETEMPLATE_KEY))
            setBrowserCommandlineTemplate(props.getProperty(BROWSERCOMMANDLINETEMPLATE_KEY));

        if (props.containsKey(ADDITIONALHELIUMPARAMETERS_KEY))
            setAdditionalHeliumParameters(props.getProperty(ADDITIONALHELIUMPARAMETERS_KEY));
        
        if (props.containsKey(OVERLOADINGON_KEY))
            setOverloading(Boolean.valueOf(props.getProperty(OVERLOADINGON_KEY)).booleanValue());
        
        if (props.containsKey(LOGGINGON_KEY))
            setLoggingOn(Boolean.valueOf(props.getProperty(LOGGINGON_KEY)).booleanValue());

        if (props.containsKey(HOST_KEY))
            setHost(props.getProperty(HOST_KEY));

		try {
			if (props.containsKey(FONTSIZE_KEY))
				setFontSize(Integer.parseInt(props.getProperty(FONTSIZE_KEY)));
		} catch (NumberFormatException e) {}
		try {
			if (props.containsKey(PORT_KEY))
				setPort(Integer.parseInt(props.getProperty(PORT_KEY)));
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


    // Returns one string with all LVM containing directories: the base path, . and the
    // manually added ones (by the user).
    public String getLVMEnvironmentSetting()
    {
        // System.out.println(getBasePath());
        String path = getBasePath();
        String simple = "simple";

        // strip off trailing path seperator
        if (path.length() >= 2 && path.endsWith(":"))
            path = path.substring(0, path.length()-2);

        path = path + File.separator + "lib";
        
        if (!overloadingOn)
            path = path + File.separator + simple;

        return path+File.pathSeparator+"."+File.pathSeparator+getLvmPaths();
    }


    public File getTempPathDescr()
    {
        File path = new File(getTempPath());
        if (!path.exists() || !path.isDirectory())
            throw new IllegalStateException("Invalid temporary directory");

        if (!path.canWrite())
            throw new IllegalStateException("Temporary directory not writable");

        return path;
    }

    public String getTempPath() {
        return tempPath;
    }
    
    public void setTempPath(String temppath)
    {
        if (temppath == null)
            throw new IllegalArgumentException("temppath is null");

        tempPath = temppath;
    }

    public String getBasePath()
    {
        return basePath;
    }


    public void setBasePath(String basepath)
    {
        if (basepath == null)
            throw new IllegalArgumentException("basepath is null");

        basePath = basepath;
    }

    public String getLvmPaths()
    {
        return lvmPaths;
    }


    public void setLvmPaths(String lvmpaths)
    {
        if (lvmpaths == null)
            throw new IllegalArgumentException("lvmpaths is null");

        lvmPaths = lvmpaths;
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

    public String getHost()
    {
        return host;
    }


    public void setHost(String parameters)
    {
        if (parameters == null)
            throw new IllegalArgumentException("parameters is null");

        host = parameters;
    }

    public int getPort()
    {
        return port;
    }

    public void setPort(int newPort)
  	{
        port = newPort;
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
        return overloadingOn;
    }
      
      
    public void setOverloading(boolean b)
    {
          overloadingOn = b;
    }
    
    public boolean getLoggingOn()
    {
        return loggingOn;
    }
    
    
    public void setLoggingOn(boolean b)
    {
        loggingOn = b;
    }

}

