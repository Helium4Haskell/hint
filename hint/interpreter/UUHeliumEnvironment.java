package hint.interpreter;


import java.io.*;

import hint.util.*;


/**
 * Specialized environment for use at the windows workstations at the UU only.
 * It does not require helium to be installed as it will do the installation
 * itself (without permanently affecting any configuration setting).
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 */

public class UUHeliumEnvironment extends ProcessEnvironment
{
    private File    installationDir;
    private File    localDir;

    public static final File   DEFAULT_INSTALL_DIR = new File("\\\\Staff_home\\projects\\helium");
    public static final File   DEFAULT_LOCAL_DIR   = new File("C:\\temp\\helium");

    public static final String UU_EDITOR_COMMANDLINE_TEMPLATE = "\"C:\\apps\\ConTEXT\\ConTEXT.exe\" %f /g%c:%r"; /* %f -> file, %r -> row, %c -> col */


    public UUHeliumEnvironment()
    {
        this(DEFAULT_INSTALL_DIR, DEFAULT_LOCAL_DIR);

        setEditorCommandlineTemplate(UU_EDITOR_COMMANDLINE_TEMPLATE);
    }


    public UUHeliumEnvironment(File installationDir, File localDir)
    {
        super();

        setInstallationDirectory(installationDir);
        setLocalDirectory(localDir);
    }


    public void setInstallationDirectory(File dir)
    {
        if (!dir.exists() || !dir.isDirectory())
            throw new IllegalArgumentException("Invalid installation directory: "+dir);

        this.installationDir = dir;
    }


    public void setLocalDirectory(File dir)
    {
        if (dir.exists() && !dir.isDirectory())
            throw new IllegalArgumentException("Invalid local directory: "+dir);

        this.localDir = dir;
    }


    public String[] getEnvironmentSettings()
    {
        String[] env = new String[6];
        env[0] = new String("PATH="     + getPathEnvironmentSetting());
        env[1] = new String("LVMPATH="  + getLVMEnvironmentSetting());
        env[2] = new String("HOME="     + System.getProperty("user.home"));
        env[3] = new String("USERNAME=" + System.getProperty("user.name"));
        env[4] = new String("OS="       + System.getProperty("os.name"));
        env[5] = new String("TEMP="     + System.getProperty("java.io.tmpdir"));
        return env;
    }


    public void createLocalInstallation() throws IOException
    {
        Dir.updateDir(installationDir, localDir);
    }


    public String getPathEnvironmentSetting()
    {
        if (localDir == null || !localDir.exists())
            throw new IllegalStateException("Local directory not available");

        File binDir = new File(localDir, "bin");
        return super.getPathEnvironmentSetting()
               + File.pathSeparator
               + binDir.getAbsolutePath();
    }


    public String getLVMEnvironmentSetting()
    {
        if (localDir == null || !localDir.exists())
            throw new IllegalStateException("Local directory not available");

        String lvmPath = System.getProperty("LVMPATH");
        if (lvmPath == null)
            lvmPath = ".";

        File libDir = new File(localDir, "lib");

        return lvmPath + File.pathSeparator + libDir.getAbsolutePath();
    }
}
