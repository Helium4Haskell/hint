package hint.util;


import java.awt.*;
import java.io.*;


public class ExtentionFilenameFilter extends javax.swing.filechooser.FileFilter implements FilenameFilter, FileFilter
{
    private String extention;
    private String description;


    public ExtentionFilenameFilter(String extention, String description)
    {
        if (extention == null)
            throw new IllegalArgumentException("extention is null");
        if (description == null)
            throw new IllegalArgumentException("description is null");

        this.extention   = extention;
        this.description = description;
    }


    public boolean accept(File file)
    {
        if (file == null)
            return false;
        if (file.isDirectory())
            return true;

        return accept(file.getParentFile(), file.getName());
    }


    public boolean accept(File dir, String name)
    {
        if (dir == null || name == null)
            return false;

        return name.toLowerCase().endsWith(extention);
    }


    public String getDescription()
    {
        return description + " ("+extention+")";
    }
}
