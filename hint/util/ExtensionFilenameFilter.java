package hint.util;


import java.awt.*;
import java.io.*;


public class ExtensionFilenameFilter extends javax.swing.filechooser.FileFilter implements FilenameFilter, FileFilter
{
    private String extension;
    private String description;


    public ExtensionFilenameFilter(String extension, String description)
    {
        if (extension == null)
            throw new IllegalArgumentException("extension is null");
        if (description == null)
            throw new IllegalArgumentException("description is null");

        this.extension   = extension;
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

        return name.toLowerCase().endsWith(extension);
    }


    public String getDescription()
    {
        return description + " ("+extension+")";
    }
}
