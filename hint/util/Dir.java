package hint.util;


import java.io.*;
import java.util.*;


/**
 * This class provides some utility functions on directories. Currently, it
 * only contains a function that will copy a directory one directory
 * recursivly to another, copying files only when required (if the modified
 * time is older than that of the origional).
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0
 * @since   HI 1.0
 */

public class Dir
{
    public static final int BUFFER_SIZE = 131072;


    public static String stripFileExtention(String filename, String extention)
    {
        int extentionStart = filename.lastIndexOf(extention);
        if (extentionStart < 0)
            return filename;

        return filename.substring(0, extentionStart);
    }


    public static void updateDir(File sourceDir, File destDir) throws IOException
    {
        if (!sourceDir.exists() || !sourceDir.isDirectory())
            throw new IllegalArgumentException("Invalid source directory: "+sourceDir);

        if (destDir.exists() && !destDir.isDirectory())
            throw new IllegalArgumentException("Invalid destination directory:'"+destDir);

        if (sourceDir.equals(destDir))
            return;

        if (!destDir.exists())
        {
            if (!destDir.mkdirs())
                throw new IOException("Unable to create directory:"+destDir);
        }

        List sourceFiles = Arrays.asList(sourceDir.listFiles());
        List destFiles   = Arrays.asList(destDir.listFiles());

        Iterator iterator = sourceFiles.iterator();
        while(iterator.hasNext())
        {
            File sourceFile = (File) iterator.next();
            File destFile   = new File(destDir, sourceFile.getName());

            if (sourceFile.isDirectory())
            {
                updateDir(sourceFile, destFile);
            }
            else
            {
                if (needReplaceFile(sourceFile, destFiles))
                    copyFile(sourceFile, destFile);
            }
        }
    }


    protected static boolean needReplaceFile(File sourceFile, List destFiles)
    {
        if (!sourceFile.isFile())
            throw new IllegalArgumentException("Source-file is not a file: "+sourceFile);

        Iterator iterator = destFiles.iterator();
        while(iterator.hasNext())
        {
            File destFile = (File) iterator.next();
            if (destFile.getName().equals(sourceFile.getName()) &&
                destFile.lastModified() >= sourceFile.lastModified())
                return false;
        }

        return true;
    }


    protected static void copyFile(File sourceFile, File destFile) throws IOException
    {
        if (sourceFile.equals(destFile))
            return;

        if (!sourceFile.isFile())
            throw new IllegalArgumentException("Source-file is not a file: "+sourceFile);

        FileInputStream  is = new FileInputStream(sourceFile);
        FileOutputStream os = new FileOutputStream(destFile);

        byte[] buffer = new byte[BUFFER_SIZE];
        int nRead;

        while((nRead = is.read(buffer)) > 0)
            os.write(buffer, 0, nRead);

        is.close();
        os.flush();
        os.close();

        if (!destFile.exists() || destFile.length() != sourceFile.length())
            throw new IOException("Malformed copy: "+sourceFile+" to "+destFile);
    }
}
