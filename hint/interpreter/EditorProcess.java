package hint.interpreter;


import java.io.*;
import javax.swing.*;

import hint.interpreter.*;
import hint.util.*;

public class EditorProcess
{
    public EditorProcess(File file, int row, int column) throws IOException
    {
        if (file == null)
            throw new IllegalArgumentException("file is null");
        if (!file.exists() || !file.isFile())
            throw new IllegalArgumentException("invalid file");
        if (row < 0)
            throw new IllegalArgumentException("row < 0");
        if (column < 0)
            throw new IllegalArgumentException("column > 0");

        String commandline = createCommandline(ProcessEnvironment.getEnvironment().getEditorCommandlineTemplate(), 
                file.getAbsolutePath() /* file.getName()*/, row, column);
        if (commandline.trim().equals(""))
            throw new IOException("empty commandline");
        // else System.out.println(commandline);
 
        String [] command = Utils.prepareForExec(commandline);
        Runtime.getRuntime().exec(command, null); //, file.getParentFile());
    }


    /**
     * Replaces in template;
     *
     *   %f  -> filename
     *   %r  -> row
     *   %c  -> column
     *   %%  -> %
     */

    public String createCommandline(String template, String filename, int row, int column)
    {
        StringBuffer output = new StringBuffer();

        for(int i=0; i < template.length(); i++)
        {
            if (template.charAt(i) == '%' && i <= template.length())
            {
                i++;
                switch(template.charAt(i))
                {
                    case '%':
                        output.append('%');
                        break;

                    case 'f':
                        // output.append("\""); // Not only seems unnecessary but it won't work otherwise. I guess Java deals with this in some way.
                        output.append(filename);
                        // output.append("\"");
                        break;

                    case 'r':
                        output.append(Integer.toString(row));
                        break;

                    case 'c':
                        output.append(Integer.toString(column));
                        break;

                    default:
                        output.append('%');
                        output.append(template.charAt(i));
                        break;
                }
            }
            else
            {
                output.append(template.charAt(i));
            }
        }

        return output.toString();
    }
}
