package hint.interpreter;


import java.io.*;
import javax.swing.*;

import hint.interpreter.*;
import hint.util.*;


public class BrowserProcess
{
    public BrowserProcess(String url) throws IOException
    {
        String commandline = createCommandline(ProcessEnvironment.getEnvironment().getBrowserCommandlineTemplate(), url);
        // System.out.println ("Executing: " + commandline);
        // String[] command = {"sh","-c",commandline};
        
        // The regular expression uses negative lookbehind to match only
        // those spaces that are not preceded by a \
        // Note the double quotation of the slashback: one for java strings, one for regexps.
        String [] command = Utils.prepareForExec(commandline);
        Runtime.getRuntime().exec(command);
    }


    /**
     * Replaces in template;
     *
     *   %u  -> url
     *   %%  -> %
     */

    public String createCommandline(String template, String url)
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

                    case 'u':
                        // output.append("\""); Seems not to work.
                        output.append(url);
                        // output.append("\"");
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
