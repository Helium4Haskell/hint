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
        Runtime.getRuntime().exec(commandline);
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
