package hint.gui;


import java.util.*;


public class Prompt
{
    private String  prompt;
    private String  moduleName;

    public static final String DEFAULT_PROMPT = "%m> ";
    public static final String NO_MODULE_NAME = "Prelude";


    public Prompt()
    {
        this(DEFAULT_PROMPT);
    }


    public Prompt(String prompt)
    {
        if (prompt == null)
            throw new IllegalArgumentException("prompt is null");

        this.prompt     = prompt;
        this.moduleName = NO_MODULE_NAME;
    }


    public void setModuleName(String moduleName)
    {
        if (moduleName == null)
            this.moduleName = NO_MODULE_NAME;
        else
            this.moduleName = moduleName;
    }


    public String toString()
    {
        StringBuffer output = new StringBuffer();

        for(int i=0; i < prompt.length(); i++)
        {
            if (prompt.charAt(i) == '%' && i <= prompt.length())
            {
                i++;
                switch(prompt.charAt(i))
                {
                    case '%':
                        output.append('%');
                        break;

                    case 'm':
                        output.append(moduleName);
                        break;

                    default:
                        output.append('%');
                        output.append(prompt.charAt(i));
                        break;
                }
            }
            else
            {
                output.append(prompt.charAt(i));
            }
        }

        return output.toString();
    }
}
