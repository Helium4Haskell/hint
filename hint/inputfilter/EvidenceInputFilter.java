package hint.inputfilter;


import java.util.*;

import hint.interpreter.*;


/**
 * Removes the 'evidence', output from the compiler that should not be visible
 * to the user of the interpreter, from the output of the helium compiler. This
 * filter assumes that the data is delivered in chunks of  one line each and all
 * output from the compiler is send to the errorRecieved method. Data send to
 * the inputRecieved method is not modified by this filter.
 *
 * @author Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0
 * @since HI 1.0
 */

public class EvidenceInputFilter extends AbstractInputFilter
{
    private boolean    inInputModuleCompileBlock;
    private boolean    inCompileBlock;
    private LinkedList compileBlockBuffer;
    private boolean    removeMinorWarnings;
    private boolean    internalError;


    public EvidenceInputFilter(InputObserver delegate, boolean removeMinorWarnings)
    {
        super(delegate);

        this.inInputModuleCompileBlock = false;
        this.inCompileBlock            = false;
        this.compileBlockBuffer        = new LinkedList();
        this.removeMinorWarnings       = removeMinorWarnings;
        this.internalError             = false;
    }


    public synchronized void errorRecieved(String data)
    {
        if (data.indexOf("INTERNAL ERROR") >=0 || internalError)
		    {
			      delegateBuffer();
            super.errorRecieved(data);
            internalError = true;
            return;
		    }

        if (data.startsWith("Parse error") && data.indexOf(HeliumProcess.HELIUM_INPUT_MODULE) != -1)
        {
            super.errorRecieved("Parse error:\n");
            return;
        }

        if (data.endsWith("is up to date\n"))
            return;

        if (data.startsWith("Compiling"))
        {
            inCompileBlock = true;

            if (data.indexOf(HeliumProcess.HELIUM_INPUT_MODULE) != -1)
                inInputModuleCompileBlock = true;
            else
                compileBlockBuffer.addLast(data);

            return;
        }

        if (data.startsWith("Compilation successful"))
        {
            if (!removeMinorWarnings && data.indexOf("with") != -1 && !inInputModuleCompileBlock)
            {
                super.errorRecieved("\0startgreen\0");
                delegateBuffer();
                super.errorRecieved(data);
            }

            inCompileBlock            = false;
            inInputModuleCompileBlock = false;
            compileBlockBuffer.clear();
            return;
        }

        if (data.startsWith("Compilation failed"))
        {
            boolean wasInInputModuleCompileBlock = inInputModuleCompileBlock;

            inInputModuleCompileBlock = false;
            inCompileBlock = false;
            delegateBuffer();

            if (wasInInputModuleCompileBlock)
                return;
        }

        if (data.indexOf(" :: ") > 0)
        {
            String trimmed = data.trim();
            int typeIndex = trimmed.indexOf(" :: ");

            if (typeIndex > 0 && isSymbol(trimmed.substring(0, typeIndex)))
            {
                super.inputRecieved(trimmed);
                return;
            }
        }

        if (inCompileBlock)
        {
            if (removeMinorWarnings)
            {
                if (data.indexOf("Missing type signature") > -1)
                    return;

                if (data.endsWith("is not used\n"))
                    return;

                if (data.indexOf("shadows the one at") > 0)
                    return;
            }


            if (inInputModuleCompileBlock)
            {
                if (data.indexOf("Missing type signature") > 0)
                    return;
                else if (data.startsWith("Functions:"))
                    return;
                else if (data.startsWith("(") && data.indexOf("): ") > 0)
                    compileBlockBuffer.addLast(data.substring(3+data.indexOf("): ")));
                else
                    compileBlockBuffer.addLast(data);
            }
            else
            {
                if (data.startsWith("(") && data.indexOf("): ") > 0)
                {
                    int endLocationIndex = data.indexOf("): ")+1;
                    enqueueLocations(data.substring(0, endLocationIndex));
                    compileBlockBuffer.addLast(data.substring(endLocationIndex));
                }
                else
                    compileBlockBuffer.addLast(data);
            }

            //if (!data.startsWith("\n"))
                //delegateBuffer();

            return;
        }

        super.errorRecieved(data);
    }


    protected boolean isSymbol(String symbol)
    {
        boolean isSymbol = true;
        for(int i=0; i < symbol.length(); i++)
        if (!Character.isJavaIdentifierPart(symbol.charAt(i)))
            isSymbol = false;
        return isSymbol && symbol.length() > 0;
    }


    protected void enqueueLocations(String locations)
    {
        StringTokenizer tokenizer = new StringTokenizer(locations, "(,)", true);
        while(tokenizer.hasMoreTokens())
        {
            String startToken = tokenizer.nextToken();
            if (startToken.equals(" ") || startToken.equals(","))
                compileBlockBuffer.addLast(startToken);
            else if (startToken.equals("(") && tokenizer.countTokens() >= 4)
            {
                String row   = tokenizer.nextToken();
                String comma = tokenizer.nextToken();
                String col   = tokenizer.nextToken();
                String endParenthesis = tokenizer.nextToken();

                String location = startToken+row+comma+col+endParenthesis;
                compileBlockBuffer.addLast(location);
            }
            else
            {
                compileBlockBuffer.addLast(startToken);
                while(tokenizer.hasMoreTokens())
                    compileBlockBuffer.addLast(tokenizer.nextToken());
            }
        }
    }


    private void delegateBuffer()
    {
        while(!compileBlockBuffer.isEmpty())
            super.errorRecieved((String) compileBlockBuffer.removeFirst());
    }
}
