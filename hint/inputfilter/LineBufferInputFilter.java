package hint.inputfilter;


import hint.interpreter.InputObserver;


/**
 * This filter buffers the input and delegates it in chunks of one line each.
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0 (5 feb 2003)
 * @since   HI 1.0
 */

public class LineBufferInputFilter extends AbstractInputFilter
{
    String previousStrInput;
    String previousStrError;

    protected static final int   IS_INPUT = 2;
    protected static final int   IS_ERROR = 4;


    public LineBufferInputFilter(InputObserver delegate)
    {
        super(delegate);
        previousStrInput = "";
        previousStrError = "";
    }


    public void inputRecieved(String data)
    {
        delegateLines(data, IS_INPUT);
    }


    public void errorRecieved(String data)
    {
        delegateLines(data, IS_ERROR);
    }


    protected void delegateLines(String data, int source)
    {
        String previous = getPrevious(source) + data;

        int lineEndPos;
        while((lineEndPos = previous.indexOf('\n')) != -1)
        {
            String line;

            line     = previous.substring(0, lineEndPos+1);
            previous = previous.substring(lineEndPos+1);

            delegateLine(source, line);
        }

        storePrevious(source, previous);
    }


    private String getPrevious(int source)
    {
        if (source == IS_INPUT)
            return previousStrInput;
        else
            return previousStrError;
    }


    private void storePrevious(int source, String newPrevious)
    {
        if (source == IS_INPUT)
            previousStrInput = newPrevious;
        else
            previousStrError = newPrevious;
    }


    private void delegateLine(int source, String data)
    {
        if (source == IS_INPUT)
            super.inputRecieved(data);
        else
            super.errorRecieved(data);
    }
}
