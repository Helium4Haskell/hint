package hint.inputfilter;


import hint.interpreter.InputObserver;


/**
 * Filter that delegates all input to the errorRecieved method.
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0 (5 feb 2003)
 * @since   HI 1.0
 */

public class RedirectInputFilter extends AbstractInputFilter
{
    private int inputTarget;
    private int errorTarget;

    public static final int TARGET_INPUT = 2;
    public static final int TARGET_ERROR = 4;


    public RedirectInputFilter(InputObserver delegate, int inputTarget, int errorTarget)
    {
        super(delegate);
    }


    public void inputRecieved(String data)
    {
        delegateData(data, inputTarget);
    }


    public void errorRecieved(String data)
    {
        delegateData(data, errorTarget);
    }


    protected void delegateData(String data, int target)
    {
        if (inputTarget == TARGET_INPUT)
            super.inputRecieved(data);
        else
            super.errorRecieved(data);
    }
}
