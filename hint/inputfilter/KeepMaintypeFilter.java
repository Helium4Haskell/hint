package hint.inputfilter;


import hint.interpreter.*;


/**
 * This function removes all type-messages from the input, except the type
 * of the main function.
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0 (5 feb 2003)
 * @since   HI 1.0
 */

public class KeepMaintypeFilter extends AbstractInputFilter
{
    private String expression;


    public KeepMaintypeFilter(InputObserver delegate, String expression)
    {
        super(delegate);
        this.expression = expression;
    }


    public void inputRecieved(String data)
    {
        int typeIndex = data.indexOf(" :: ");
        if (typeIndex > 0)
        {
            if (data.startsWith(HeliumProcess.MAIN_FUNCTION))
                super.inputRecieved(expression + data.substring(typeIndex));

            return;
        }

        super.inputRecieved(data);
    }
}
