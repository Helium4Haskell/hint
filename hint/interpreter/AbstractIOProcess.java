package hint.interpreter;


import java.io.*;
import java.util.*;

import hint.util.*;


/**
 * This is an abstraction of a process. It allows it's input to be observed
 * using the InputObserver interface. Besides observing, some facilities are
 * available to manipulate the process, such as writing data to it's input
 * stream or to terminate the process. Do not call these methods from one of
 * the methods of the observer (same thread), as it could deadlock the
 * process.
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0 (5 feb 2003)
 * @since   HI 1.0
 */

abstract public class AbstractIOProcess
{
    private InputObserver  observer;
    private Process        externalProcess;
    private int            lastExitCode;

    private ShutdownHook   shutdownHook;
    private Thread         inputProcessor;
    private Thread         errorProcessor;

    private BufferedReader input;   // reader for data from the external process
    private BufferedReader error;   // reader for errors from the external process
    private PrintWriter    output;  // writer for data to the external process


    protected AbstractIOProcess()
    {
        observer        = null;
        externalProcess = null;
        lastExitCode    = -1;

        shutdownHook    = null;
        inputProcessor  = null;
        errorProcessor  = null;

        input  = null;
        error  = null;
        output = null;
    }


    public void sendData(String data)
    {
        if (data == null)
            throw new IllegalArgumentException("parameter data is null");

        output.println(data);
    }


    public void terminate()
    {
        externalProcess.destroy();
        waitUntilFinished();
    }


    public int getExitCode()
    {
        waitUntilFinished();
        return lastExitCode;
    }


    public void waitUntilFinished()
    {
        if (externalProcess != null)
            try { externalProcess.waitFor(); }
            catch(InterruptedException e) { Log.getLogger().throwing(getClass().getName(), "waitUntilFinished", e); }

        if (inputProcessor != null)
            try { inputProcessor.join(); }
            catch(InterruptedException e) { Log.getLogger().throwing(getClass().getName(), "waitUntilFinished", e); }

        if (errorProcessor != null)
            try { errorProcessor.join(); }
            catch(InterruptedException e) { Log.getLogger().throwing(getClass().getName(), "waitUntilFinished", e); }

        if (externalProcess != null)
        {
            cleanup();
            lastExitCode = externalProcess.exitValue();
        }
    }


    protected void setObserver(InputObserver observer)
    {
        if (observer == null)
            throw new IllegalArgumentException("Observer parameter is null");

        this.observer = observer;
    }


    protected void execute(Commandline commandline) throws IOException
    {
        externalProcess = Runtime.getRuntime().exec(commandline.toCommandArray(), ProcessEnvironment.getEnvironment().getEnvironmentSettings(), commandline.getWorkingDirectory());
        shutdownHook    = new ShutdownHook(externalProcess);

        input  = new BufferedReader(new InputStreamReader(externalProcess.getInputStream()));
        error  = new BufferedReader(new InputStreamReader(externalProcess.getErrorStream()));
        output = new PrintWriter(externalProcess.getOutputStream(), true);

        inputProcessor = new InputStreamProcessor(input);
        errorProcessor = new ErrorStreamProcessor(error);
        inputProcessor.start();
        errorProcessor.start();
    }


    private synchronized void cleanup()
    {
        if (shutdownHook != null)
        {
            shutdownHook.removeHook();
            shutdownHook = null;
        }

        try { input.close(); }
        catch(IOException e) { Log.getLogger().throwing(getClass().getName(), "cleanup", e); }

        try { error.close(); }
        catch(IOException e) { Log.getLogger().throwing(getClass().getName(), "cleanup", e); }

        output.close();
    }


    private class InputStreamProcessor extends AbstractInputProcessor
    {
        protected InputStreamProcessor(BufferedReader input)
        {
            super(input);
        }


        protected void relayInput(String data)
        {
            if (observer != null)
                observer.inputRecieved(data);
        }
    }


    private class ErrorStreamProcessor extends AbstractInputProcessor
    {
        protected ErrorStreamProcessor(BufferedReader input)
        {
            super(input);
        }


        protected void relayInput(String data)
        {
            if (observer != null)
                observer.errorRecieved(data);
        }
    }


    abstract private class AbstractInputProcessor extends Thread
    {
        public static final int BUFFER_SIZE = 64;

        protected Reader input;


        protected AbstractInputProcessor(BufferedReader input)
        {
            super();
            this.input = input;

            setPriority(Thread.MIN_PRIORITY);
        }


        public void run()
        {
            StringBuffer buffer = new StringBuffer();
            boolean readCarriageReturn = false;
            int c = -1;
            int charactersCount = 0;

            try
            {
                while((c = input.read()) != -1)
                {
                    if (readCarriageReturn && c != '\n')
                        buffer.append('\n');

                    if (c == '\r')
                        readCarriageReturn = true;
                    else
                    {
                        buffer.append((char) c);
                        readCarriageReturn = false;
                    }

                    if (!input.ready() || charactersCount >= BUFFER_SIZE)
                    {
                        relayInput(buffer.toString());
                        buffer.setLength(0);
                        charactersCount = 0;
                    }

                    charactersCount++;
                }
            }
            catch(IOException e) { Log.getLogger().throwing(getClass().getName(), "run", e); }
            catch(NullPointerException e) { /* if process has been interrupted */ }

            if (c == -1 && readCarriageReturn)
                buffer.append('\n');

            if (buffer.length() > 0)
                relayInput(buffer.toString());
        }


        abstract protected void relayInput(String data);
    }


    private static class ShutdownHook extends Thread
    {
        protected boolean isHookRemoved;
        protected Process externalProcess;


        public ShutdownHook(Process externalProcess)
        {
            super();

            this.externalProcess = externalProcess;
            this.isHookRemoved   = false;

            Runtime.getRuntime().addShutdownHook(this);
        }


        public void run()
        {
            externalProcess.destroy();
        }


        public void removeHook()
        {
            if (!isHookRemoved)
            {
                Runtime.getRuntime().removeShutdownHook(this);
                isHookRemoved   = true;
                externalProcess = null;
            }
        }
    }
}
