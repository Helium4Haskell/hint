package hint.interpreter;


import java.io.*;
import java.util.*;

import hint.util.*;


/**
 * <p>
 * As an 'interface' to the helium compiler and virtual machine, an Interpreter
 * allows the interpretation of an expression, extraction of a type or loading
 * of a module to be observed and manipulated.
 *
 * <p>
 * Each invocation of the compiler/virtual machine is described by a session,
 * which is managed by the session manager. A session is split into a set
 * of phases (actions) that are executed after eachother. For example, one
 * action executed the compiler and another handles the completion of the
 * virtual machine.
 *
 * <p>
 * The interpreter should be deadlock free, except for one unlikely event:
 * if the helium compiler never ends execution and a sendData() operation is
 * called. SendData() will block until the compiler is finished. All other
 * operations, should never block, since evaluate-operations create a new
 * thread which calls the evaluate method of the manager and the
 * cancelEvaluation() operation will always terminate the compiler.
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0 (5 feb 2003)
 * @since   HI 1.0
 */

public class Interpreter
{
    private File               loadedModule;
    private SessionManager     manager;
    private Session            currentSession;
    private MessageBroadcaster messageBroadcaster;


    public Interpreter()
    {
        loadedModule       = null;
        currentSession     = null;
        manager            = new SessionManager();

        messageBroadcaster = new MessageBroadcaster();
    }


    public void evaluate(HeliumParameters parameters)
    {
        new AsyncEvalRunner(parameters).start();
    }


    public void sendData(String data)
    {
        manager.sendData(data);
    }


    public void cancelEvaluation()
    {
        manager.cancelEvaluation();
    }


    public void addObserver(InterpreterObserver observer)
    {
        messageBroadcaster.addObserver(observer);
    }


    public void removeObserver(InterpreterObserver observer)
    {
        messageBroadcaster.removeObserver(observer);
    }


    public void removeObservers()
    {
        messageBroadcaster.removeObservers();
    }


    public File getLoadedModule()
    {
        return loadedModule;
    }


    private void moduleLoaded(File module)
    {
        this.loadedModule = module;
    }


    protected class SessionManager
    {
        public synchronized void evaluate(HeliumParameters parameters)
        {
            while(currentSession != null && currentSession.isRunning())
                waitForCompletion();

            // note: 27 feb 2003: a loaded module is always explicitly specified. This way
            // the Interpreter's own module bookkeeping is ignored.
            //if (parameters.getModule() == null && loadedModule != null)
                //parameters.setModule(loadedModule);

            currentSession = new Session();
            currentSession.setHeliumParameters(parameters);

            messageBroadcaster.enableBroadcasting();

            Thread runner = new Thread(new Runner());
            runner.start();
        }


        public synchronized void sendData(String data)
        {
            if (currentSession == null)
                return;

            Session session = currentSession;
            while(session.isRunning() && !session.isLVMRunning())
                try { wait(); }
                catch(InterruptedException e) { Log.getLogger().throwing(getClass().getName(), "sendData", e); }

            if (session.isLVMRunning())
                session.getLVMProcess().sendData(data);
        }


        public synchronized void cancelEvaluation()
        {
            if (currentSession == null)
                return;

            Session session = currentSession;
            session.setCanceled();

            if (session.getHeliumProcess() != null)
                session.getHeliumProcess().terminate();

            if (session.getLVMProcess() != null)
                session.getLVMProcess().terminate();

            while(session.isRunning())
                try { wait(); }
                catch(InterruptedException e) { Log.getLogger().throwing(getClass().getName(), "cancelEvaluation", e); }
        }


        public synchronized void lvmStarted()
        {
            notifyAll();
        }


        public synchronized void evaluationStarted()
        {
            messageBroadcaster.evaluationStarted();
        }


        public synchronized void finishEvaluation()
        {
            if (currentSession != null)
                currentSession.endSession();

            messageBroadcaster.evaluationFinished();
            messageBroadcaster.disableBroadcasting();

            currentSession = null;
            notifyAll();
        }


        public synchronized void waitForCompletion()
        {
            if (currentSession == null)
                return;

            Session session = currentSession;
            while(session.isRunning())
                try { wait(); }
                catch(InterruptedException e) { Log.getLogger().throwing(getClass().getName(), "waitForCompletion", e); }
        }
    }


    protected static class Session
    {
        private boolean           isCanceled;
        private boolean           isStarted;

        private HeliumProcess     heliumProcess;
        private LVMProcess        lvmProcess;
        private HeliumParameters  heliumParameters;


        public Session()
        {
            isCanceled       = false;
            isStarted        = true;

            heliumProcess    = null;
            lvmProcess       = null;
            heliumParameters = null;
        }


        public boolean isCanceled()
        {
            return isCanceled;
        }


        public void setCanceled()
        {
            isCanceled = true;
        }


        public void setHeliumProcess(HeliumProcess heliumProcess)
        {
            this.heliumProcess = heliumProcess;
        }


        public HeliumProcess getHeliumProcess()
        {
            return heliumProcess;
        }


        public void setLVMProcess(LVMProcess lvmProcess)
        {
            this.lvmProcess = lvmProcess;
        }


        public LVMProcess getLVMProcess()
        {
            return lvmProcess;
        }


        public void setHeliumParameters(HeliumParameters parameters)
        {
            this.heliumParameters = parameters;
        }


        public HeliumParameters getHeliumParameters()
        {
            return heliumParameters;
        }


        public boolean isRunning()
        {
            return isStarted;
        }


        public boolean isLVMRunning()
        {
            return lvmProcess != null;
        }


        public void endSession()
        {
            isCanceled       = true;
            isStarted        = false;
            heliumProcess    = null;
            lvmProcess       = null;
            heliumParameters = null;
        }
    }


    protected class Runner implements Runnable
    {
        private List actions;


        public Runner()
        {
            actions = new LinkedList();
            actions.add(new StartCompileAction());
            actions.add(new FinishCompileAction());
            actions.add(new StartLVMAction());
            actions.add(new FinishLVMAction());
        }


        public void run()
        {
            AbstractSessionAction action = null;

            manager.evaluationStarted();

            Iterator iterator = actions.iterator();
            while(iterator.hasNext())
            {
                if (currentSession.isCanceled())
                {
                    action.cancelAction();
                    manager.finishEvaluation();
                    return;
                }

                action = (AbstractSessionAction) iterator.next();

                try
                {
                    boolean performedAction = action.performAction();
                    if (!performedAction)
                        break;
                }
                catch(IOException e)
                {
                    Log.getLogger().throwing(getClass().getName(), "run", e);
                    messageBroadcaster.interpreterFailure(e);
                    break;
                }
            }

            manager.finishEvaluation();
        }
    }


    protected class FinishLVMAction extends AbstractSessionAction
    {
        public boolean performAction() throws IOException
        {
            currentSession.getLVMProcess().waitUntilFinished();
            return true;
        }
    }


    protected class StartLVMAction extends AbstractSessionAction
    {
        public boolean performAction() throws IOException
        {
            File lvmFile = currentSession.getHeliumProcess().getCompiledLVMFile();
            if (!lvmFile.exists())
                return false;

            File workingDir = null;
            if (currentSession.getHeliumParameters().getModule() != null)
                workingDir = currentSession.getHeliumParameters().getModule().getParentFile();

            currentSession.setLVMProcess(new LVMProcess(lvmFile, workingDir, messageBroadcaster));
            manager.lvmStarted();
            return true;
        }


        public void cancelAction()
        {
            currentSession.getLVMProcess().terminate();
        }
    }


    protected class FinishCompileAction extends AbstractSessionAction
    {
        public boolean performAction() throws IOException
        {
            HeliumProcess heliumProcess = currentSession.getHeliumProcess();
            if (heliumProcess.getExitCode() != HeliumProcess.COMPILATION_SUCCESSFULL_EXITCODE)
                return false;

            moduleLoaded(currentSession.getHeliumParameters().getModule());

            return !currentSession.getHeliumParameters().compileOnly();
        }
    }


    protected class StartCompileAction extends AbstractSessionAction
    {
        public boolean performAction() throws IOException
        {
            currentSession.setHeliumProcess(new HeliumProcess(messageBroadcaster, currentSession.getHeliumParameters()));
            return true;
        }


        public void cancelAction()
        {
            currentSession.getHeliumProcess().terminate();
        }
    }


    abstract protected class AbstractSessionAction
    {
        abstract public boolean performAction() throws IOException;


        public void cancelAction()
        {
        }
    }


    protected class AsyncEvalRunner extends Thread
    {
        private HeliumParameters parameters;


        public AsyncEvalRunner(HeliumParameters parameters)
        {
            super();
            this.parameters = parameters;
        }


        public void run()
        {
            manager.evaluate(parameters);
        }
    }
}
