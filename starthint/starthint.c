

#include <windows.h>


BOOL getHeliumBinDirectory(LPTSTR directoryBuffer, const DWORD bufferCharacterSize)
{
    DWORD valueLength;
    DWORD dirAttributes;

    valueLength = GetEnvironmentVariable("HELIUMBINDIR", directoryBuffer, bufferCharacterSize);
    if (valueLength <= 0 || valueLength > bufferCharacterSize)
        return FALSE;

    if (directoryBuffer[valueLength-1] == '\\')
        directoryBuffer[valueLength-1] = '\0';

    dirAttributes = GetFileAttributes(directoryBuffer);
    if (dirAttributes == 0xFFFFFFFF || !(dirAttributes & FILE_ATTRIBUTE_DIRECTORY))
        return FALSE;

    return TRUE;
}


BOOL createHintProcess(LPCTSTR hintCommandline)
{
    TCHAR               commandline[MAX_PATH+1];
    BOOL                result;
    STARTUPINFO         startupInfo;
    PROCESS_INFORMATION processInfo;
    DWORD               exitCode;

    lstrcpyn(commandline, hintCommandline, min(lstrlen(hintCommandline), MAX_PATH));

    ZeroMemory(&startupInfo, sizeof(STARTUPINFO));
    ZeroMemory(&processInfo, sizeof(PROCESS_INFORMATION));

    startupInfo.cb = sizeof(PROCESS_INFORMATION);

    result = CreateProcess( NULL
                          , commandline
                          , NULL
                          , NULL
                          , FALSE
                          , DETACHED_PROCESS
                          , NULL
                          , NULL
                          , &startupInfo
                          , &processInfo
                          );

    if (!result || !processInfo.hProcess)
        return FALSE;

    exitCode = 0;
    WaitForInputIdle(processInfo.hProcess, INFINITE);
    GetExitCodeProcess(processInfo.hProcess, &exitCode);
    if (exitCode != STILL_ACTIVE)
        return FALSE;

    return TRUE;
}


BOOL createHintCommandline(LPTSTR commandlineBuffer, INT commandlineCharacterSize, LPCTSTR binDirectory)
{
    LPCTSTR commandlineTemplate = "javaw -DPATH=\"%s;%%PATH%%\" -jar \"%s\\Hint.jar\"";

    if (lstrlen(commandlineTemplate) + 2 * commandlineCharacterSize <= lstrlen(binDirectory))
        return FALSE;

    wsprintf(commandlineBuffer, commandlineTemplate, binDirectory, binDirectory);
    return TRUE;
}


int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nShowCmd)
{
    TCHAR binDirectory[MAX_PATH+1];
    TCHAR commandline[MAX_PATH+1];
    TCHAR errorMessage[MAX_PATH+100];

    if (!getHeliumBinDirectory(binDirectory, MAX_PATH))
    {
        MessageBox(NULL, "Invalid or non-existent HELIUMBINDIR environment variable or directory", "Environment error", MB_ICONERROR);
        return -1;
    }

    if (!createHintCommandline(commandline, MAX_PATH, binDirectory))
    {
        MessageBox(NULL, "Unable to create the commandline", "Commandline error", MB_ICONERROR);
        return -1;
    }

    if (!createHintProcess(commandline))
    {
        wsprintf(errorMessage, "Unable to start Hint with the following commandline:\n%s", commandline);

        MessageBox(NULL, errorMessage, "Execution error", MB_ICONERROR);
        return -1;
    }

    return 0;
}
