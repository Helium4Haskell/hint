

/**
 * Vul voor TEMPLATE de uit te voeren commandline in. Gebruik %s voor
 * de inhoud van de omgevingsvariable HELIUMBINDIR en %% voor het
 * procent teken.
 *
 * Geef voor HELIUMBINDIR de naam van de omgevingsvariable op die de
 * bin-directory bevat.
 */

#define TEMPLATE     "javaw -DPATH=\"%s;%%PATH%%\" -jar \"%s\\Hint.jar\""
#define HELIUMBINDIR "HELIUMBINDIR"


#include <windows.h>
#include <stdlib.h>
#include <stdio.h>


int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nShowCmd)
{ 
    PROCESS_INFORMATION info;
    STARTUPINFO startup;
    char commandline[MAX_PATH];
    char *heliumBinDir;

    heliumBinDir = getenv(HELIUMBINDIR);
    if (!heliumBinDir || strlen(heliumBinDir) <= 0)
    {
        MessageBox(NULL, "The " HELIUMBINDIR " variable is not available", "Error", 0);
        return -1;
    }

    if (heliumBinDir[strlen(heliumBinDir)-1] == '\\')
        heliumBinDir[strlen(heliumBinDir)-1] = '\0';

    sprintf(commandline, TEMPLATE, heliumBinDir, heliumBinDir);

    memset(&startup, '\0', sizeof(STARTUPINFO));
    memset(&info,    '\0', sizeof(PROCESS_INFORMATION));
	
	CreateProcess(NULL, commandline, NULL, NULL, FALSE, 0, NULL, NULL, &startup, &info);
    /* CreateProcess geeft een niet-nul resultaat ook al gaat het goed. 
	   De LastError is dan 6, ERROR_INVALID_HANDLE volgens WinError.c
	int result;
	char message[100];

	if ((result = ...) != 0)
    {
		result = GetLastError();
		sprintf(message, "Failed with error code %d", result);
        MessageBox(NULL, commandline, message, 0);
        return -1;
    }
	*/

    return 0;
}
