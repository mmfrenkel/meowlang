#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/**
 *  Returns 0 if the strings are different, and 1 if they match.
 *  Note that this is the reverse of the ordering strncmp.
 */
int custom_strcmp(char *str1, char *str2)
{
	if (strncmp(str1, str2, strlen(str1)))
		return 0;
	return 1;
}
