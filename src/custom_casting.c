#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>


char *custom_itoa(int val)
{
	int n_chars_in_val = 1; /* need at least one for null terminator */
	char *buffer;
	int stored = val;

	/* find number of characters we need */
	while (val != 0) {
        	val /= 10;
        	n_chars_in_val++;
    	}

	/* actually create the buffer */
	buffer = malloc(sizeof(char) * n_chars_in_val);
	if (!buffer) {
		printf("%s\n", strerror(errno));
		return NULL;
	}

	/* ... and add the values to it */
	if (sprintf(buffer, "%d", stored) != n_chars_in_val - 1) {
		printf("could not cast int to string\n");
		free(buffer);
		return NULL;
	}

	return buffer;
}
