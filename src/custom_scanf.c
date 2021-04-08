#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

int custom_scanf(char **buf_ptr)
{
	int size_of_buffer = 10;
	int size = 0;
	char c;
	int n_characters = 0;
	char *buffer;

	buffer = malloc(sizeof(char) * size_of_buffer);
	if (!buffer) {
		printf("%s\n", strerror(errno));
		return -1;
	}

	while ((c = getchar()) != EOF && (c != '\n')) {
		buffer[n_characters] = c;
		n_characters++;

		/* if buffer is full realloc */
		if (n_characters == size_of_buffer) {
			size_of_buffer *= 2;
			buffer = realloc(buffer, sizeof(char) * size_of_buffer);
			if (!buffer) {
				printf("%s\n", strerror(errno));
				return -1;
			}
		}
	}
	*buf_ptr = buffer;
	return n_characters;
}
