#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

char *custom_strcat(char *lhs, char *rhs)
{
  printf("%s", lhs);
  printf("%s", rhs);

  /* the size of the concatenated string */
  int size_of_buffer = strlen(lhs) + strlen(rhs) + 1;

  /* malloc space for the new string */
	char *buffer;
	buffer = malloc(sizeof(char) * size_of_buffer);
	if (!buffer) {
		printf("%s\n", strerror(errno));
		return "\0";
	}

  memcpy(buffer, lhs, strlen(lhs));
  memcpy(buffer+strlen(lhs), rhs, strlen(rhs)+1);
  /* null terminate the buffer */
	buffer[strlen(buffer) + 1]= '\0';
  printf("%s", buffer);
  return '\0'; // buffer gets printed correctly at this point
  return buffer; // segfault at this point
}