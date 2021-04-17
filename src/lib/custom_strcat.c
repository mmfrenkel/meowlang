#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

char *custom_strcat(char *lhs, char *rhs)
{
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
  memcpy(buffer+strlen(lhs), rhs, strlen(rhs));
  /* null terminate the buffer */
	buffer[strlen(lhs) + strlen(rhs)]= '\0';
  // return "\0"; // buffer gets printed correctly at this point
  return buffer; // segfault at this point
}

// int main(void)
// {
//   char *hello = "hello ";
//   char *goodbye = "goodbye ";
//   char *hg = custom_strcat(hello, goodbye);
//   char *gh = custom_strcat(goodbye, hello);
//   printf("%s", hg);
//   printf("%s", gh);
//   return 0;
// }
