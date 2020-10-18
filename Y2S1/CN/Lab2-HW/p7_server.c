#include <arpa/inet.h>
#include <ctype.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdbool.h>

#define MIN(a, b) ((a) < (b) ? (a) : (b))

bool is_num(char* s) {
  while (*s)
    if (!isdigit(*s++))
      return false;

  return true;
}

void recv_expect(int fd, void* buf, size_t size) {
  if (recv(fd, buf, size, MSG_WAITALL) != size) {
    printf("Error: Didn't recv enough.\n");
    exit(EXIT_FAILURE);
  }
}

void send_expect(int fd, const void* buf, size_t size) {
  if (send(fd, buf, size, 0) < size) {
    printf("Error: Didn't send enough.\n");
    exit(EXIT_FAILURE);
  }
}

void handle_client(int client, struct sockaddr_in client_addr) {
  size_t len, i, l;
  char* s;

  recv_expect(client, &len, sizeof(len));
  s = malloc(len * sizeof(char) + 1);

  recv_expect(client, s, len);
  s[len] = 0;

  recv_expect(client, &i, sizeof(i));
  recv_expect(client, &l, sizeof(l));

  send_expect(client, s + i, MIN(l, len - i));
}

int main(int argc, char** argv) {
  signal(SIGCHLD, SIG_IGN);

  if (argc < 2) {
    printf("No port was specified.\n");
    return 1;
  }

  if (!is_num(argv[1])) {
    printf("Port must be a number.\n");
    return 2;
  }

  int server_fd;
  struct sockaddr_in server_addr;
  int addr_len = sizeof(server_addr);

  if (!(server_fd = socket(AF_INET, SOCK_STREAM, 0))) {
    perror("Socket creation failed.\n");
    exit(EXIT_FAILURE);
  }

  printf("Listening.\n");


  server_addr.sin_family = AF_INET;
  server_addr.sin_addr.s_addr = INADDR_ANY;
  server_addr.sin_port = atoi(argv[1]);

  printf("Listening.\n");


  if (bind(server_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
    perror("Bind error.\n");
    exit(EXIT_FAILURE);
  }

  if (listen(server_fd, 3) < 0) {
    perror("Listen error.\n");
    exit(EXIT_FAILURE);
  }

  printf("Listening.\n");

  for (;;) {
    struct sockaddr_in client_addr;
    int client = accept(server_fd, (struct sockaddr*)&client_addr, (socklen_t*)&addr_len);

    if (!fork()) {
      if (!client) {
        perror("Accept error.\n");
        exit(EXIT_FAILURE);
      }

      printf("Client connected: %s:%d.\n", inet_ntoa(client_addr.sin_addr), client_addr.sin_port);
      handle_client(client, client_addr);

      close(client);
      return 0;
    }
  }

  return 0;
}
