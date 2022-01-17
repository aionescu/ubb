#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>

int main()
{
    int s;
    struct sockaddr_in server;
    char buffer[500];
    int length;
    uint server_size;
    //creating the socket
    s = socket(AF_INET, SOCK_STREAM, 0);
    if (s < 0)
    {
        printf("eroare la creere socket");
        return 1;
    }
    server.sin_port = htons(1276);
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = inet_addr("127.0.0.1");
    server_size = sizeof(server);
    //connecting to the server

    if (connect(s, (struct sockaddr*)&server, server_size) < 0)
    {
        perror("Connection error.\n");
        exit(EXIT_FAILURE);
    }

    while (1)
    {
        printf("0 piatra, 1 foarfeca, 2 hartie: ");
        fgets(buffer, 500, stdin);
        length = strlen(buffer);
        length = htons(length);
        send(s, (char*)buffer,1, 0);
        printf("message sent \n");
        char answer[500];
        int number;
        int received;
        received = recv(s, (char*)answer,1, MSG_WAITALL);
        answer[received] = '\0';
        if (strcmp(answer, "1") == 0)
        {
            printf("server won");
            break;
        }
        if (strcmp(answer, "2") == 0)
        {
            printf("client won");
            break;
        }
    }
    close(s);
    return 0;
}
