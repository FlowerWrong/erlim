#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <netinet/in.h>
#include <arpa/inet.h>
// unix std
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

// json-c
#include <stddef.h>
#include "json.h"
#include "json_object.h"

#include <errno.h>

/*
 * gcc onechat.c -I/usr/local/include/json-c -L/usr/local/lib -ljson-c
 * http://www.gnu.org/software/libc/manual/html_node/Sockets.html
*/

#define MYPORT  8080
#define BUFFER_SIZE 1024

int main() {
    // 定义sockfd
    int sock_client = socket(AF_INET, SOCK_STREAM, 0);

    json_object *new_obj;

    // 定义sockaddr_in
    struct sockaddr_in servaddr;
    // memset() 函数常用于内存空间初始化
    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_port = htons(MYPORT);  // 服务器端口
    servaddr.sin_addr.s_addr = inet_addr("127.0.0.1");  // 服务器ip

    // 连接服务器，成功返回0，错误返回-1
    if (connect(sock_client, (struct sockaddr *) &servaddr, sizeof(servaddr)) < 0) {
        perror("connect error");
        exit(1);
    }
    char *p = "{ \"cmd\": \"login\", \"name\": \"13560474456\", \"pass\": \"12345678\", \"ack\": \"72cdf1ae-62a3-4ebf-821c-a809d1931293\", \"device\": \"android-xiaomi\" }";
    printf("payload is %s\n", p);
    int payloadlen = strlen(p);
    printf("paylen is %d\n", payloadlen);
    new_obj = json_tokener_parse(p);
    json_object_put(new_obj);

    char loginbuf[BUFFER_SIZE] = "ONECHAT/1.0\r\nPAYLOAD_LEN: ";
    char payloadlenstr[10];
    sprintf(payloadlenstr, "%d", payloadlen);
    strcat(loginbuf, payloadlenstr);
    strcat(loginbuf, "\r\n\r\n");
    strcat(loginbuf, p);

    if (send(sock_client, loginbuf, strlen(loginbuf), 0) < 0) {
        perror("send error");
        exit(1);
    }

    char recvbuf[BUFFER_SIZE];

    // TODO 异步io
    while (1) {
        int err;
        ssize_t num_bytes_rcvd = recv(sock_client, recvbuf, sizeof(recvbuf), 0);
        err = errno;
        printf("errno is %d\n", err);
        printf("recv data len is %zd\n", num_bytes_rcvd);
        // Note: recv <= 0
        if (num_bytes_rcvd == 0) {
            break;
        } else if (num_bytes_rcvd < 0) {
            perror("recv error");
            break;
        }
        recvbuf[num_bytes_rcvd] = '\0';

        printf("recv data is %s \n", recvbuf);

        char *p;
        int i = 0;
        char *arr[4];
        p = strtok(recvbuf, "\n");
        while (p != NULL) {
            arr[i++] = p;
            printf("%s\n", p);
            p = strtok(NULL, "\n");
        }

        json_object *reply_obj = json_tokener_parse(arr[3]);
        json_object *ack;
        if (json_object_object_get_ex(reply_obj, "ack", &ack))
            printf("The value of ack is %s\n", json_object_get_string(ack));
        json_object_put(reply_obj);
        json_object_put(ack);

        memset(recvbuf, 0, sizeof(recvbuf));
    }
    printf("close socket. \n");
    close(sock_client);
    return 0;
}
