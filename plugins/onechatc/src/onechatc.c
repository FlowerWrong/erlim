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
	char * p = "{ \"cmd\": \"login\", \"name\": \"13560474456\", \"pass\": \"12345678\", \"ack\": \"72cdf1ae-62a3-4ebf-821c-a809d1931293\", \"device\": \"android-xiaomi\" }";
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

	while (1) {
        ssize_t num_bytes_rcvd = recv(sock_client, recvbuf, sizeof(recvbuf), 0);
        printf("recv data len is %zd\n", num_bytes_rcvd);
        if ( num_bytes_rcvd < 0) {
			perror("recv error");
			break;
		}
        recvbuf[num_bytes_rcvd] = '\0';
        for (int i = 0; i < num_bytes_rcvd; ++i) {
            putchar(*(recvbuf + i));
        }

        FILE *fp = fopen("help.txt", "wb");
        fwrite(recvbuf, sizeof(recvbuf), 1, fp);
        fclose(fp);

		memset(recvbuf, 0, sizeof(recvbuf));
	}

	close(sock_client);
	return 0;
}
