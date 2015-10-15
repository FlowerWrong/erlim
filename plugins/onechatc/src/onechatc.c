#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/shm.h>

/*
 * http://www.gnu.org/software/libc/manual/html_node/Sockets.html
*/

#define MYPORT  8080
#define BUFFER_SIZE 1024

int main() {
	// 定义sockfd
	int sock_cli = socket(AF_INET, SOCK_STREAM, 0);

	// 定义sockaddr_in
	struct sockaddr_in servaddr;
	// memset() 函数常用于内存空间初始化
	memset(&servaddr, 0, sizeof(servaddr));
	servaddr.sin_family = AF_INET;
	servaddr.sin_port = htons(MYPORT);  // 服务器端口
	servaddr.sin_addr.s_addr = inet_addr("127.0.0.1");  // 服务器ip

	// 连接服务器，成功返回0，错误返回-1
	if (connect(sock_cli, (struct sockaddr *) &servaddr, sizeof(servaddr))
			< 0) {
		perror("connect");
		exit(1);
	}

	char sendbuf[BUFFER_SIZE];
	char recvbuf[BUFFER_SIZE];
	while (fgets(sendbuf, sizeof(sendbuf), stdin) != NULL) {
		send(sock_cli, sendbuf, strlen(sendbuf), 0); // 发送
		if (strcmp(sendbuf, "exit\n") == 0)
			break;
		recv(sock_cli, recvbuf, sizeof(recvbuf), 0); // 接收
		fputs(recvbuf, stdout);

		memset(sendbuf, 0, sizeof(sendbuf));
		memset(recvbuf, 0, sizeof(recvbuf));
	}

	close(sock_cli);
	return 0;
}
