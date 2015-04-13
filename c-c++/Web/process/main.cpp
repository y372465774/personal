#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <assert.h>
#include <fcntl.h>
#include <sys/epoll.h>
#include <pthread.h>

#include "http_conn.h"
#include "processpool.h"

#define BUFFER_SIZE 1024
const char* ip = "127.0.0.1";
int port = 2222;
char buf[BUFFER_SIZE];

int main()
{
    int listenfd,connfd;
    int ret;
    char* p;
    struct sockaddr_in address,conn_address;
    socklen_t conn_length;
    
    listenfd = socket(PF_INET,SOCK_STREAM,0);
    
    if(listenfd == -1)
    {
        printf("error\n");
        exit(1);
    }

    bzero(&address,sizeof(address));
    address.sin_family = PF_INET;
    inet_pton(PF_INET,ip,&address.sin_addr);//--------------    
    address.sin_port = htons(port);//------------
    
    ret = bind(listenfd,(struct sockaddr*)&address,sizeof(address));
    if(ret == -1)
    {
        printf("bind error\n");
        exit(1);
    }

    listen(listenfd,5);
    assert(listenfd != -1);
    
    processpool<http_conn>* pool = processpool<http_conn>::create(listenfd);

    if(pool)
    {
        pool->run();
        delete pool;
    }
    
        /*
        ret = recv(connfd,buf,BUFFER_SIZE-1,0);
        printf("ret = %d \n",ret);
        p = NULL;
        p = strpbrk(buf,"\r\n");
        printf("%s",p);
        printf("%s\n",buf);
        */
   
   close(listenfd);
   //close(connfd);


    return 0;
}

