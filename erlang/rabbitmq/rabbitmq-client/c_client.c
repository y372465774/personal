#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
#include <arpa/inet.h>

#define MY_SIZE int
#define BUFSIZE 1024
struct frame
{
    char type;
    short channel;
    long size;
    char *content;//[BUFSIZE];
    char end;
};
MY_SIZE get_value(char const *str,short size)
{
    short i = size-1;
    MY_SIZE value = 0;
    MY_SIZE base = 1;
    while(i>=0)
    {
        MY_SIZE temp = (unsigned char)(str[i]);
        value = value + base*temp;
        base *= 256;
        --i;
    }
    return value;
}
void changeToValue(void const *source,void const* destination,unsigned char len)
{
    unsigned char *s = (unsigned char*) source,*d = (unsigned char*)destination;
    for(;len;len--){
        *(d+len-1) = *s;
        s++;
    }
}
void get_classId_methodId(char const* buf)
{
    printf("classId = %d\n",get_value(buf,2));
    printf("methodId = %d\n",get_value(buf+2,2));
}

void parse(char *amqp,struct frame * fr)
{
    int i;
    fr->type = amqp[0];
    fr->channel = get_value(amqp+1,2);
    //fr->size = get_value(amqp+3,4);
    changeToValue(amqp+3,&fr->size,4);
    fr->content = amqp+7;
    fr->end = amqp[7+fr->size];
    //memcpy(&fr->channel,amqp+1,sizeof(short));
    //memcpy(&fr->size,amqp+3,sizeof(long));
    //for(i = 0; i< 4 ;i++)
    //    printf("%d ",(unsigned char)(amqp[i+3]));
}
void print_fr(struct frame* fr)
{
    printf("type:%d\n",(int)fr->type);
    printf("chananel:%d\n",(int)fr->channel);
    printf("size:%d\n",(int)fr->size);
    get_classId_methodId(fr->content);
    printf("end:%x \n",(unsigned char)fr->end);
}
int main(int argc,char **argv)
{
    int i;    
    int sockfd;
    unsigned char rcvBuf[BUFSIZE];
    char reqBuf[512];
    
    struct sockaddr_in server_addr;
    //struct hostent *host;
    int host;
    int port;
    int len;

    fd_set rfds,orfds;
    int ret,maxfd = -1;
    
    /*  parameter check */
    if(argc!=3)
    {
        fprintf(stderr,"Usage: %s hostip port \a\n",argv[0]);
        exit(1);
    }
    /*
    if((host = gethostbyname(argv[1]))==NULL)
    {
        bail("gethostbyname()");
    }
    */
    if((port=atoi(argv[2]))<0)
    {
        fprintf(stderr,"Usage: %s hostip port\a\n",argv[0]);
        exit(1);
    }
    

    /* create socket  */
    if((sockfd=socket(PF_INET,SOCK_STREAM,0))==-1)
    {
        printf("socket()");
    }

    /*  create server inet  */
    memset(&server_addr,0,sizeof(server_addr));
    server_addr.sin_family=PF_INET; // protocoll family
    server_addr.sin_port=htons(port);// change host-byte to net-byte
    //inet_aton(&host,&argv[1]);// change *.*.*.*(string) to net-byte
    server_addr.sin_addr.s_addr=inet_addr(argv[1]);
    
    /*  connect to server  */
    if(connect(sockfd,(struct sockaddr *)(&server_addr),sizeof(server_addr))==-1)
    {
        printf("connect()");
    }
    
    
    printf("connected to server :%s \n",inet_ntoa(server_addr.sin_addr));
    strcpy(rcvBuf,"AMQP");
    rcvBuf[4]=0;
    rcvBuf[5]=0;
    rcvBuf[6]=9;
    rcvBuf[7]=1;

    send(sockfd,rcvBuf,8,0);
    printf("len= %d\n",len);
    len = recv(sockfd,rcvBuf,BUFSIZE,0);
    if(len > 0) rcvBuf[len] = '\0';
    //for(i = 0 ;i < len ; i++)
    //    printf("%c ",rcvBuf[i]);
   // printf("\n");
    struct frame fr;
    parse(rcvBuf,&fr);
    print_fr(&fr);
    close(sockfd);
    return 0;
}
