#ifndef PROCESSPOOL_H
#define PROCESSPOOL_H
        
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/epoll.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include "fd_control.h"

int sig_pipefd[2];//process recv and deal with sig

void sig_handler(int sig)
{
    int save_errno = errno;
    int msg = sig;
    send(sig_pipefd[1],(char *)&msg,1,0);
    errno = save_errno;
}
/*  void (*handler)(int)    */
void addsig(int sig,void(handler)(int),bool restart = true)
{
    struct sigaction sa;
    memset(&sa,0,sizeof(sa));
    sa.sa_handler = handler;
    if(restart)
    {
        sa.sa_flags |= SA_RESTART;
    }
    sigfillset(&sa.sa_mask);
    assert(sigaction(sig,&sa,NULL) != -1);
}

class process
{
    public:
        process():m_pid(-1){};
    public:
        pid_t m_pid;  // process id
        int m_pipefd[2];//use pipe to commucate between parent and child
};

template <typename T>
class processpool
{
    private:
        /* ptivate --> for create()     */
        processpool(int listenfd,int process_number = 8);
    public:
        /* sigle mode , for just have one preocesspool instance , and deal with signal  */
        static processpool<T> *create(int listenfd,int process_number = 8)
        {
            if(!m_instance)
            { 
                m_instance = new processpool<T>{listenfd,process_number};
                printf("m_idx-> pid - > %d\n",(int)getpid());
            }
            return m_instance;
        }
        ~processpool()
        {
            printf(" ~ is %d \n",m_idx);
            delete [] m_sub_process;
        }
        void run();

    private:
        void setup_sig_pipe();
        void run_parent();
        void run_child();

    private:
        
        static const int MAX_PROCESS_NUMBER = 8;//16 ;
        static const int USER_PER_PROCESS = 2000;//65536 ;
        static const int MAX_EVENT_NUMBER = 2000;//0 ;
        static processpool<T>* m_instance;
        
        int m_idx ;// judge parent or child 

        int m_epollfd;
        int m_stop;
        
       // for parent process 
        int m_listenfd;
        int m_process_number ;
        process* m_sub_process;
        
        
};

template <typename T>
processpool<T>* processpool<T>::m_instance = NULL;


template<typename T>
processpool<T>::processpool(int listenfd,int process_number):m_listenfd(listenfd),m_process_number(process_number),m_idx(-1),m_stop(false)
{
    assert( (process_number > 0 ) && (process_number <= MAX_PROCESS_NUMBER) );
    m_sub_process = new process[process_number];
    assert(m_sub_process);

    for(int i = 0;i < process_number; i++)
    {
        int ret = socketpair(PF_UNIX,SOCK_STREAM,0,m_sub_process[i].m_pipefd);
        assert(ret == 0);
        
        m_sub_process[i].m_pid = fork();
        assert(m_sub_process[i].m_pid >= 0);
        /*  father [0]  son [1] */
        if( m_sub_process[i].m_pid > 0)
        {
            close(m_sub_process[i].m_pipefd[1]);
            continue;
        }
        else
        {
            close(m_sub_process[i].m_pipefd[0]);
            m_idx = i;
            break;
        }
    }
}

template<typename T>
void processpool<T>::setup_sig_pipe()
{
    m_epollfd = epoll_create(5);
    assert(m_epollfd != -1);

    int ret = socketpair(PF_UNIX,SOCK_STREAM,0,sig_pipefd);
    assert( ret != -1);

    setnonblocking(sig_pipefd[1]);
    addfd(m_epollfd,sig_pipefd[0]);

    addsig(SIGCHLD,sig_handler);
    addsig(SIGTERM,sig_handler);
    addsig(SIGINT,sig_handler);
    addsig(SIGPIPE,SIG_IGN);

}

template<typename T>
void processpool<T>::run()
{
    if(m_idx != -1)
    {
        run_child();
        return;
    }
    run_parent();
}

template<typename T>
void processpool<T>::run_child()
{
    printf("child %d %d\n",m_idx,(int)getpid());
    //int pipefd = m_sub_process[m_idx].m_pipefd[1];
   
    setup_sig_pipe();
    //communicate with his father with pipe   
    int pipefd = m_sub_process[m_idx].m_pipefd[1];
    addfd(m_epollfd,pipefd);
    epoll_event events[MAX_EVENT_NUMBER];
    T* users = new T[USER_PER_PROCESS];
    assert(users);
    int number = 0;
    int ret = -1;
    while(!m_stop)
    {
        number = epoll_wait(m_epollfd,events,MAX_EVENT_NUMBER,-1);
        if((number < 0) && (errno != EINTR))
        {
            printf("epoll failure\n");
            break;
        }
        for( int i = 0;i < number ;i++)
        {
            int sockfd = events[i].data.fd;
            
            // new connection
            if((sockfd == pipefd) && (events[i].events & EPOLLIN))
            {
                int client = 0;
                printf("?");
                ret = recv(sockfd,(char*)&client,sizeof(client),0);
                if(((ret < 0) && (errno != EAGAIN)) || ret == 0)
                {
                    continue;
                }
                else
                {
                    printf("recv client->%d,id->%d\n",client,m_idx);
                    struct sockaddr_in client_address;
                    socklen_t client_addrlength = sizeof(client_address);

                    int connfd = accept(m_listenfd,(struct sockaddr*)&client_address,&client_addrlength);

                    if(connfd < 0)
                    {
                        printf("errno is: %d id->%d\n",errno,m_idx);
                        continue;
                    }
                    fflush(stdout);
                    addfd(m_epollfd,connfd);
                    users[connfd].init(m_epollfd,connfd,client_address);
                }

            }// deal with signal
            else if( (sockfd == sig_pipefd[0]) && (events[i].events & EPOLLIN))
            {
                int sig;
                char signals[1024];
                ret = recv(sig_pipefd[0],signals,sizeof(signals),0);
                if(ret <= 0)
                {
                    continue;
                }
                else
                {
                    for(int i = 0;i < ret ;i++)
                    {
                        switch(signals[i])
                        {
                            case SIGCHLD:
                                {
                                    pid_t pid;
                                    int stat;
                                    while( (pid = waitpid(-1,&stat,WNOHANG)) > 0 )
                                    {
                                        continue;
                                    }
                                    break;
                                }
                            case SIGTERM:
                            case SIGINT:
                                {
                                    m_stop = true;
                                    break;
                                }
                            default:
                            {
                                break;
                            }
                        }
                    }
                }

            }
            else if(events[i].events & (EPOLLRDHUP | EPOLLHUP | EPOLLERR))
            {
                printf("disconnect...\n");
                users[sockfd].close_conn();
            }
            else if(events[i].events & EPOLLIN)
            {
                if(users[sockfd].read())
                    users[sockfd].process();
                else
                    users[sockfd].close_conn();
            }
            else if(events[i].events & EPOLLOUT)
            {
                if(!users[sockfd].write())
                    users[sockfd].close_conn();

            }
        }
    }
    delete [] users;
    users = NULL;
 
    close(pipefd);
    // close(m_listenfd);
    close(m_epollfd);
}


template<typename T>
void processpool<T>::run_parent()
{
    printf("parents %d\n",(int)getpid());
    
    setup_sig_pipe();    
    addfd(m_epollfd,m_listenfd);
    /*
    epoll_event event;
    event.data.fd = m_listenfd;
    event.events = EPOLLIN | EPOLLRDHUP;
    epoll_ctl(m_epollfd,EPOLL_CTL_ADD,m_listenfd,&event);
    setnonblocking(m_listenfd);
    */
    epoll_event events[MAX_EVENT_NUMBER];
    int sub_process_counter = 0;
    int new_conn = 1;
    int number = 0;
    int ret = -1;

    while(!m_stop)
    {
        number = epoll_wait(m_epollfd,events,MAX_EVENT_NUMBER,-1);
        if((number < 0) && (errno != EINTR))
        {
            printf("epoll failure\n");
            break;
        }
        for(int i = 0;i < number ;i++)
        {
            int sockfd = events[i].data.fd;
            if(sockfd == m_listenfd)
            {
                // find a child to distribute job
                int i = sub_process_counter;
                do
                {
                    if(m_sub_process[i].m_pid != -1)
                    {
                        break;
                    }
                    i = (i+1)%m_process_number;
                }while(i != sub_process_counter);
                
                if(m_sub_process[i].m_pid == -1)
                {
                    m_stop = true;
                    break;
                }
                sub_process_counter = (i+1)%m_process_number;
                send(m_sub_process[i].m_pipefd[0],(char*)&new_conn,sizeof(new_conn),0);
                printf("send requst to child %d %d\n",i,new_conn);
                ++new_conn;
                fflush(stdout);
                
            }
            else if( (sockfd == sig_pipefd[0]) && (events[i].events & EPOLLIN))
            {
                int sig;
                char signals[1024];
                ret = recv(sig_pipefd[0],signals,sizeof(signals),0);
                if(ret <= 0)
                {
                    continue;
                }
                else
                {
                    for (int i = 0; i < ret ;i++)
                    {
                        switch(signals[i])
                        {
                            case SIGCHLD:
                                {
                                    pid_t pid;
                                    int stat;
                                    while( (pid = waitpid(-1,&stat,WNOHANG)) > 0 )
                                    {
                                        for(int i = 0;i < m_process_number;i++)
                                        {
                                            if(m_sub_process[i].m_pid == pid)
                                            {
                                                printf("child %d join\n",-1);
                                                close(m_sub_process[i].m_pipefd[0]);
                                                m_sub_process[i].m_pid = -1;
                                            }
                                        }
                                    }
                                    m_stop = true;
                                    for (int i = 0;i < m_process_number; i++)
                                    {
                                        if(m_sub_process[i].m_pid != -1)
                                        {
                                            m_stop = false;
                                        }
                                    }
                                    break;
                                }
                            case SIGTERM:
                            case SIGINT:
                                {
                                    printf("kill all the child now\n");
                                    for(int i = 0;i < m_process_number ;i++)
                                    {
                                        int pid = m_sub_process[i].m_pid;
                                        if(pid != -1)
                                        {
                                            kill(pid,SIGTERM);
                                        }
                                    }
                                    break;
                                }
                            default:
                                {
                                    break;
                                }
                        }
                    }
                }
            }
            else
            {
                continue;
            }

        }
    }
    // close (m_listenfd);
    
    close(m_epollfd);

}


#endif
