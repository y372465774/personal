#ifndef FD_CONTROL_H
#define FD_CONTROL_H

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/epoll.h>

int setnonblocking(int fd);
void addfd(int epollfd,int fd);
void modfd(int epollfd,int fd,int ev); 
void removefd(int epollfd,int fd);


#endif
