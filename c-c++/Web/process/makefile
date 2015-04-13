INCLUDE=../include/
HTTP=../http_server/
CC = g++
main : main.o http_conn.o fd_control.o
	$(CC) -o main main.o http_conn.o fd_control.o
main.o : main.cpp $(INCLUDE)http_conn.h $(INCLUDE)processpool.h
	$(CC) -c main.cpp -I$(INCLUDE) -std=c++11
http_conn.o : $(HTTP)http_conn.cpp $(INCLUDE)http_conn.h $(INCLUDE)fd_control.h
	$(CC) -c $(HTTP)http_conn.cpp -I$(INCLUDE)
fd_control.o : fd_control.cpp $(INCLUDE)fd_control.h
	$(CC) -c fd_control.cpp -I$(INCLUDE)
clean :
	-rm *.o
