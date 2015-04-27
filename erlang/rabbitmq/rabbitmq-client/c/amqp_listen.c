#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <stdint.h>
#include <amqp_tcp_socket.h>
#include <amqp.h>
#include <amqp_framing.h>

#include <assert.h>
void amqp_dump(void const *buffer,size_t len)
{
    unsigned char * buf = (unsigned char *)buffer;
    size_t i;
    size_t j;
    for(i=j=0;i<len;i++){
        printf("%02X:",(buf[i]));
        if((i+1)%10 == 0) {
            printf("          ");
            for(;j<=i;j++)
                printf("%c",buf[j]);
            printf("\n");
        }   
    }   
    printf("              ");
    for(;j<len;j++)
        printf("%c",buf[j]);
    printf("\n");
} 

int main(int argc, char const *const *argv)
{
  char const *hostname;
  int port, status;
  char const *exchange;
  char const *bindingkey;
  amqp_socket_t *socket = NULL;
  amqp_connection_state_t conn;

  amqp_bytes_t queuename; 
  char const * myque = "first_queue";

  if (argc < 5) {
    fprintf(stderr, "Usage: amqp_listen host port exchange bindingkey\n");
    return 1;
  }

  hostname = argv[1];
  port = atoi(argv[2]);
  exchange = argv[3];
  bindingkey = argv[4];
  printf("%s %d ex= %s r_key=%s\n",hostname,port,exchange,bindingkey);
  conn = amqp_new_connection();

  socket = amqp_tcp_socket_new(conn);
  if (!socket) {
    fprintf(stderr,"creating TCP socket Error\n");
  }

  status = amqp_socket_open(socket, hostname, port);
  if (status) {
    fprintf(stderr,"opening TCP socket\n");
  }

  amqp_login(conn, "/", 0, 131072, 0, AMQP_SASL_METHOD_PLAIN, "guest", "guest");
  amqp_channel_open(conn, 1);/* 创建管道  */
  amqp_get_rpc_reply(conn);
  printf("Channel opened!\n");

  {
    amqp_queue_declare_ok_t *r = amqp_queue_declare(conn, 1,amqp_cstring_bytes(myque), 1, 0, 0, 1,
                                 amqp_empty_table);/* 声明队列   */
    amqp_get_rpc_reply(conn);
    queuename = amqp_bytes_malloc_dup(r->queue);
    if (queuename.bytes == NULL) {
      fprintf(stderr, "Out of memory while copying queue name");
      return 1;
    }
    printf("queue %.*s \n",(int)queuename.len,(char*)queuename.bytes);
  }
  /*  队列绑定  */
  //amqp_queue_bind(conn, 1, queuename, amqp_cstring_bytes(exchange), amqp_cstring_bytes(bindingkey),
  //                amqp_empty_table);
  //amqp_get_rpc_reply(conn);
  //printf("queue %.*s \n",(int)queuename.len,(char*)queuename.bytes);
  //printf("bund!\n");
  
  amqp_basic_consume(conn, 1, queuename, amqp_empty_bytes, 0, 1, 0, amqp_empty_table);
  amqp_rpc_reply_t reply = amqp_get_rpc_reply(conn);
 
   if(reply.reply_type != AMQP_RESPONSE_NORMAL){
      printf("%d\n",reply.reply.id);
    }
  printf("amqp_consume ok?\n");
  /*  等待消息到达  */
  
  {
      amqp_frame_t frame;
      int result;
      
      amqp_basic_deliver_t *d;
      amqp_basic_properties_t *p;
      size_t body_target;
      size_t body_received;

      while(1){
        amqp_maybe_release_buffers(conn);
        printf("amqp_wait_frame\n");
        result = amqp_simple_wait_frame(conn,&frame);
        printf("Result: %d\n",result);
        if(result < 0 ){
            break;
        }
        printf("Frame type: %d channel: %d\n", frame.frame_type, frame.channel);
        if (frame.frame_type != AMQP_FRAME_METHOD) {
               continue;
        }
        //  printf("%d\n",*((char *)&frame.payload.method.id));
        printf("Method: %s\n",amqp_method_name(frame.payload.method.id));
        if (frame.payload.method.id != AMQP_BASIC_DELIVER_METHOD) {
            continue;
        }
        
        d = (amqp_basic_deliver_t *) frame.payload.method.decoded;
        printf("Delivery: %u exchange: %.*s routingkey: %.*s\n",
                (unsigned) d->delivery_tag,
                (int) d->exchange.len, (char *) d->exchange.bytes,
                (int) d->routing_key.len, (char *) d->routing_key.bytes);
        result = amqp_simple_wait_frame(conn, &frame);
        if (result < 0) {
            break;
        }
        if (frame.frame_type != AMQP_FRAME_HEADER) {
            fprintf(stderr, "Expected header!");
            abort();
        }
        p = (amqp_basic_properties_t *) frame.payload.properties.decoded;
        if (p->_flags & AMQP_BASIC_CONTENT_TYPE_FLAG) {
            printf("Content-type: %.*s\n",
                    (int) p->content_type.len, (char *) p->content_type.bytes);
        }
        printf("----\n");
        body_target = frame.payload.properties.body_size;
        body_received = 0;
        while (body_received < body_target) {
            result = amqp_simple_wait_frame(conn, &frame);
            if (result < 0) {
                break;
            }
            if (frame.frame_type != AMQP_FRAME_BODY) {
                fprintf(stderr, "Expected body!");
                abort();
            }
            body_received += frame.payload.body_fragment.len;
            printf("len -> %d \n",frame.payload.body_fragment.len);
            assert(body_received <= body_target);
            amqp_dump(frame.payload.body_fragment.bytes,
                    frame.payload.body_fragment.len);
        }
        if (body_received != body_target) {
            /* Can only happen when amqp_simple_wait_frame returns <= 0 */
            /* We break here to close the connection */
            break;
        }
        /* everything was fine, we can quit now because we received the reply */
        break;
      }
  }
  /*  关闭连接 释放资源  */
  amqp_channel_close(conn, 1, AMQP_REPLY_SUCCESS);
  amqp_connection_close(conn, AMQP_REPLY_SUCCESS);
  amqp_destroy_connection(conn);

  return 0;
}
