#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <stdint.h>
#include <amqp_tcp_socket.h>
#include <amqp.h>
#include <amqp_framing.h>

#include <assert.h>

void do_request(amqp_connection_state_t conn,
                amqp_basic_properties_t *p,
                amqp_basic_deliver_t *d,
                amqp_bytes_t body
                )
{
    /*
     do_request
    */

  /*
   *
   *
   *    you can do some business with the getting 'body'
   *
   * 
   * */
   int i;
   char test[30]; 
   sprintf(test,"%.*s",(int)body.len,(char *)body.bytes);
   for (i = (int)body.len ;i < 20 ;i++)
        test[i] = 'a'+i;
    test[i] = '\0';   
   /*
    * response to client
    * */ 
    amqp_basic_publish(conn,
                        1,
                        amqp_cstring_bytes(""),//exchange),
                        p->reply_to,//amqp_cstring_bytes(routingkey),
                        0,
                        0,
                        p,
                        amqp_cstring_bytes(test));//body);
 
}

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

int main(int argc, char *argv[])
{
  char const *hostname;
  int port, status;
  char const *exchange;
  char const *routingkey;
  char const *messagebody;
  amqp_socket_t *socket = NULL;
  amqp_connection_state_t conn;
  char const *rpc_queue = "rpc_queue";
  amqp_bytes_t listen_queue ;
  //amqp_cstring_bytes("rpc_queue");
  
  if (argc < 3) { /* minimum number of mandatory arguments */
    fprintf(stderr, "usage:\namqp_rpc_sendstring_client host port exchange routingkey messagebody\n");
    return 1;
  }

  hostname = argv[1];
  port = atoi(argv[2]);
  //exchange = argv[3];
  //routingkey = argv[4];
  //messagebody = argv[5];

  /*
     establish a channel that is used to connect RabbitMQ server
  */

  conn = amqp_new_connection();

  socket = amqp_tcp_socket_new(conn);
  if (!socket) {
    printf("creating TCP socket");
  }

  status = amqp_socket_open(socket, hostname, port);
  if (status) {
    printf("opening TCP socket");
  }
  
  printf("established connection!\n");
  amqp_login(conn, "/", 0, 131072, 0, AMQP_SASL_METHOD_PLAIN, "guest", "guest");//                    "Logging in");
  amqp_channel_open(conn, 1);
  amqp_get_rpc_reply(conn);//, "Opening channel");
  
  printf("open channel!\n");
  
  /*
     declare a listen_ queue
  */

  {
    amqp_queue_declare_ok_t *r = amqp_queue_declare(conn, 1, amqp_cstring_bytes(rpc_queue), 0, 0, 0, 1,
                                 amqp_empty_table);
    amqp_get_rpc_reply(conn);
    printf("shen me gui %d\n",r->queue.len);
    
    listen_queue = amqp_bytes_malloc_dup(r->queue);
    printf("declare queue!\n");
  }
  printf("queue %.*s\n",(int)listen_queue.len,(char*)listen_queue.bytes);
  //  listening ...
  
  printf("listening ... \n");
  for(;;)
  {
    amqp_basic_consume(conn, 1, listen_queue, amqp_empty_bytes, 0, 1, 0, amqp_empty_table);
    amqp_get_rpc_reply(conn);//, "Consuming");
    printf("Comsuming!\n");
    //;

    {
      amqp_frame_t frame;
      int result;

      amqp_basic_deliver_t *d;
      amqp_basic_properties_t *p;
      size_t body_target;
      size_t body_received;

      while (1) {
        amqp_maybe_release_buffers(conn);
        result = amqp_simple_wait_frame(conn, &frame);
        printf("Result: %d\n", result);
        if (result < 0) {
          break;
        }

        printf("Frame type: %d channel: %d\n", frame.frame_type, frame.channel);
        if (frame.frame_type != AMQP_FRAME_METHOD) {
          continue;
        }

        printf("Method: %s\n", amqp_method_name(frame.payload.method.id));
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
        //printf("correlation_id = %d", p->correlation_id);
        amqp_dump(p->correlation_id.bytes,p->correlation_id.len);
        printf("properties ok\n");
        
        body_target = frame.payload.properties.body_size;
        body_received = 0;
        printf("start body...\n");
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
         goto out;
            break;
        }
        printf("do_request ... \n");
        do_request(conn,p,d,frame.payload.body_fragment);
        /* everything was fine, we can quit now because we received the reply */
        printf("do_request over...\n");
        break;
      }
    }
  }

  /*
     closing
  */
out :
  amqp_bytes_free(listen_queue);
  amqp_channel_close(conn, 1, AMQP_REPLY_SUCCESS);//, "Closing channel");
  amqp_connection_close(conn, AMQP_REPLY_SUCCESS);//, "Closing connection");
  amqp_destroy_connection(conn);//, "Ending connection");

  return 0;
}


