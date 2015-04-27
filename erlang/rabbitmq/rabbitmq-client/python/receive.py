#! /usr/bin/env python
import pika
import time
connection = pika.BlockingConnection(pika.ConnectionParameters(
    host='192.168.115.132'))
channel = connection.channel();

channel.exchange_declare(exchange = 'first_ex',
                                type = 'direct')

channel.queue_declare(queue='first_queue')

channel.queue_bind(exchange = 'first_ex',
                            queue = 'first_queue',
                            routing_key = 'first_routingkey') 

print '[*] Waiting for messages. To exit press CTRL+C'

def callback(ch,method,properties,body):
    print "[x] Receive %r "%(body,)
    #time.sleep(body.count('.'))
    #print "[x] Done"
    ch.basic_ack(delivery_tag = method.delivery_tag)

channel.basic_consume(callback,
                      queue='first_queue')

channel.start_consuming()
