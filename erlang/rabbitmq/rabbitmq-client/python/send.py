#! /usr/bin/env python

import pika
import sys

connection = pika.BlockingConnection(pika.ConnectionParameters(
    host='192.168.115.135'))

channel = connection.channel();

channel.exchange_declare(exchange = 'first_ex',
                        type = 'direct')

channel.queue_declare(queue='first_queue')

channel.queue_bind(exchange = 'first_ex',
                    queue = 'first_queue',
                    routing_key = 'first_routingkey')

message = 'hello ,this is my first test'
channel.basic_publish(exchange='first_ex',
        routing_key='first_routingkey',
        body=message)

print "[x] Sent %s"%(message,)

connection.close()
