package com.acme.envelopes;

import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
public class HighLevelConsumer {

    @KafkaListener(topics = {"topic-v1"})
    void receive(ConsumerRecord<byte[], byte[]> record) {
        System.out.println("Received " + record);
    }

}
