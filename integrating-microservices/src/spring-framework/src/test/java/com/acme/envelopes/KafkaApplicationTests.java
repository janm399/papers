package com.acme.envelopes;

import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest
@EnableAspectJAutoProxy(proxyTargetClass = true)
public class KafkaApplicationTests {
    @Autowired
    private KafkaTemplate<byte[], byte[]> kafkaTemplate;
    @Autowired
    private KafkaConsumerProbe kafkaConsumerProbe;

    @Test
    public void messagesPublishedAndReceived() {
        ConsumerRecordsProbe probe = this.kafkaConsumerProbe.consumerRecords();

        byte[] data = {1, 2, 3};
        this.kafkaTemplate.send("topic-v1", data);
        ConsumerRecord<?, ?> received = probe.nextConsumerRecord();
        System.out.println(received);
    }

}
