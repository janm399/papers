package com.acme.envelopes;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.kafka.core.KafkaTemplate;

//@Component
class KafkaRunner implements ApplicationRunner {
    private final KafkaTemplate<byte[], byte[]> kafkaTemplate;

    @Autowired
    public KafkaRunner(KafkaTemplate<byte[], byte[]> kafkaTemplate) {
        this.kafkaTemplate = kafkaTemplate;
    }

    @Override
    public void run(ApplicationArguments args) throws InterruptedException {
        //noinspection InfiniteLoopStatement
        while (true) {
            byte[] data = {1, 2, 3};
            this.kafkaTemplate.send("topic-v1", data);

            Thread.sleep(1000L);
        }
    }

}
