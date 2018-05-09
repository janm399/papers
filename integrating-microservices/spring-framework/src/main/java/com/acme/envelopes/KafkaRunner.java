package com.acme.envelopes;

import com.google.protobuf.Any;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;

import java.time.ZonedDateTime;
import java.util.UUID;

@Component
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
            Messages.User user = Messages.User.newBuilder()
                    .setId(UUID.randomUUID().toString())
                    .setName("Jan")
                    .setDate(ZonedDateTime.now().toString())
                    .build();
            Envelopes.Envelope envelope = Envelopes.Envelope.newBuilder()
                    .setCorrelationId(UUID.randomUUID().toString())
                    .setToken("t")
                    .setPayload(Any.pack(user))
                    .build();

            this.kafkaTemplate.send("topic-v1", envelope.toByteArray());

            Thread.sleep(1000L);
        }
    }

}
