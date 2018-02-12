package com.acme.envelopes;

import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
public class HighLevelConsumer {

    @KafkaListener(topics = {"topic-v1"})
    void highLevel(final @Envelope.CorrelationId String correlationId,
                   final @Envelope.Token String token,
                   final @Envelope.Payload Messages.User user) {
        System.out.println("Received " + correlationId + " => " + token + " => " + user);
    }

}
