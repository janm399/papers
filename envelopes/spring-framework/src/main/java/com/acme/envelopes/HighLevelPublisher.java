package com.acme.envelopes;

import com.google.protobuf.Any;

import java.time.LocalDateTime;
import java.util.UUID;

public class HighLevelPublisher {

    void foo() {
        final Messages.User user = Messages.User.newBuilder()
                .setId(UUID.randomUUID().toString())
                .setName("Jan")
                .setDate(LocalDateTime.now().toString())
                .build();
        final Envelopes.Envelope envelope = Envelopes.Envelope.newBuilder()
                .setCorrelationId("")
                .setToken("")
                .setPayload(Any.pack(user))
                .build();

        System.out.println(envelope);
    }

}
