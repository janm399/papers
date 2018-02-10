package com.acme.envelopes;

import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.annotation.PartitionOffset;
import org.springframework.kafka.annotation.TopicPartition;
import org.springframework.kafka.support.Acknowledgment;
import org.springframework.stereotype.Service;

@Service
public class HighLevelConsumer {

    @KafkaListener(topicPartitions =
        @TopicPartition(topic = "topic-v1", partitionOffsets = @PartitionOffset(partition = "0", initialOffset = "latest"))
    )
    // TODO: The @Envelope.* annotations are the desired shape of the method, where the values are extracted
    // TODO: the payload of the ConsumerRecord<..., x>, where x is convertible to Envelope.
    // TODO: Note in particular the @Envelope.Payload, which performs the google.protobuf.Any conversions
    void receive(final Acknowledgment ack,
                 final @Envelope.CorrelationId String correlationId,
                 final @Envelope.Token String token,
                 final @Envelope.Payload Messages.User user) {

        ack.acknowledge();
    }

}
