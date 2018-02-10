package com.acme.envelopes;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.kafka.KafkaProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.annotation.EnableKafka;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.config.KafkaListenerContainerFactory;
import org.springframework.kafka.core.*;
import org.springframework.kafka.listener.ConcurrentMessageListenerContainer;

@Configuration
@EnableKafka
public class KafkaConfiguration {
    private final KafkaProperties kafkaProperties;

    @Autowired
    public KafkaConfiguration(KafkaProperties kafkaProperties) {
        this.kafkaProperties = kafkaProperties;
    }

    @Bean
    KafkaListenerContainerFactory<ConcurrentMessageListenerContainer<byte[], byte[]>> kafkaListenerContainerFactory(
            ConsumerFactory<byte[], byte[]> consumerFactory) {
        ConcurrentKafkaListenerContainerFactory<byte[], byte[]> factory =
                new ConcurrentKafkaListenerContainerFactory<>();
        factory.setConsumerFactory(consumerFactory);
        factory.setConcurrency(3);
        factory.getContainerProperties().setPollTimeout(3000);
        return factory;
    }

    @Bean
    KafkaTemplate<byte[], byte[]> kafkaTemplate() {
        return new KafkaTemplate<>(producerFactory());
    }

    @Bean
    ProducerFactory<byte[], byte[]> producerFactory() {
        return new DefaultKafkaProducerFactory<>(this.kafkaProperties.buildProducerProperties());
    }

    @Bean
    ConsumerFactory<byte[], byte[]> consumerFactory() {
        return new DefaultKafkaConsumerFactory<>(this.kafkaProperties.buildConsumerProperties());
    }

}
