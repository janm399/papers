package com.acme.envelopes;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.kafka.KafkaProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.annotation.EnableKafka;
import org.springframework.kafka.annotation.KafkaListenerAnnotationBeanPostProcessor;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.config.KafkaListenerContainerFactory;
import org.springframework.kafka.core.*;
import org.springframework.kafka.listener.ConcurrentMessageListenerContainer;
import org.springframework.messaging.converter.GenericMessageConverter;
import org.springframework.messaging.converter.MessageConverter;

@Configuration
@EnableKafka
public class KafkaConfiguration {
    private final KafkaProperties kafkaProperties;

    @Autowired
    public KafkaConfiguration(KafkaProperties kafkaProperties) {
        this.kafkaProperties = kafkaProperties;
    }

    @Bean
    EnvelopeAwareMessageHandlerMethodFactory envelopeAwareMessageHandlerMethodFactory() {
        return new EnvelopeAwareMessageHandlerMethodFactory();
    }

    @Bean
    KafkaListenerContainerFactory<ConcurrentMessageListenerContainer<byte[], byte[]>> kafkaListenerContainerFactory(
            KafkaListenerAnnotationBeanPostProcessor<?, ?> postProcessor,
            EnvelopeAwareMessageHandlerMethodFactory envelopeAwareMessageHandlerMethodFactory,
            ConsumerFactory<byte[], byte[]> consumerFactory) {
        postProcessor.setMessageHandlerMethodFactory(envelopeAwareMessageHandlerMethodFactory);
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
