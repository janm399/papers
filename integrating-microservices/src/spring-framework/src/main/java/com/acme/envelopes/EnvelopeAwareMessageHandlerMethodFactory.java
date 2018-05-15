package com.acme.envelopes;

import com.google.protobuf.*;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.core.MethodParameter;
import org.springframework.format.support.DefaultFormattingConversionService;
import org.springframework.kafka.annotation.KafkaListenerAnnotationBeanPostProcessor;
import org.springframework.kafka.support.KafkaNull;
import org.springframework.messaging.Message;
import org.springframework.messaging.converter.GenericMessageConverter;
import org.springframework.messaging.handler.annotation.support.*;
import org.springframework.messaging.handler.invocation.HandlerMethodArgumentResolver;
import org.springframework.messaging.handler.invocation.InvocableHandlerMethod;
import org.springframework.util.ClassUtils;
import org.springframework.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

public class EnvelopeAwareMessageHandlerMethodFactory implements MessageHandlerMethodFactory, BeanFactoryAware, InitializingBean {
    private ConfigurableBeanFactory beanFactory;
    private MessageHandlerMethodFactory messageHandlerMethodFactory;

    @Override
    public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
        this.beanFactory = (ConfigurableBeanFactory)beanFactory;
    }

    @Override
    public void afterPropertiesSet() {
        this.messageHandlerMethodFactory = createDefaultMessageHandlerMethodFactory();
    }

    private MessageHandlerMethodFactory createDefaultMessageHandlerMethodFactory() {
        DefaultMessageHandlerMethodFactory defaultFactory = new DefaultMessageHandlerMethodFactory();
        defaultFactory.setBeanFactory(this.beanFactory);

        DefaultFormattingConversionService conversionService = new DefaultFormattingConversionService();
        defaultFactory.setConversionService(conversionService);

        List<HandlerMethodArgumentResolver> argumentResolvers = new ArrayList<>();

        // Annotation-based argument resolution
        argumentResolvers.add(new EnvelopePayloadMethodArgumentResolver());
        argumentResolvers.add(new HeaderMethodArgumentResolver(conversionService, this.beanFactory));
        argumentResolvers.add(new HeadersMethodArgumentResolver());

        // Type-based argument resolution
        final GenericMessageConverter messageConverter = new GenericMessageConverter(conversionService);
        argumentResolvers.add(new MessageMethodArgumentResolver(messageConverter));
        argumentResolvers.add(new PayloadArgumentResolver(messageConverter) {

            @Override
            protected boolean isEmptyPayload(Object payload) {
                return payload == null || payload instanceof KafkaNull;
            }

        });
        defaultFactory.setArgumentResolvers(argumentResolvers);

        defaultFactory.afterPropertiesSet();
        return defaultFactory;
    }

    @Override
    public InvocableHandlerMethod createInvocableHandlerMethod(Object bean, Method method) {
        return this.messageHandlerMethodFactory.createInvocableHandlerMethod(bean, method);
    }

    static class EnvelopePayloadMethodArgumentResolver implements HandlerMethodArgumentResolver {

        private <A extends Annotation, P> boolean isParameter(MethodParameter parameter, Class<A> annotationType, Class<P> parameterType) {
            final A annotation = parameter.getParameterAnnotation(annotationType);
            return annotation != null && parameterType.isAssignableFrom(parameter.getParameterType());
        }

        @Override
        public boolean supportsParameter(MethodParameter parameter) {
            return isParameter(parameter, Envelope.Payload.class, GeneratedMessageV3.class) ||
                   isParameter(parameter, Envelope.Payload.class, Any.class) ||
                   isParameter(parameter, Envelope.Token.class, String.class) ||
                   isParameter(parameter, Envelope.CorrelationId.class, String.class);
        }

        private Object resolveArgumentEnvelope(MethodParameter parameter, Envelopes.Envelope envelope) throws Exception {
            if (isParameter(parameter, Envelope.CorrelationId.class, String.class)) return envelope.getCorrelationId();
            if (isParameter(parameter, Envelope.Token.class, String.class)) return envelope.getToken();
            if (isParameter(parameter, Envelope.Payload.class, Any.class)) return envelope.getPayload();
            if (isParameter(parameter, Envelope.Payload.class, GeneratedMessageV3.class)) {
                final Any any = envelope.getPayload();
                final Method getDescriptor = ClassUtils.getStaticMethod(parameter.getParameterType(), "getDescriptor");
                final Descriptors.Descriptor descriptor = (Descriptors.Descriptor) ReflectionUtils.invokeMethod(getDescriptor, null);
                final String parameterTypeUrl = "type.googleapis.com/" + descriptor.getFullName();
                if (any.getTypeUrl().equals(parameterTypeUrl)) {
                    final Method parseFrom = ClassUtils.getStaticMethod(parameter.getParameterType(), "parseFrom", ByteString.class);
                    return ReflectionUtils.invokeMethod(parseFrom, null, any.getValue());
                }
            }

            return null;
        }

        @Override
        public Object resolveArgument(MethodParameter parameter, Message<?> message) throws Exception {
            if (message.getPayload() instanceof Envelopes.Envelope) {
                Envelopes.Envelope envelope = (Envelopes.Envelope) message.getPayload();
                return resolveArgumentEnvelope(parameter, envelope);
            }
            if (message.getPayload() instanceof byte[]) {
                byte[] value = (byte[]) message.getPayload();
                Envelopes.Envelope envelope = Envelopes.Envelope.parseFrom(value);
                return resolveArgumentEnvelope(parameter, envelope);
            }

            return null;
        }
    }

}
