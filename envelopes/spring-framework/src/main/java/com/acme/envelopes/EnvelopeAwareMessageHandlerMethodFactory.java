package com.acme.envelopes;

import com.google.protobuf.Any;
import com.google.protobuf.ByteString;
import com.google.protobuf.GeneratedMessage;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.core.MethodParameter;
import org.springframework.format.support.DefaultFormattingConversionService;
import org.springframework.kafka.support.KafkaNull;
import org.springframework.messaging.Message;
import org.springframework.messaging.converter.GenericMessageConverter;
import org.springframework.messaging.handler.annotation.support.*;
import org.springframework.messaging.handler.invocation.HandlerMethodArgumentResolver;
import org.springframework.util.ClassUtils;
import org.springframework.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

public class EnvelopeAwareMessageHandlerMethodFactory extends DefaultMessageHandlerMethodFactory
        implements MessageHandlerMethodFactory, BeanFactoryAware, InitializingBean {
    private ConfigurableBeanFactory beanFactory;

    @Override
    public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
        super.setBeanFactory(beanFactory);
        this.beanFactory = (ConfigurableBeanFactory)beanFactory;
    }

    @Override
    public void afterPropertiesSet() {
        super.afterPropertiesSet();

        setBeanFactory(beanFactory);

        DefaultFormattingConversionService conversionService = new DefaultFormattingConversionService();
        setConversionService(conversionService);

        List<HandlerMethodArgumentResolver> argumentResolvers = new ArrayList<>();

        // Annotation-based argument resolution
        argumentResolvers.add(new EnvelopePayloadMethodArgumentResolver());

        argumentResolvers.add(new HeaderMethodArgumentResolver(conversionService, beanFactory));
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
        setArgumentResolvers(argumentResolvers);
    }

    static class EnvelopePayloadMethodArgumentResolver implements HandlerMethodArgumentResolver {

        private <A extends Annotation, P> boolean isParameter(MethodParameter parameter, Class<A> annotationType, Class<P> parameterType) {
            final A annotation = parameter.getParameterAnnotation(annotationType);
            return annotation != null && parameterType.isAssignableFrom(parameter.getParameterType());
        }

        @Override
        public boolean supportsParameter(MethodParameter parameter) {
            return isParameter(parameter, Envelope.Payload.class, GeneratedMessage.class) ||
                   isParameter(parameter, Envelope.Payload.class, Any.class) ||
                   isParameter(parameter, Envelope.Token.class, String.class) ||
                   isParameter(parameter, Envelope.CorrelationId.class, String.class);
        }

        @Override
        public Object resolveArgument(MethodParameter parameter, Message<?> message) throws Exception {
            if (message.getPayload() instanceof Envelopes.Envelope) {
                Envelopes.Envelope envelope = (Envelopes.Envelope) message.getPayload();
                if (isParameter(parameter, Envelope.CorrelationId.class, String.class)) return envelope.getCorrelationId();
                if (isParameter(parameter, Envelope.Token.class, String.class)) return envelope.getToken();
                if (isParameter(parameter, Envelope.Payload.class, Any.class)) return envelope.getPayload();
                if (isParameter(parameter, Envelope.Payload.class, GeneratedMessage.class)) {
                    final Any any = envelope.getPayload();
                    final String parameterTypeUrl = "type.googleapis.com/" + parameter.getParameterType().getCanonicalName();
                    if (any.getTypeUrl().equals(parameterTypeUrl)) {
                        final Method parseFrom = ClassUtils.getStaticMethod(parameter.getParameterType(), "parseFrom", ByteString.class);
                        return ReflectionUtils.invokeMethod(parseFrom, null, any.getValue());
                    }
                }

                return null;
            }
            if (message.getPayload() instanceof ConsumerRecord) {
                ConsumerRecord cr = (ConsumerRecord) message.getPayload();
                if (cr.value() instanceof byte[]) {
                    byte[] value = (byte[]) cr.value();
                    return Envelopes.Envelope.parseFrom(value);
                }
            }

            return null;
        }
    }

}
