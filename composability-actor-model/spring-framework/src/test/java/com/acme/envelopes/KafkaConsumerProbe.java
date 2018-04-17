package com.acme.envelopes;

import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.kafka.support.SendResult;
import org.springframework.stereotype.Component;
import org.springframework.util.concurrent.ListenableFuture;
import org.springframework.util.concurrent.ListenableFutureCallback;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.stream.Collectors;

/**
 * A probe that observes receipt of messages in the {{@link org.springframework.kafka.annotation.KafkaListener}}
 * annotated methods in the components that make up this application. Typical usage is in a test where one wants
 * to assert that a particular {{@link ConsumerRecord}} is received.
 *
 * <pre class="code">
 * &#064;RunWith(SpringRunner.class)
 * &#064;SpringBootTest
 * &#064;EnableAspectJAutoProxy
 * public class KafkaApplicationTests {
 *     &#064;Autowired
 *     private KafkaTemplate&lt;byte[], byte[]&gt; kafkaTemplate;
 *     &#064;Autowired
 *     private KafkaConsumerProbe kafkaListenerProbe;
 *
 *     &#064;Test
 *     public void messagesPublishedAndConsumed() {
 *         ConsumerRecordsProbe probe = this.kafkaListenerProbe.consumerRecords();
 *
 *         byte[] data = {1, 2, 3};
 *         this.kafkaTemplate.send("topic-v1", data);
 *         ConsumerRecord&lt;?, ?&gt; received = probe.nextConsumerRecord();
 *
 *         // Assert something about `received` here
 *         System.out.println(received);
 *     }
 * }
 * </pre>
 */
interface KafkaConsumerProbe {

    /**
     * Returns a tracking probe for {{@link org.apache.kafka.clients.consumer.Consumer}} with any K, V
     *
     * @return the tracking probe that can be used to assert the receipt of records
     */
    ConsumerRecordsProbe consumerRecords();

}

// TODO: Similar to the KafkaConsumerProbe
interface KafkaProducerProbe {

    // TODO: Similar to the KafkaConsumerProbe
    ProducerRecordsProbe producerRecords();

}

interface ProducerRecordsProbe {

}

/**
 * A probe that tracks the consumption of {{@link ConsumerRecord}}s by their timestamp.
 */
interface ConsumerRecordsProbe {

    /**
     * Returns the next received {{@link ConsumerRecord}} if received within {{@code timeoutMilliseconds}}.
     * @param timeoutMilliseconds the timeout in milliseconds
     * @return the next received record or {{@link NoSuchElementException}}
     * @throws NoSuchElementException if no records arrive within {{@code timeoutMilliseconds}}
     */
    ConsumerRecord<?, ?> nextConsumerRecord(long timeoutMilliseconds);

    /**
     * Returns the next received <i>non-empty</i> batch of {{@link ConsumerRecord}} if received within {{@code timeoutMilliseconds}}.
     * @param timeoutMilliseconds the timeout in milliseconds
     * @return the next received records or {{@link NoSuchElementException}}
     * @throws NoSuchElementException if no records arrive within {{@code timeoutMilliseconds}}
     */
    List<ConsumerRecord<?, ?>> nextConsumerRecords(long timeoutMilliseconds);

    default List<ConsumerRecord<?, ?>> nextConsumerRecords() {
        return nextConsumerRecords(10000);
    }

    default ConsumerRecord<?, ?> nextConsumerRecord() {
        return nextConsumerRecord(10000);
    }

}

@Aspect
@Component
class KafkaProbeAspect implements KafkaConsumerProbe, KafkaProducerProbe {
    private final BlockingQueue<ConsumerRecord<?, ?>> pollingQueue = new LinkedBlockingQueue<>();
    private final List<ConsumerRecord<?, ?>> allConsumerRecords = new ArrayList<>();

    @Before(value = "@annotation(org.springframework.kafka.annotation.KafkaListener) && args(consumerRecord)")
    private void recordConsumed(ConsumerRecord<?, ?> consumerRecord) {
        this.pollingQueue.add(consumerRecord);
        this.pollingQueue.drainTo(this.allConsumerRecords);
    }

    @AfterReturning(value = "execution(* org.springframework.kafka.core.KafkaTemplate.send(..))", returning = "retval")
    private void recordProduced(ListenableFuture<?> retval) {
        // we know that x is ListenableFuture<SendResult<?, ?>>, but the pointcut expression fails to
        // resolve when we say `ListenableFuture<SendResult<?, ?>> retval` as the advice parameter
        //
        // TODO: Complete me
        //
        // noinspection unchecked
        ((ListenableFuture<SendResult<?, ?>>) retval).addCallback(new ListenableFutureCallback<SendResult<?, ?>>() {
            @Override
            public void onFailure(Throwable throwable) {
                System.out.println("******* " + throwable);
            }

            @Override
            public void onSuccess(SendResult<?, ?> sendResult) {
                System.out.println("********* Yay " + sendResult.getProducerRecord());
            }
        });
    }

    @Override
    public ConsumerRecordsProbe consumerRecords() {
        return new ConsumerRecordsProbeImpl();
    }

    @Override
    public ProducerRecordsProbe producerRecords() {
        throw new RuntimeException("Not implemented yet");
    }

    class ConsumerRecordsProbeImpl implements ConsumerRecordsProbe {
        private long lastTimestamp = 0L;

        @Override
        public ConsumerRecord<?, ?> nextConsumerRecord(long timeoutMilliseconds) {
            List<ConsumerRecord<?, ?>> crs = neCrs(timeoutMilliseconds);

            // neCrs returns non-empty list
            return crs.get(0);
        }

        @Override
        public List<ConsumerRecord<?, ?>> nextConsumerRecords(long timeoutMilliseconds) {
            return neCrs(timeoutMilliseconds);
        }

        private List<ConsumerRecord<?, ?>> neCrs(long timeoutMilliseconds) {
            long start = System.currentTimeMillis();
            while (System.currentTimeMillis() - start < timeoutMilliseconds) {
                List<ConsumerRecord<?, ?>> crs = crs();
                if (!crs.isEmpty()) return crs;

                // Ugh, but it'll do for now...
                try {
                    Thread.sleep(100L);
                } catch (InterruptedException ignored) {

                }
            }

            throw new NoSuchElementException("No new ConsumerRecords.");
        }

        private List<ConsumerRecord<?, ?>> crs() {
            List<ConsumerRecord<?, ?>> crs = allConsumerRecords.stream().filter(consumerRecord -> consumerRecord.timestamp() > lastTimestamp).collect(Collectors.toList());
            if (crs.isEmpty()) return crs;

            ConsumerRecord<?, ?> last = crs.get(0);
            this.lastTimestamp = last.timestamp();
            return crs;
        }

    }

}
