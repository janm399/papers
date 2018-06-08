package com.acme;

import java.util.Arrays;

interface JReportGenerator {

    byte[] generate(final String user);

}

class JReportGeneratorImpl implements JReportGenerator {
    @Override
    public byte[] generate(String user) {
        return new byte[0];
    }
}

class JReportService {
    private final JReportGenerator reportGenerator;

    public JReportService(JReportGenerator reportGenerator) {
        this.reportGenerator = reportGenerator;
    }

    void reportAll() {
        for (String user : Arrays.asList("a", "b", "c")) {
            reportGenerator.generate(user);
        }
    }
}

class JMain {
    public static void main(String[] args) {

    }
}