package com.acme

import org.nd4j.jita.conf.CudaEnvironment

object NDInitializer {

  def init(): Unit = {
    CudaEnvironment.getInstance().getConfiguration
      .setMaximumDeviceCacheableLength(1024 * 1024 * 1024L)
      .setMaximumDeviceCache(6L * 1024 * 1024 * 1024L)
      .setMaximumHostCacheableLength(1024 * 1024 * 1024L)
      .setMaximumHostCache(6L * 1024 * 1024 * 1024L)
  }

}
