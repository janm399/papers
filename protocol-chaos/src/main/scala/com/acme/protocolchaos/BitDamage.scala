package com.acme.protocolchaos

import java.util
import java.util.concurrent.atomic.AtomicLong
import java.util.zip.CRC32

import com.trueaccord.scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

import scala.collection.immutable.Range
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

object BitDamage {

  val knownResults: List[(Array[Byte], Array[Byte])] =
    List(
      (Array[Byte](8, -32, -80, -120, -106, 2, 18, -56, 20, 79, 106, 86, 38, 110, 44, 101, 86, 72, 54, 71, 35, 36, 44, 115, 66, 48, 110, 107, 46, 45, 75, 47, 59, 55, 75, 115, 61, 96, 38, 81, 66, 34, 116, 43, 54, 117, 51, 94, 123, 109, 94, 77, 71, 71, 101, 44, 126, 90, 74, 116, 35, 93, 97, 102, 124, 35, 34, 88, 121, 111, 48, 68, 97, 82, 92, 60, 80, 97, 38, 67, 37, 35, 54, 82, 78, 41, 116, 92, 63, 85, 68, 112, 75, 68, 80, 105, 54, 33, 100, 58, 92, 59, 125, 94, 121, 103, 109, 102, 105, 105, 69, 108, 83, 113, 72, 115, 66, 90, 105, 117, 81, 89, 47, 109, 79, 78, 33, 67, 92, 112, 83, 62, 48, 61, 47, 75, 51, 80, 75, 83, 118, 80, 106, 60, 44, 123, 101, 55, 47, 38, 109, 83, 89, 115, 45, 56, 104, 71, 39, 46, 49, 59, 78, 58, 85, 50, 85, 123, 101, 104, 105, 124, 60, 105, 64, 43, 56, 57, 102, 48, 120, 66, 82, 85, 34, 61, 103, 117, 101, 65, 53, 49, 80, 112, 86, 112, 74, 67, 120, 81, 51, 75, 52, 82, 49, 111, 95, 79, 95, 39, 79, 76, 52, 36, 73, 71, 77, 78, 47, 111, 42, 68, 57, 79, 53, 61, 57, 95, 97, 97, 41, 99, 80, 91, 102, 94, 54, 88, 103, 65, 34, 41, 83, 104, 71, 88, 47, 62, 114, 67, 75, 102, 84, 101, 86, 61, 114, 87, 39, 68, 125, 51, 90, 100, 119, 42, 111, 92, 39, 74, 78, 64, 97, 42, 112, 52, 72, 55, 41, 73, 84, 79, 74, 70, 33, 120, 40, 111, 94, 102, 114, 48, 104, 35, 63, 107, 67, 66, 37, 60, 81, 47, 89, 107, 115, 36, 104, 35, 106, 57, 33, 37, 88, 125, 113, 76, 41, 126, 111, 119, 84, 65, 93, 63, 85, 98, 109, 121, 65, 68, 81, 95, 103, 37, 97, 36, 88, 84, 68, 114, 53, 117, 81, 58, 119, 81, 122, 59, 49, 44, 69, 91, 35, 113, 121, 121, 108, 83, 117, 87, 42, 108, 56, 120, 57, 70, 65, 48, 38, 50, 44, 104, 79, 96, 107, 65, 84, 59, 62, 39, 72, 118, 72, 63, 37, 98, 113, 58, 124, 57, 71, 46, 109, 38, 36, 70, 69, 36, 115, 114, 57, 105, 122, 93, 57, 33, 104, 59, 33, 44, 96, 76, 58, 105, 45, 118, 47, 66, 36, 117, 105, 118, 90, 67, 112, 68, 80, 116, 124, 87, 110, 80, 66, 126, 115, 46, 118, 122, 120, 44, 84, 90, 119, 72, 66, 95, 97, 59, 122, 50, 52, 33, 65, 60, 118, 59, 93, 114, 75, 53, 64, 44, 105, 45, 111, 111, 113, 100, 71, 107, 123, 95, 75, 56, 77, 65, 74, 70, 48, 86, 50, 56, 50, 92, 112, 78, 51, 34, 72, 43, 101, 60, 85, 93, 66, 66, 73, 75, 110, 102, 43, 90, 105, 116, 95, 105, 86, 101, 34, 126, 82, 83, 92, 97, 85, 62, 96, 34, 34, 49, 98, 69, 79, 80, 124, 80, 58, 124, 93, 55, 70, 66, 64, 107, 71, 122, 83, 36, 39, 89, 34, 47, 65, 45, 56, 88, 113, 120, 62, 107, 78, 65, 75, 123, 93, 100, 102, 113, 126, 63, 33, 68, 87, 46, 116, 119, 74, 62, 121, 66, 122, 93, 59, 33, 95, 53, 111, 36, 112, 101, 84, 47, 89, 113, 40, 99, 115, 80, 79, 66, 65, 90, 43, 85, 38, 70, 125, 46, 76, 47, 46, 94, 95, 94, 85, 71, 109, 112, 104, 49, 107, 83, 59, 40, 49, 102, 121, 77, 52, 122, 67, 81, 44, 51, 48, 101, 96, 79, 72, 44, 113, 37, 110, 124, 118, 64, 86, 123, 58, 42, 113, 109, 106, 109, 89, 114, 90, 98, 47, 48, 93, 51, 58, 120, 92, 96, 114, 101, 102, 75, 56, 122, 45, 113, 85, 83, 103, 59, 110, 72, 58, 122, 64, 58, 55, 124, 70, 51, 93, 45, 63, 119, 115, 111, 125, 77, 84, 65, 74, 122, 100, 89, 120, 49, 91, 122, 86, 53, 41, 48, 90, 106, 73, 104, 57, 77, 100, 85, 38, 49, 112, 85, 89, 117, 100, 66, 66, 46, 104, 74, 80, 117, 63, 38, 39, 53, 91, 51, 71, 33, 70, 126, 49, 112, 122, 65, 49, 49, 117, 104, 65, 39, 52, 42, 101, 72, 81, 92, 43, 66, 72, 106, 50, 33, 89, 58, 34, 51, 33, 76, 50, 114, 118, 44, 120, 54, 76, 57, 84, 60, 119, 117, 116, 79, 66, 123, 34, 93, 106, 77, 111, 124, 104, 109, 99, 110, 51, 115, 81, 89, 110, 72, 54, 101, 91, 61, 93, 83, 39, 45, 119, 81, 33, 39, 38, 91, 66, 43, 36, 85, 110, 91, 104, 87, 48, 50, 61, 60, 113, 78, 98, 39, 39, 64, 85, 38, 106, 44, 85, 48, 107, 44, 35, 69, 123, 37, 98, 85, 49, 60, 105, 121, 79, 73, 63, 110, 83, 50, 83, 47, 76, 88, 62, 50, 73, 50, 64, 54, 117, 101, 55, 38, 89, 55, 82, 90, 108, 89, 50, 74, 88, 112, 81, 44, 103, 102, 62, 97, 39, 68, 39, 57, 44, 51, 50, 47, 113, 119, 106, 68, 50, 121, 101, 100, 35, 117, 43, 65, 45, 62, 46, 120, 87, 53, 70, 53, 56, 90, 64, 77, 119, 56, 113, 110, 104, 74, 109, 51, 80, 79, 71, 46, 96, 100, 112, 74, 72, 108, 92, 122, 40, 73, 121, 100, 90, 92, 51, 34, 44, 98, 112, 120, 47, 103, 79, 47, 95, 45, 104, 69, 59, 99, 82, 91, 33, 109, 81, 47, 126, 120, 77, 104, 69, 55, 116, 66, 95, 125, 84, 96, 74, 86, 88, 79, 103, 113, 52, 90, 120, 76, 120, 35, 116, 37, 126, 118, 115, 67, 123, 114, 65, 117, 118, 65, 103, 105, 35, 109, 66, 107, 65, 70, 45, 69, 39, 101, 46, 124, 122, 33, 33, 44, 111, 92, 91, 73, 36, 105, 40, 42, 57, 113, 39, 70, 59, 105, 122, 47, 88, 125, 120, 53, 60, 74, 50, 79, 41, 33, 121, 74, 113, 66, 55, 55, 95, 90, 36, 33, 100, 60, 70, 107, 124, 104, 115, 113, 108, 79, 108, 76, 72, 77, 41, 43, 35, 35, 40, 118, 122, 44, 107, 123, 78, 75, 46, 36, 86, 47, 45, 118, 60, 80, 88, 89, 114, 93, 98, 79, 108, 105, 83, 46, 94, 66, 111, 114, 65, 95, 40, 71, 82, 62, 75, 96, 120, 53, 112, 111, 49, 44, 87, 81, 80, 100, 116, 40, 112, 86, 116, 65, 111, 77, 37, 83, 66, 112, 79, 47, 104, 48, 79, 34, 91, 119, 97, 49, 58, 46, 117, 112, 76, 94, 124, 109, 120, 119, 35, 61, 113, 92, 74, 50, 40, 109, 70, 35, 93, 46, 119, 119, 41, 100, 85, 41, 109, 54, 112, 33, 91, 73, 46, 57, 76, 33, 71, 64, 56, 121, 35, 105, 87, 43, 59, 97, 50, 124, 69, 57, 42, 97, 120, 122, 42, 58, 55, 100, 74, 109, 122, 78, 75, 88, 76, 111, 86, 35, 61, 110, 63, 45, 83, 108, 62, 41, 121, 116, 62, 115, 61, 113, 99, 126, 99, 119, 95, 45, 34, 70, 65, 89, 85, 63, 123, 59, 46, 86, 110, 69, 79, 111, 47, 123, 50, 77, 46, 67, 39, 97, 81, 60, 95, 33, 33, 43, 34, 101, 102, 74, 84, 109, 55, 115, 84, 65, 109, 56, 50, 91, 56, 43, 94, 41, 119, 117, 64, 81, 90, 89, 101, 96, 45, 44, 112, 33, 33, 100, 69, 44, 66, 44, 96, 91, 119, 42, 70, 56, 57, 105, 77, 57, 65, 104, 121, 99, 44, 115, 61, 93, 78, 102, 54, 79, 83, 38, 85, 79, 41, 82, 99, 54, 124, 101, 71, 48, 37, 85, 118, 124, 56, 100, 58, 64, 49, 81, 115, 68, 64, 106, 124, 68, 94, 34, 72, 83, 55, 65, 124, 100, 49, 90, 51, 121, 107, 59, 86, 34, 87, 109, 76, 124, 48, 43, 86, 52, 60, 45, 123, 60, 35, 125, 40, 104, 97, 47, 59, 41, 124, 73, 115, 114, 67, 56, 78, 66, 75, 122, 69, 91, 86, 115, 76, 113, 89, 54, 72, 84, 58, 93, 41, 90, 99, 37, 59, 49, 74, 40, 120, 94, 86, 53, 36, 79, 124, 73, 107, 93, 70, 59, 66, 62, 53, 73, 52, 112, 57, 47, 93, 105, 48, 73, 117, 35, 123, 33, 40, 64, 96, 45, 79, 48, 59, 78, 110, 85, 82, 117, 107, 55, 52, 105, 98, 51, 97, 113, 42, 38, 126, 74, 50, 89, 87, 96, 91, 35, 105, 55, 97, 113, 109, 66, 48, 78, 44, 76, 82, 117, 80, 57, 125, 50, 64, 61, 75, 45, 122, 121, 119, 94, 83, 74, 46, 58, 116, 69, 60, 35, 88, 101, 42, 103, 89, 81, 49, 37, 94, 113, 57, 40, 123, 91, 114, 34, 33, 77, 94, 93, 58, 33, 83, 104, 124, 85, 103, 44, 47, 102, 68, 45, 34, 39, 38, 109, 85, 111, 105, 41, 76, 70, 44, 53, 82, 110, 126, 106, 99, 43, 118, 85, 98, 93, 115, 56, 100, 93, 57, 41, 77, 41, 96, 101, 42, 33, 43, 117, 123, 85, 66, 93, 41, 43, 71, 97, 73, 121, 116, 66, 86, 99, 44, 117, 121, 40, 54, 50, 65, 69, 91, 50, 71, 99, 120, 111, 35, 119, 66, 77, 69, 117, 96, 100, 126, 42, 106, 56, 39, 121, 48, 54, 96, 91, 61, 90, 92, 98, 78, 118, 42, 76, 118, 35, 34, 63, 38, 47, 61, 84, 70, 86, 120, 79, 86, 35, 75, 49, 111, 72, 117, 94, 37, 43, 77, 54, 83, 51, 119, 123, 45, 35, 104, 115, 54, 63, 35, 33, 36, 44, 83, 94, 34, 50, 64, 107, 125, 100, 36, 103, 48, 94, 39, 63, 50, 43, 45, 108, 91, 86, 71, 87, 60, 87, 120, 105, 88, 111, 118, 38, 37, 50, 60, 67, 80, 122, 46, 120, 60, 59, 84, 124, 82, 126, 57, 42, 81, 45, 41, 90, 118, 78, 116, 109, 102, 56, 35, 66, 126, 34, 64, 112, 97, 91, 86, 119, 44, 36, 65, 121, 95, 81, 92, 85, 94, 65, 56, 108, 93, 44, 44, 115, 62, 59, 109, 106, 46, 73, 75, 110, 107, 95, 63, 79, 65, 78, 88, 78, 107, 78, 69, 86, 120, 70, 92, 44, 100, 121, 77, 74, 124, 67, 34, 70, 33, 50, 60, 108, 112, 36, 97, 103, 54, 100, 69, 55, 33, 87, 94, 96, 110, 119, 34, 76, 44, 55, 118, 77, 109, 116, 98, 46, 91, 81, 114, 120, 107, 75, 58, 38, 78, 83, 90, 69, 83, 37, 54, 80, 116, 111, 64, 105, 52, 51, 98, 107, 91, 74, 106, 63, 100, 36, 68, 70, 83, 62, 59, 121, 48, 85, 52, 42, 81, 126, 79, 37, 99, 79, 64, 104, 85, 79, 55, 57, 46, 37, 116, 47, 108, 67, 66, 57, 94, 117, 80, 50, 70, 107, 67, 93, 86, 115, 101, 124, 118, 76, 39, 81, 60, 117, 90, 60, 43, 105, 68, 118, 61, 79, 83, 69, 35, 75, 34, 90, 119, 60, 97, 78, 46, 33, 42, 75, 98, 80, 70, 96, 124, 33, 90, 94, 79, 50, 110, 76, 108, 125, 112, 95, 62, 49, 63, 82, 47, 109, 61, 75, 84, 59, 80, 81, 119, 77, 92, 87, 39, 106, 61, 124, 117, 106, 33, 102, 102, 50, 103, 39, 70, 61, 93, 87, 91, 44, 75, 78, 92, 119, 88, 111, 41, 43, 67, 38, 58, 98, 69, 45, 113, 108, 126, 77, 82, 61, 114, 39, 57, 84, 122, 48, 56, 35, 72, 72, 35, 74, 58, 71, 61, 57, 84, 43, 116, 74, 115, 73, 72, 101, 104, 38, 87, 93, 57, 48, 39, 89, 42, 41, 60, 41, 52, 113, 48, 67, 93, 93, 35, 76, 41, 39, 96, 96, 119, 109, 70, 93, 72, 97, 100, 61, 64, 85, 38, 84, 90, 116, 58, 88, 62, 113, 120, 123, 33, 66, 58, 104, 76, 33, 101, 75, 60, 75, 107, 65, 107, 46, 87, 122, 38, 58, 101, 69, 121, 105, 64, 122, 71, 43, 78, 57, 38, 91, 112, 84, 70, 77, 84, 108, 67, 123, 117, 122, 83, 66, 116, 109, 113, 38, 104, 110, 89, 101, 123, 74, 38, 91, 78, 72, 117, 42, 90, 120, 50, 79, 101, 85, 60, 118, 94, 36, 38, 102, 100, 74, 80, 41, 107, 40, 80, 104, 51, 117, 58, 92, 63, 81, 47, 91, 80, 34, 46, 119, 124, 123, 96, 33, 118, 100, 125, 121, 34, 94, 87, 78, 71, 85, 102, 54, 113, 38, 76, 119, 89, 92, 94, 72, 97, 55, 63, 70, 41, 36, 50, 78, 72, 103, 93, 41, 125, 61, 92, 42, 126, 121, 74, 82, 58, 78, 34, 87, 82, 124, 37, 94, 39, 45, 115, 37, 33, 84, 39, 102, 39, 116, 63, 85, 76, 66, 72, 50, 34, 112, 114, 63, 124, 121, 63, 64, 102, 37, 72, 125, 65, 62, 100, 110, 54, 70, 98, 109, 61, 88, 86, 115, 109, 116, 77, 68, 73, 88, 74, 118, 49, 93, 114, 110, 85, 101, 124, 112, 79, 106, 75, 120, 44, 96, 70, 106, 56, 36, 84, 48, 97, 69, 90, 108, 46, 62, 115, 37, 46, 124, 90, 82, 47, 92, 110, 119, 41, 65, 113, 84, 116, 109, 71, 82, 113, 88, 55, 112, 57, 66, 89, 106, 126, 49, 50, 112, 126, 105, 88, 75, 54, 78, 99, 118, 115, 49, 66, 106, 110, 83, 57, 49, 63, 39, 83, 122, 114, 74, 84, 87, 79, 104, 65, 50, 37, 41, 103, 77, 114, 115, 116, 64, 74, 90, 95, 79, 40, 78, 41, 101, 68, 105, 89, 87, 54, 43, 89, 124, 72, 41, 115, 88, 96, 68, 87, 113, 73, 61, 35, 104, 115, 94, 36, 45, 53, 39, 52, 124, 71, 111, 98, 84, 85, 59, 40, 74, 117, 39, 74, 34, 63, 50, 69, 71, 55, 52, 78, 105, 112, 35, 82, 36, 125, 48, 88, 81, 62, 115, 35, 61, 75, 34, 118, 48, 101, 86, 46, 111, 67, 106, 87, 107, 93, 85, 50, 57, 45, 64, 51, 90, 87, 123, 71, 118, 112, 40, 45, 55, 60, 45, 62, 84, 88, 47, 97, 114, 104, 64, 63, 110, 110, 45, 44, 110, 96, 87, 42, 59, 42, 51, 76, 120, 58, 115, 52, 109, 87, 48, 112, 122, 92, 71, 84, 107, 70, 118, 62, 41, 42, 86, 43, 110, 108, 88, 123, 97, 100, 54, 75, 78, 86, 83, 77, 50, 102, 50, 119, 57, 54, 109, 90, 72, 51, 52, 80, 65, 63, 100, 114, 119, 80, 87, 106, 118, 87, 97, 74, 40, 67, 101, 71, 109, 33, 78, 76, 39, 61, 33, 118, 35, 43, 104, 115, 75, 112, 116, 40, 85, 90, 58, 111, 91, 85, 96, 51, 105, 46, 103, 100, 105, 44, 112, 82, 72, 106, 47, 123, 124, 81, 51, 37, 36, 67, 103, 111, 123, 80, 62, 118, 101, 50, 34, 113, 64, 82, 50, 108, 90, 118, 56, 58, 86, 103, 42, 47, 85, 88, 78, 115, 112, 126, 43, 108, 105, 112, 89, 99, 72, 82, 84, 59, 124, 33, 54, 68, 46, 73, 112, 38, 47, 57, 83, 109, 85, 55, 114, 121, 80, 47, 113, 103, 104, 63, 53, 88, 62, 103, 96, 70, 92, 112, 77, 72, 92, 96, 77, 47, 93, 118, 99, 58, 96, 119, 91, 108, 102, 108, 40, 114, 111, 89, 36, 71, 116, 126, 108, 74, 80, 122, 93, 90, 64, 44, 81, 36),
       Array[Byte](8, -32, -80, -120, -106, 2, 18, -56, 20, 79, 106, 86, 38, 110, 44, 101, 86, 72, 54, 71, 35, 36, 44, 115, 66, 48, 110, 107, 46, 45, 75, 47, 59, 55, 75, 115, 61, 96, 38, 81, 66, 34, 116, 43, 54, 117, 51, 94, 123, 109, 94, 77, 71, 71, 101, 44, 126, 90, 74, 116, 35, 93, 97, 102, 124, 35, 34, 88, 121, 111, 48, 68, 97, 82, 92, 60, 80, 97, 38, 67, 37, 35, 54, 82, 78, 41, 116, 92, 63, 85, 68, 112, 75, 68, 80, 105, 54, 33, 100, 58, 92, 59, 125, 94, 121, 103, 109, 102, 105, 105, 69, 108, 83, 113, 72, 115, 66, 90, 105, 117, 81, 89, 47, 109, 79, 78, 33, 67, 92, 112, 83, 62, 48, 61, 47, 75, 51, 80, 75, 83, 118, 80, 106, 60, 44, 123, 101, 55, 47, 38, 109, 83, 89, 115, 45, 56, 104, 71, 39, 46, 49, 59, 78, 58, 85, 50, 85, 123, 101, 104, 105, 124, 60, 105, 64, 43, 56, 57, 102, 48, 120, 66, 82, 85, 34, 61, 103, 117, 101, 65, 53, 49, 80, 112, 86, 112, 74, 67, 120, 81, 51, 75, 52, 82, 49, 111, 95, 79, 95, 39, 79, 76, 52, 36, 73, 71, 77, 78, 47, 111, 42, 68, 57, 79, 53, 61, 57, 95, 97, 97, 41, 99, 80, 91, 102, 94, 54, 88, 103, 65, 34, 41, 83, 104, 71, 88, 47, 62, 114, 67, 75, 102, 84, 101, 86, 61, 114, 87, 39, 68, 125, 51, 90, 100, 119, 42, 111, 92, 39, 74, 78, 64, 97, 42, 112, 52, 72, 55, 41, 73, 84, 79, 74, 70, 33, 120, 40, 111, 94, 102, 114, 48, 104, 35, 63, 107, 67, 66, 37, 60, 81, 47, 89, 107, 115, 36, 104, 35, 106, 57, 33, 37, 88, 125, 113, 76, 41, 126, 111, 119, 84, 65, 93, 63, 85, 98, 109, 121, 65, 68, 81, 95, 103, 37, 97, 36, 88, 84, 68, 114, 53, 117, 81, 58, 119, 81, 122, 59, 49, 44, 69, 91, 35, 113, 121, 121, 108, 83, 117, 87, 42, 108, 56, 120, 57, 70, 65, 48, 38, 50, 44, 104, 79, 96, 107, 65, 84, 59, 62, 39, 72, 118, 72, 63, 37, 98, 113, 58, 124, 57, 71, 46, 109, 38, 36, 70, 69, 36, 115, 114, 57, 105, 122, 93, 57, 33, 104, 59, 33, 44, 96, 76, 58, 105, 45, 118, 47, 66, 36, 117, 105, 118, 90, 67, 112, 68, 80, 116, 124, 87, 110, 80, 66, 126, 115, 46, 118, 122, 120, 44, 84, 90, 119, 72, 66, 95, 97, 59, 122, 50, 52, 33, 65, 60, 118, 59, 93, 114, 75, 53, 64, 44, 105, 45, 111, 111, 113, 100, 71, 107, 123, 95, 75, 56, 77, 65, 74, 70, 48, 86, 50, 56, 50, 92, 112, 78, 51, 34, 72, 43, 101, 60, 69, 76, 66, 66, 73, 75, 110, 102, 43, 90, 105, 116, 95, 105, 86, 101, 34, 126, 82, 83, 92, 97, 85, 62, 96, 34, 34, 49, 98, 1, 79, 80, 124, 80, 58, 124, 93, 55, 70, 66, 64, 107, 71, 122, 83, 36, 39, 89, 34, 47, 65, 45, 56, 88, 113, 120, 62, 107, 78, 65, 75, 123, 93, 100, 102, 113, 126, 63, 33, 68, 87, 46, 116, 119, 74, 62, 121, 66, 122, 93, 59, 33, 95, 53, 111, 36, 112, 101, 84, 47, 89, 113, 40, 99, 115, 80, 79, 66, 65, 90, 43, 85, 38, 70, 125, 46, 76, 47, 46, 94, 95, 94, 85, 71, 109, 112, 104, 49, 107, 83, 59, 40, 49, 102, 121, 77, 52, 122, 67, 81, 44, 51, 48, 101, 96, 79, 72, 44, 113, 37, 110, 124, 118, 64, 86, 123, 58, 42, 113, 109, 106, 109, 89, 114, 90, 98, 47, 48, 93, 51, 58, 120, 92, 96, 114, 101, 102, 75, 56, 122, 45, 113, 85, 83, 103, 59, 110, 72, 58, 122, 64, 58, 55, 124, 70, 51, 93, 45, 42, 119, 115, 111, 125, 77, 84, 65, 74, 50, 100, 89, 120, 49, 91, 122, 86, 53, 41, 48, 90, 106, 73, 104, 57, 77, 100, 85, 38, 49, 112, 85, 89, 117, 100, 66, 66, 46, 104, 74, 80, 117, 63, 38, 39, 53, 91, 51, 71, 33, 70, 126, 49, 112, 122, 65, 49, 49, 117, 104, 65, 39, 52, 42, 101, 72, 81, 92, 43, 66, 72, 106, 50, 33, 89, 58, 34, 51, 33, 76, 50, 114, 118, 44, 120, 54, 76, 57, 84, 60, 119, 117, 116, 79, 66, 123, 34, 93, 106, 77, 111, 124, 104, 109, 99, 110, 51, 115, 81, 89, 110, 72, 54, 101, 91, 61, 93, 83, 39, 45, 119, 81, 33, 39, 38, 91, 66, 43, 36, 85, 110, 91, 104, 87, 48, 50, 61, 60, 113, 78, 98, 39, 39, 64, 85, 38, 106, 44, 85, 48, 107, 44, 35, 69, 123, 37, 98, 85, 49, 60, 105, 121, 79, 73, 63, 110, 83, 50, 83, 47, 76, 88, 62, 50, 73, 50, 64, 54, 117, 101, 55, 38, 89, 55, 82, 90, 108, 89, 50, 74, 88, 112, 81, 44, 103, 102, 62, 97, 39, 68, 39, 57, 44, 51, 50, 47, 113, 119, 106, 68, 50, 121, 101, 100, 35, 117, 43, 65, 45, 62, 46, 120, 87, 53, 70, 53, 56, 90, 64, 77, 119, 56, 113, 110, 104, 74, 109, 51, 80, 79, 71, 46, 96, 100, 112, 74, 72, 108, 92, 122, 40, 73, 121, 100, 90, 92, 51, 34, 44, 98, 112, 120, 47, 103, 79, 47, 95, 45, 104, 69, 59, 99, 82, 91, 33, 109, 81, 47, 126, 120, 77, 104, 69, 55, 116, 66, 95, 125, 84, 96, 74, 86, 88, 79, 103, 113, 52, 90, 120, 76, 120, 35, 116, 37, 126, 118, 115, 67, 123, 114, 65, 117, 118, 65, 103, 105, 35, 109, 66, 107, 65, 70, 45, 69, 39, 101, 46, 124, 122, 33, 33, 44, 111, 92, 91, 73, 36, 105, 40, 42, 57, 113, 39, 70, 59, 105, 122, 47, 88, 125, 120, 53, 60, 74, 50, 79, 41, 33, 121, 74, 113, 66, 55, 55, 95, 90, 36, 33, 100, 60, 70, 107, 32, 104, 115, 113, 108, 79, 108, 76, 72, 77, 41, 43, 35, 35, 40, 118, 122, 44, 107, 123, 78, 75, 46, 36, 86, 47, 45, 118, 60, 80, 88, 89, 114, 93, 98, 79, 108, 105, 83, 46, 94, 66, 111, 114, 65, 95, 40, 71, 82, 62, 75, 96, 120, 53, 112, 111, 49, 44, 87, 81, 80, 100, 116, 40, 112, 86, 116, 65, 111, 77, 37, 83, 66, 112, 79, 47, 104, 48, 79, 34, 91, 119, 97, 49, 58, 46, 117, 112, 76, 94, 124, 109, 120, 119, 35, 61, 113, 92, 74, 50, 40, 109, 70, 35, 93, 46, 119, 119, 41, 100, 85, 41, 109, 54, 112, 33, 91, 73, 46, 57, 76, 33, 71, 64, 56, 121, 35, 105, 87, 43, 59, 97, 50, 124, 69, 57, 42, 97, 120, 122, 42, 58, 55, 100, 74, 109, 122, 78, 75, 88, 76, 111, 86, 35, 61, 110, 63, 45, 83, 108, 62, 41, 121, 116, 62, 115, 61, 113, 99, 126, 99, 119, 95, 45, 34, 70, 65, 89, 85, 63, 123, 59, 46, 86, 110, 69, 79, 111, 47, 123, 50, 77, 46, 67, 39, 97, 81, 60, 95, 33, 33, 43, 34, 101, 102, 74, 84, 109, 55, 115, 84, 65, 109, 56, 50, 91, 56, 43, 94, 41, 119, 117, 64, 81, 90, 89, 101, 96, 45, 44, 112, 33, 33, 100, 69, 44, 66, 44, 96, 91, 119, 42, 70, 56, 57, 105, 77, 57, 65, 104, 121, 99, 44, 115, 61, 93, 78, 102, 54, 79, 83, 38, 85, 79, 41, 82, 99, 54, 124, 101, 71, 48, 37, 85, 118, 124, 56, 100, 58, 64, 49, 81, 115, 68, 64, 106, 124, 68, 94, 34, 72, 83, 55, 65, 124, 100, 49, 90, 51, 121, 107, 59, 86, 34, 87, 109, 76, 124, 48, 43, 86, 52, 60, 45, 123, 60, 35, 125, 40, 104, 97, 47, 59, 41, 124, 73, 115, 114, 67, 56, 78, 66, 75, 122, 69, 91, 86, 115, 76, 113, 89, 54, 72, 84, 58, 93, 41, 90, 99, 37, 59, 49, 74, 40, 120, 94, 86, 53, 36, 79, 124, 73, 107, 93, 70, 59, 66, 62, 53, 73, 52, 112, 57, 47, 93, 105, 48, 73, 117, 35, 123, 33, 40, 64, 96, 45, 79, 48, 59, 78, 110, 85, 82, 117, 107, 55, 52, 105, 98, 51, 97, 113, 42, 38, 126, 74, 50, 89, 87, 96, 91, 35, 105, 55, 97, 113, 109, 66, 48, 78, 44, 76, 82, 117, 80, 57, 125, 50, 64, 61, 75, 45, 122, 121, 119, 94, 83, 74, 46, 58, 116, 69, 60, 35, 88, 101, 42, 103, 89, 81, 49, 37, 94, 113, 57, 40, 123, 91, 114, 34, 33, 77, 94, 93, 58, 33, 83, 104, 124, 85, 103, 44, 47, 102, 68, 45, 34, 39, 38, 109, 85, 111, 105, 41, 76, 70, 44, 53, 82, 110, 126, 106, 99, 43, 118, 85, 98, 93, 115, 56, 100, 93, 57, 41, 77, 41, 96, 101, 42, 33, 43, 117, 123, 85, 66, 93, 41, 43, 71, 97, 73, 121, 116, 66, 86, 99, 44, 117, 121, 40, 54, 50, 65, 69, 91, 50, 71, 99, 120, 111, 35, 119, 66, 77, 69, 117, 96, 100, 126, 42, 106, 56, 39, 121, 48, 54, 96, 91, 61, 90, 92, 98, 78, 118, 42, 76, 118, 35, 34, 63, 38, 47, 61, 84, 70, 86, 120, 79, 86, 35, 75, 49, 111, 72, 117, 94, 37, 43, 77, 54, 83, 51, 119, 123, 45, 35, 104, 115, 54, 63, 35, 33, 36, 44, 83, 94, 34, 50, 64, 107, 125, 100, 36, 103, 48, 94, 39, 63, 50, 43, 45, 108, 91, 86, 71, 87, 60, 87, 120, 105, 88, 111, 118, 38, 37, 50, 60, 67, 80, 122, 46, 120, 60, 59, 84, 124, 82, 126, 57, 42, 81, 45, 41, 90, 118, 78, 116, 109, 102, 56, 35, 66, 126, 34, 64, 112, 97, 91, 86, 119, 44, 36, 65, 121, 95, 81, 92, 85, 94, 65, 56, 108, 93, 44, 44, 115, 62, 59, 109, 106, 46, 73, 75, 110, 107, 95, 63, 79, 65, 78, 88, 78, 107, 78, 69, 86, 120, 70, 68, 44, 100, 121, 77, 74, 124, 67, 34, 70, 33, 50, 60, 108, 112, 36, 97, 103, 54, 100, 69, 55, 33, 87, 94, 96, 110, 119, 34, 76, 44, 55, 118, 77, 109, 116, 98, 46, 91, 81, 114, 120, 107, 75, 58, 38, 78, 83, 90, 69, 83, 37, 54, 80, 116, 111, 64, 105, 52, 51, 98, 107, 91, 74, 106, 63, 100, 36, 68, 70, 83, 62, 59, 121, 48, 85, 52, 42, 81, 126, 79, 37, 99, 79, 64, 104, 85, 79, 55, 57, 46, 37, 116, 47, 108, 67, 66, 57, 94, 117, 80, 50, 70, 107, 67, 93, 86, 115, 101, 124, 118, 76, 39, 81, 60, 117, 90, 60, 43, 105, 68, 118, 61, 79, 83, 69, 35, 75, 34, 90, 119, 60, 97, 78, 46, 33, 42, 75, 98, 80, 70, 96, 124, 33, 90, 94, 79, 50, 110, 76, 108, 125, 112, 95, 62, 49, 63, 82, 47, 109, 61, 75, 84, 59, 80, 81, 119, 77, 92, 87, 39, 106, 61, 124, 117, 106, 33, 102, 102, 50, 103, 39, 70, 61, 93, 87, 91, 44, 75, 78, 92, 119, 88, 111, 41, 43, 67, 38, 58, 98, 69, 45, 113, 108, 126, 77, 82, 61, 114, 39, 57, 84, 122, 48, 56, 35, 72, 72, 35, 74, 58, 71, 61, 57, 84, 43, 116, 74, 115, 73, 72, 101, 104, 38, 87, 93, 57, 48, 39, 89, 42, 41, 60, 41, 52, 113, 48, 67, 93, 93, 35, 76, 41, 39, 96, 96, 119, 109, 70, 93, 72, 97, 100, 61, 64, 85, 38, 84, 90, 116, 58, 88, 62, 113, 120, 123, 33, 66, 58, 104, 76, 33, 101, 75, 60, 75, 107, 65, 107, 46, 87, 122, 38, 58, 101, 69, 121, 105, 64, 122, 71, 43, 78, 57, 38, 91, 112, 84, 70, 77, 84, 108, 67, 123, 117, 122, 83, 66, 116, 109, 113, 38, 104, 110, 89, 101, 123, 74, 38, 91, 78, 72, 117, 42, 90, 120, 50, 79, 101, 85, 60, 118, 94, 36, 38, 102, 100, 74, 80, 41, 107, 40, 80, 104, 51, 117, 58, 92, 63, 81, 47, 91, 80, 34, 46, 119, 124, 123, 96, 33, 118, 100, 125, 121, 34, 94, 87, 78, 71, 85, 102, 54, 113, 38, 76, 119, 89, 92, 94, 72, 97, 55, 63, 70, 41, 36, 50, 78, 72, 103, 93, 41, 125, 61, 92, 42, 126, 121, 74, 82, 58, 78, 34, 87, 82, 124, 37, 94, 39, 45, 115, 37, 33, 84, 39, 102, 39, 116, 63, 85, 76, 66, 72, 50, 34, 112, 114, 63, 124, 121, 63, 64, 102, 33, 72, 125, 65, 62, 100, 110, 54, 70, 98, 109, 61, 88, 86, 115, 109, 116, 77, 68, 73, 88, 74, 118, 49, 93, 114, 110, 85, 101, 124, 112, 79, 106, 75, 120, 44, 96, 70, 106, 56, 36, 84, 48, 97, 69, 90, 108, 46, 62, 115, 37, 46, 124, 90, 82, 47, 92, 110, 119, 41, 65, 113, 84, 116, 109, 71, 82, 113, 88, 55, 112, 57, 66, 89, 106, 126, 49, 50, 112, 126, 105, 88, 75, 54, 78, 99, 118, 115, 49, 66, 106, 110, 83, 57, 49, 63, 39, 83, 122, 114, 74, 84, 87, 79, 104, 65, 50, 37, 41, 103, 77, 114, 115, 116, 64, 74, 90, 95, 79, 40, 78, 41, 101, 68, 105, 89, 87, 54, 43, 89, 124, 72, 41, 115, 88, 96, 68, 87, 113, 73, 61, 35, 104, 115, 94, 36, 45, 53, 39, 52, 124, 71, 111, 98, 84, 85, 59, 40, 74, 117, 39, 74, 34, 63, 50, 69, 71, 55, 52, 78, 105, 112, 35, 82, 36, 125, 48, 88, 81, 62, 115, 35, 61, 75, 34, 118, 48, 101, 86, 46, 111, 67, 106, 87, 107, 93, 85, 50, 57, 45, 64, 51, 90, 87, 123, 71, 118, 112, 40, 45, 55, 60, 45, 62, 84, 88, 47, 97, 114, 104, 64, 63, 110, 110, 45, 44, 110, 96, 87, 42, 59, 42, 51, 76, 120, 58, 115, 52, 109, 87, 48, 112, 122, 92, 71, 84, 107, 70, 118, 62, 41, 42, 86, 43, 110, 108, 88, 123, 97, 100, 54, 75, 78, 86, 83, 77, 50, 102, 50, 119, 57, 54, 109, 90, 72, 51, 52, 80, 65, 63, 100, 114, 119, 80, 87, 106, 118, 87, 97, 74, 40, 67, 101, 71, 109, 33, 78, 76, 39, 61, 33, 118, 35, 43, 104, 115, 75, 112, 116, 40, 85, 90, 58, 111, 91, 85, 96, 51, 105, 46, 103, 100, 105, 44, 112, 82, 72, 106, 47, 123, 124, 81, 51, 37, 36, 67, 103, 111, 123, 80, 62, 112, 101, 50, 34, 113, 64, 82, 50, 108, 90, 118, 56, 58, 86, 103, 42, 47, 85, 88, 78, 115, 112, 126, 43, 108, 105, 112, 89, 99, 72, 82, 84, 59, 124, 33, 54, 68, 46, 73, 112, 38, 47, 57, 83, 109, 85, 55, 114, 121, 80, 47, 113, 103, 104, 63, 53, 88, 62, 103, 96, 70, 92, 112, 77, 72, 92, 96, 77, 47, 93, 118, 99, 58, 96, 119, 91, 108, 102, 108, 40, 114, 111, 89, 36, 71, 116, 126, 108, 74, 80, 122, 93, 90, 64, 44, 81, 36))
    )

  class Run[M <: GeneratedMessage with Message[M]](mc: GeneratedMessageCompanion[M], g: ⇒ M)(implicit ctx: ExecutionContext) {
    private var result: Option[(M, Array[Byte], Array[Byte])] = None

    private def damage(bytes: Array[Byte]) = {
      var x = 0
      var damaged: Boolean = false
      while (x < 10) {
        val idx = Random.nextInt(bytes.length)
        val b = bytes(idx)
        val nb = (b & (idx % 0xff)).toByte
        if (b != nb) {
          damaged = true
          bytes(idx) = nb
        }
        x = x + 1
      }
      (bytes, damaged)
    }

    def run(): (M, Array[Byte], Array[Byte]) = {
      var continue: Boolean = true
      val totalAttempts = new AtomicLong(0)

      def oneRun(): Option[(M, Array[Byte], Array[Byte])] = {
        val crc = new CRC32
        val innerAttempts = 10000
        while (continue) {
          val x = g
          val b = x.toByteArray
          crc.reset()
          crc.update(b)
          val c = crc.getValue
          var innerCounter = 0
          while (innerCounter < innerAttempts) {
            innerCounter += 1

            val (bP, damaged) = damage(b.clone())
            if (damaged) {
              crc.reset()
              crc.update(bP)
              val cP = crc.getValue
              if (c == cP) {
                println("YAY")
                if (mc.validate(bP).isSuccess) {
                  println("YAY!!")
                  return Some(x, b, bP)
                }
              }
            }
          }
          totalAttempts.getAndAdd(innerAttempts)
        }
        None
      }


      val startMillis = System.currentTimeMillis()
      Future {
        while (continue) {
          Thread.sleep(10000)
          val l = totalAttempts.get()
          val elapsed = System.currentTimeMillis() - startMillis
          val mps = 1000 * l.toDouble / elapsed.toDouble
          println(s"Made $l attempts, $mps per second.")
        }
      }

      val runs = List.fill(4)(Future(oneRun()))
      val completed = Future.firstCompletedOf(runs)
      val Some(result) = Await.result(completed, Duration.Inf)
      continue = false
      result
    }

  }

  def main(args: Array[String]): Unit = {
    import Utils._
    knownResults.foreach { case (b, bp) ⇒
      val damagedBitCount = b.indices.foldLeft(0)((r, i) ⇒ r + b(i).bitDifference(bp(i)))
      val ranges = b.indices.flatMap(i ⇒ if (b(i) != bp(i)) Some(Range(math.max(0, i - 5), math.min(b.length, i + 5))) else None)
      val (sl, spl, ml) = ranges.foldLeft(("", "", "")) { case ((sl, spl, ml), range) ⇒
        val s = escape(b.slice(range.start, range.end))
        val sp = escape(bp.slice(range.start, range.end))
        val l = math.max(s.length, sp.length)
        val m = {
          val mt = s"      ^ ${range.start + 5}"
          mt + List.fill(mt.length - l)(" ").mkString("")
        }

        (s"$sl$s...", s"$spl$sp...", s"$ml$m")
      }
      println(sl)
      println(spl)
      println(ml)

      for (i ← b.indices) {
        val bi = b(i)
        val bpi = bp(i)
        if (bi != bpi) {
          val bis = bitString(bi)
          val bpis = bitString(bpi)
          println(s"at $i $bis $bpis")
        }
      }

      val x = X.parseFrom(b)
      val xp = X.parseFrom(bp)
      val crc = new CRC32
      crc.update(b)
      val c = crc.getValue
      crc.reset()
      crc.update(bp)
      val cp = crc.getValue
      println(damagedBitCount)
      println(x)
      println(xp)
      println(xp.greeting.length)
      println(c)
      println(cp)
    }

    import ExecutionContext.Implicits.global
    def g: X = X(count = Random.nextInt(), greeting = List.fill(Random.nextInt(4096))(Random.nextPrintableChar()).mkString(""))
    val (msg, b, bP) = new Run(X, g).run()
    val msgP = X.parseFrom(bP)
    println(msg)
    println(msgP)
    println(util.Arrays.toString(b))
    println(util.Arrays.toString(bP))
    println(escape(b))
    println(escape(bP))
  }

}
