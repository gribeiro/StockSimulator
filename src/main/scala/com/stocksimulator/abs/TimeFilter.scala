package com.stocksimulator.abs

import org.joda.time.DateTime

trait TimeFilter[T] {
def timeFiltering(from:DateTime, to:DateTime):T
}