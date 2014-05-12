package com.stocksimulator.main
import com.stocksimulator.main.ConfigurationModule.MongoConfig

object CurrentMongoConfig extends MongoConfig {
 val host = "192.168.90.15"
 val port = 27017
 val coll = "stockSimulator"
}