package com.stocksimulator.remote

import akka.actor.Actor

class JobSender extends Actor {
  def receive = {
    case MasterJobSender(actor, j) =>
      println("Sending job..")
      actor ! j
    case JobAck =>
      println("Job sent..")
      context.system.shutdown
  }
}