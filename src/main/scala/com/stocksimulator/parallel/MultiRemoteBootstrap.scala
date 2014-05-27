package com.stocksimulator.parallel

class MultiRemoteBootstrap  {
/*  val workers = new Workers(conf.localWorkers, createBundle, conf.name)

  def run() = {
    this.log("Run started..")
    val uniqueParams = params.toArray.distinct

    this.log("Job count after filter: " + uniqueParams.length)
    val results = uniqueParams.map {
      params =>
        params.set("_date", date)
        val strategy = createBundle(params)
        (params, strategy.init) // Block current actor flow.
      // p => workers.master ! spWork(p, date)
    }
    //workers.master ! spLast
    //workers.system.awaitTermination()
    this.log("Local worker system terminated...")
    //result.parametersResult
    results
  }*/


}