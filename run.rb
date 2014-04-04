require 'java'

java_import 'com.stocksimulator.main.RubyBSAdapter'
java_import 'com.stocksimulator.main.RBSFactory'
java_import 'com.stocksimulator.abs.Parameters'
java_import 'com.stocksimulator.common_strategies.RubyRatioAdapter'

class Variables
	
	def self.symbolA 
		"PETR4$16"
	end

	def self.symbolB
		"INDc1"
	end
end
class Start < RBSFactory


	def self.setVar()
		papers = [Variables.symbolB, Variables.symbolA] #INDc1
		self.setOutputName("mini322".to_java)
		self.setMongoOutputSymbol(Variables.symbolA)
		self.setLog(true)
		cleanSymbols()
		for paper in papers
			pushSymbol(paper)
		end

		self.setDelay(100)
	end
	
	def self.run()
		#
		#dates = ["21/01/2014", "22/01/2014", "23/01/2014", "24/01/2014", "27/01/2014", "28/01/2014","29/01/2014", "30/01/2014","06/02/2014", "05/02/2014", "27/02/2014", "04/02/2014", "26/02/2014", "20/02/2014", "13/02/2014", "10/02/2014", "31/01/2014", "21/02/2014","19/02/2014", "18/02/2014", "03/02/2014",  "07/02/2014",  "11/02/2014", "12/02/2014",  "14/02/2014", "25/02/2014", "06/03/2014", "07/03/2014", "10/03/2014"]
		#puts dates
		#dates = ["21/02/2014","19/02/2014", "20/02/2014", "18/02/2014"]
		#dates = ["11/02/2014"]
		dates = ["17/03/2014", "18/03/2014", "19/03/2014"]
		self.setVar()

		ret = []
		for date in dates
			alloc = getFile(date)
			puts alloc
			ret.push(RubyConf.new(alloc, date))
		end
		ret.to_java RubyBSAdapter
	end

end

class RubyConfFactory
	def self.getNew(filename, date)
		RubyConf.new(filename, date)
	end
end

class RubyConf < RubyBSAdapter
	attr_reader :from, :to, :name,  :bookOrder, :actorsQtd, :strategyType, :watchSymbol, :rbFilename, :rbKlass, :replace

	def to_int(a)
		a.to_java(:int)
	end

	def postRun()

	end
	def initialize(filename, date)
		@from = "09:10:00" #15:10
		@to = "17:40:00" #15:25
		@name = "fireProofR"
		@dbLookUp = "fireProof2" #dbLookupName
		@dateRB = date
		@bookOrder = to_int(30) #Fila
		@actorsQtd = to_int(4)
		@replace = false
		@strategyType = "TestStrategy"
		@watchSymbol = [Variables.symbolB]
		@rbFilename = "self"
		@rbKlass = "RubyStrategy"  
	end
	
	def mConfig
		mongoConfigMaker("localhost", 27017, @dbLookUp+@dateRB, myFilename)
	end 

	def myInst
		instMaker([Variables.symbolB, Variables.symbolA])
	end

	def varParam()
		params = []
		hardLimit_range = (5..10).step(5) #50..500 #50..70
		spread_range = (20..40).step(5)
		for hardLimit in hardLimit_range

			for spread in spread_range
				a = Parameters.new
				a.set("spread", to_int(spread))
				a.set("hardLimit", to_int(hardLimit))
				params.push(a)

			end	
		end
		params
	end
end

#class RubyStrategy < RubyTestAdapter

#end